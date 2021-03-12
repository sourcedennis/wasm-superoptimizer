
module Lang.Wasm.Superopt.GraphOpt where

import Melude
-- 
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
-- Stdlib imports
import           Control.Monad.State as S
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
-- External library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
import qualified Data.Vector as Vector
-- Local library imports
import Lang.Wasm.Ast ( ValType, InstrCtx, Instr, ResultType (..) )
-- Local imports
import Lang.Wasm.InstrCost ( Cost )
import Lang.Wasm.Process
  -- ( MultiGraph, MultiNode, MultiId, SingleGraphFrozen, SingleGraph, SingleId
  -- , DropKeep (..), KeptVals, Edge (..), Tree (..) )
import qualified Lang.Wasm.Ast as Ast
import Lang.Wasm.Symbolic.Configuration ( Configuration )
import Lang.Wasm.Superopt.MonadSuperopt as MS
import Lang.Wasm.Superopt.MonadSuperopt ( MonadSuperopt )
import qualified Lang.Wasm.Process as P
import qualified Lang.Wasm.Symbolic.Configuration.Propagation as PG
import qualified Lang.Wasm.Symbolic.Configuration as Cfg
import Lang.Wasm.Drive ( drive )
import qualified Lang.Wasm.Synthesis as Synth
import           Lang.Wasm.Dataflow.Liveness as L
import           Lang.Wasm.Dataflow.Liveness ( LivenessState )


data Settings =
  Settings {
    settTimeoutVerify     :: TimeMs
  , settTimeoutSynthesis  :: TimeMs
  , settTimeoutTotal      :: TimeMs
  , settSymExecBound      :: Int
    -- ^ Bound on path length during bounded symbolic execution
    -- (during verification - 300000 works well)
  , settEnvBound          :: Int
  }

superopt :: MonadSuperopt m
         => Settings
         -> InstrCtx
         -> P.GraphFrozen ()
         -> Vector ValType -- ^ params
         -> Vector ValType -- ^ locals
         -> ResultType
         -> Maybe Ast.Limits
         -> MaybeT m (P.GraphFrozen ())
superopt settings ctx g ps ls rt mem =
  do
    let cfg =
          Cfg.init
            (Vector.toList $ Ast.itxGlobals ctx)
            (Vector.toList ps)
            (Vector.toList ls)
            mem

    let (startI, nodes, terminalI) = P.unfreezeGraph g
    let envBound = settEnvBound settings
    cfgs <- PG.propagate envBound ctx rt cfg g
    ls <- L.graphLiveness ctx rt g
    (startI', s) <- runStateT (rebuildGraph startI rt) (SuperState nodes cfgs settings ctx ls)
    return $ P.freezeGraph (startI', ssGraph s, terminalI)

data Env

-- | State traced through the `State` monad during superoptimization
data SuperState =
  SuperState {
    ssGraph           :: IdList (P.GraphNode ())
  , ssConfigurations  :: IntMap (Configuration Env)
  , ssSettings        :: Settings
  , ssCtx             :: InstrCtx
  , ssLiveness        :: IntMap LivenessState
  }

rebuildGraph :: MonadSuperopt m
             => NodeId
             -> ResultType
             -> StateT SuperState (MaybeT m) NodeId
rebuildGraph i rt =
  do
    node <- lookupNode i
    case node of
      Node _ (PgeInstr x nextI) ->
        do
          nextI' <- rebuildGraph nextI rt
          cloneConf nextI nextI'
          i' <- newNode $ Node () $ PgeInstr x nextI'
          cloneConf i i'
          tryExpandAndReplace i' rt
      Node _ (PgeIf ft ifG elseG nextI) ->
        do
          let ifRt = Ast.ftResults ft
          ifG' <- rebuildGraph ifG ifRt
          cloneConf ifG ifG'
          elseG' <- rebuildGraph elseG ifRt
          cloneConf elseG elseG'
          nextI' <- rebuildGraph nextI rt
          cloneConf nextI nextI'
          i' <- newNode $ Node () $ PgeIf ft ifG' elseG' nextI'
          cloneConf i i'
          tryExpandAndReplace i' rt
      Node _ (PgeBlock ft bodyG nextI) ->
        do
          let blockRt = Ast.ftResults ft
          bodyG' <- rebuildGraph bodyG blockRt
          cloneConf bodyG bodyG'
          nextI' <- rebuildGraph nextI rt
          cloneConf nextI nextI'
          i' <- newNode $ Node () $ PgeLoop ft bodyG' nextI'
          cloneConf i i'
          tryExpandAndReplace i' rt
      Node _ (PgeLoop ft bodyG nextI) ->
        do
          let blockRt = Ast.ftResults ft
          bodyG' <- rebuildGraph bodyG blockRt
          cloneConf bodyG bodyG'
          nextI' <- rebuildGraph nextI rt
          cloneConf nextI nextI'
          i' <- newNode $ Node () $ PgeLoop ft bodyG' nextI'
          cloneConf i i'
          tryExpandAndReplace i' rt
      Node _ (PgeBr lIdx) ->
        return i
      Node _ (PgeBrIf lIdx nextI) ->
        return i
      Node _ (PgeBrTable lIdxs lIdx) ->
        return i
      Node _ (PgeCall fIdx nextI) ->
        newNode . Node () . PgeCall fIdx =<< rebuildGraph nextI rt
      Node _ (PgeCallIndirect ft nextI) ->
        newNode . Node () . PgeCallIndirect ft =<< rebuildGraph nextI rt
      NodeNaturalEnd _ -> return i
      NodeReturn _ -> return i
      NodeTrapped _ -> return i
      NodeTerminal _ -> return i
      NodeStaticError -> return i

newNode :: Monad m => P.GraphNode () -> StateT SuperState (MaybeT m) NodeId
newNode n =
  do
    s <- S.get
    let (i, g) = IdList.append n (ssGraph s)
    S.put $ s { ssGraph = g }
    return i

lookupNode :: Monad m => NodeId -> StateT SuperState (MaybeT m) (P.GraphNode ())
lookupNode i =
  do
    nodes <- S.gets ssGraph
    case IdList.lookup i nodes of
      Nothing -> fail "Missing node"
      Just n  -> return n

cloneConf :: Monad m => NodeId -> NodeId -> StateT SuperState (MaybeT m) ()
cloneConf srcI dstI =
  do
    cfgs <- S.gets ssConfigurations
    case IntMap.lookup srcI cfgs of
      Nothing -> fail "Missing node"
      Just cfg ->
        S.modify $ \s -> s { ssConfigurations = IntMap.insert dstI cfg (ssConfigurations s) }

tryExpandAndReplace :: MonadSuperopt m => NodeId -> ResultType -> StateT SuperState (MaybeT m) NodeId
tryExpandAndReplace i rt =
  do
    -- trace ("Expanding " ++ show i) $ return ()
    s <- S.get
    let stgs = ssSettings s
    finalI <- finalState i -- final reachable starting from i (TODO: Cache)
    case (IntMap.lookup i (ssConfigurations s), IntMap.lookup finalI (ssLiveness s)) of
      (Just cfg, Just ls) ->
        do
          -- trace "Q" $ return ()
          mTree <- drive (ssCtx s) rt (settEnvBound stgs) cfg (i, IdList.toVector $ ssGraph s, finalI)
          case mTree of
            Nothing -> return i -- driving failed
            Just tree ->
              do
                let verifyTime = settTimeoutVerify stgs
                let synthTime = settTimeoutSynthesis stgs
                newInstrs <- lift $ MS.timeout synthTime $ runMaybeT $ Synth.synthesize verifyTime (ssCtx s) cfg tree ls
                case newInstrs of
                  Just (t, Just xs) ->
                    lift (logMicroGenDone t (Just (0, 10))) >> insertInstrs xs
                  Just (t, Nothing) ->
                    lift (logMicroGenDone t Nothing) >> return i
                  Nothing -> return i -- TODO: Check if tree is ok
      -- (Just cfg, _) -> trace "only cfg" $ return i
      -- (_, Just ls) -> trace "only liveness" $ return i
      _ -> return i

-- isTreeOk :: 

insertInstrs :: MonadSuperopt m => [Synth.NcfInstr] -> StateT SuperState (MaybeT m) NodeId
insertInstrs []                                = newNode (NodeNaturalEnd ())
insertInstrs (Synth.NFInstrSimple instr:xs)    = newNode . Node () . PgeInstr instr =<< insertInstrs xs
insertInstrs (Synth.NFInstrCall fIdx:xs)       = newNode . Node () . PgeCall fIdx =<< insertInstrs xs
insertInstrs (Synth.NFInstrCallIndirect ft:xs) = newNode . Node () . PgeCallIndirect ft =<< insertInstrs xs

finalState :: Monad m => NodeId -> StateT SuperState (MaybeT m) NodeId
finalState i =
  do
    node <- lookupNode i
    case node of
      Node _ (PgeInstr x nextI) -> finalState nextI
      Node _ (PgeIf ft ifG elseG nextI) -> finalState nextI
      Node _ (PgeBlock ft bodyG nextI) -> finalState nextI
      Node _ (PgeLoop ft bodyG nextI) -> finalState nextI
      Node _ (PgeBr lIdx) -> return i
      Node _ (PgeBrIf lIdx nextI) ->  return i
      Node _ (PgeBrTable lIdxs lIdx) ->  return i
      Node _ (PgeCall fIdx nextI) -> finalState nextI
      Node _ (PgeCallIndirect ft nextI) -> finalState nextI
      NodeNaturalEnd _ -> return i
      NodeReturn _ -> return i
      NodeTrapped _ -> return i
      NodeTerminal _ -> return i
      NodeStaticError -> return i
