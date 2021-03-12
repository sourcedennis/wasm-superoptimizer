{-# LANGUAGE MultiParamTypeClasses , FlexibleInstances, DeriveGeneric,
             TemplateHaskell
#-}

module Lang.Wasm.Dataflow.Liveness
  ( -- * Data Structures
    Live (..)
  , LiveVal
  , LivenessState (..)
  , LivenessLattice (..)
  , LiveGlobal
  , LiveActivation
  , LiveStack
    -- ** Lenses
  , lsGlobals, lsActivation, lsStack
    -- * Dataflow operations
  , confluence
  , transfer
    -- * Helpers
  , isLiveRoot
  , isLiveInvalid
  , terminalState
  , terminalStateUseless
  , graphLiveness
  ) where

import Melude
-- Stdlib imports
import           GHC.Generics ( Generic )
import           Data.Maybe ( fromMaybe )
-- Extra stdlib imports
import           Control.DeepSeq ( NFData )
import qualified Data.Vector as Vector
import qualified Control.Monad.State as S
import           Control.Monad.State ( StateT )
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
-- External library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
-- Local library imports
import           Lang.Wasm.Process ( FlowEdge, GraphFrozen (..), graphFlow )
import qualified Lang.Wasm.Data as WD
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( PVal (..), ValType (..), LocalIdx (..), GlobalIdx (..), InstrCtx (..), ResultType, zipValM )
-- Local imports
import           Lang.Wasm.Algebra ( MonadWasmState (..), Address (..) )
import qualified Lang.Wasm.AlgebraBwd as AB
import           Lang.Wasm.AlgebraBwd ( SimpleBwdAlgebra (..), execBwdAlgebra )
import qualified Lang.Wasm.Dataflow as D
import           Lang.Wasm.Dataflow ( FlowBwdAlgebra (..) )
import qualified Algorithm.Confluence as Conf
import qualified Algorithm.Dataflow as Dataflow

-- TEMP
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
import Algorithm.Graph as AG


data Live
  = Live -- Top
  | Dead -- Bottom
  deriving (Eq, Show, Generic)

type LiveVal = PVal Live Live Live Live

-- An /element/ in the liveness lattice of program states.
data LivenessLattice
  = LiveInvalid -- top (only occurs in type-incorrect graphs)
  | LiveOk LivenessState
  | LiveUnknown -- bottom
  deriving (Eq, Show, Generic)

data LivenessState =
  LivenessState {
    _lsGlobals    :: IdList LiveGlobal
  , _lsActivation :: LiveActivation
  , _lsStack      :: LiveStack
  }
  deriving (Eq, Show, Generic)

type LiveGlobal = WD.PGlobal Live Live Live Live
type LiveActivation = WD.PActivation Live Live Live Live
type LiveStack = WD.PStack Live Live Live Live

$(makeLenses ''LivenessState)

instance NFData Live
instance NFData LivenessLattice
instance NFData LivenessState

-- | Returns `True` iff the lattice element is the value for the graph's
-- (forward) root. The root node has an empty stack.
isLiveRoot :: LivenessLattice -> Bool
isLiveRoot (LiveOk s) = (s^.lsStack) == ([]:|[[]])
isLiveRoot _          = False

isLiveInvalid :: LivenessLattice -> Bool
isLiveInvalid LiveInvalid = True
isLiveInvalid _           = False

transfer :: InstrCtx -> FlowEdge -> ( LivenessLattice -> LivenessLattice )
transfer ctx edge (LiveOk stack)  =
  let fType = fromMaybe (error "Missing function") . Ast.itxFuncType ctx
  in
  maybe LiveInvalid LiveOk (execStateT (D.execBwdFlowAlgebra fType flowBwdAlgebra edge) stack)
transfer ctx edge LiveUnknown     = LiveUnknown
transfer ctx edge LiveInvalid     = LiveInvalid

confluence :: LivenessLattice -> LivenessLattice -> LivenessLattice
confluence _           LiveInvalid = LiveInvalid
confluence LiveInvalid _           = LiveInvalid
confluence LiveUnknown x           = x
confluence x           LiveUnknown = x
confluence (LiveOk a)  (LiveOk b)  =
  maybe LiveInvalid LiveOk (confluenceState a b)

confluenceState :: LivenessState -> LivenessState -> Maybe LivenessState
confluenceState a b =
  LivenessState
    <$> Conf.idList (WD.zipGlobalVal confluenceLiveVal) (a ^. lsGlobals) (b ^. lsGlobals)
    <*> WD.zipActivationVal confluenceLiveVal (a ^. lsActivation) (b ^. lsActivation)
    <*> Conf.listNE (Conf.list confluenceLiveVal) (a ^. lsStack) (b ^. lsStack)
  where
  confluenceLiveVal :: LiveVal -> LiveVal -> Maybe LiveVal
  confluenceLiveVal (VI32 x) (VI32 y) = return $ VI32 (confluenceLive x y)
  confluenceLiveVal (VI64 x) (VI64 y) = return $ VI64 (confluenceLive x y)
  confluenceLiveVal (VF32 x) (VF32 y) = return $ VF32 (confluenceLive x y)
  confluenceLiveVal (VF64 x) (VF64 y) = return $ VF64 (confluenceLive x y)
  confluenceLiveVal _ _ = Nothing

confluenceLive :: Live -> Live -> Live
confluenceLive Live _    = Live
confluenceLive _    Live = Live
confluenceLive Dead Dead = Dead

-- | All global variables are live in a terminal state. All local variables are
-- dead.
terminalState :: InstrCtx -> ResultType -> LivenessState
terminalState ctx resultType =
  let gs = IdList.fromList $ map (WD.global Live Live Live Live) $ Vector.toList $ Ast.itxGlobals ctx
      ps = IdList.fromList $ map (WD.val Dead Dead Dead Dead) $ Vector.toList $ Ast.itxParams ctx
      ls = IdList.fromList $ map (WD.val Dead Dead Dead Dead) $ Vector.toList $ Ast.itxLocals ctx
      s  = map (WD.val Live Live Live Live) $ reverse $ Vector.toList resultType
  in LivenessState gs (WD.PActivation ps ls) (s:|[])

-- | All global variables are live in a terminal state. All local variables are
-- dead.
terminalStateUseless :: InstrCtx -> ResultType -> LivenessState
terminalStateUseless ctx resultType =
  let gs = IdList.fromList $ map (WD.global Live Live Live Live) $ Vector.toList $ Ast.itxGlobals ctx
      ps = IdList.fromList $ map (WD.val Live Live Live Live) $ Vector.toList $ Ast.itxParams ctx
      ls = IdList.fromList $ map (WD.val Live Live Live Live) $ Vector.toList $ Ast.itxLocals ctx
      s  = map (WD.val Live Live Live Live) $ reverse $ Vector.toList resultType
  in LivenessState gs (WD.PActivation ps ls) (s:|[])

instance MonadWasmState Live Live Live Live () (StateT LivenessState Maybe) where
  popStack =
    withStateLens lsStack $ StateT WD.popStackVal

  peekStack =
    withStateLens lsStack $ getsM WD.peekStackVal

  pushStack =
    withStateLens lsStack . S.modify . WD.pushStack

  pushLabel =
    withStateLens lsStack $ S.modify $ WD.pushStackLabel []
    
  popLabel =
    withStateLens lsStack $ modifyM WD.popStackLabel

  getLocal =
    withStateLens lsActivation . getsM . WD.getLocal

  setLocal =
    withStateLens lsActivation .: modifyM .: WD.setLocal

  getGlobal =
    withStateLens lsGlobals . getsM . WD.getGlobal

  setGlobal =
    withStateLens lsGlobals .: modifyM .: WD.setGlobalForce

  getGlobals = S.gets (map WD.gVal . IdList.toList . view lsGlobals)

  getMemory = return ()
  setMemory _ = return ()

  -- These below are currenly unused by backward analysis
  storeI32   = const $ const $ return ()
  storeI64   = const $ const $ return ()
  storeF32   = const $ const $ return ()
  storeF64   = const $ const $ return ()
  store8I32  = const $ const $ return ()
  store16I32 = const $ const $ return ()
  store8I64  = const $ const $ return ()
  store16I64 = const $ const $ return ()
  store32I64 = const $ const $ return ()
  loadI32     (Address v _) = return v
  loadI64     (Address v _) = return v
  loadF32     (Address v _) = return v
  loadF64     (Address v _) = return v
  load8I32  _ (Address v _) = return v
  load16I32 _ (Address v _) = return v
  load8I64  _ (Address v _) = return v
  load16I64 _ (Address v _) = return v
  load32I64 _ (Address v _) = return v
  memGrow = const $ return Live
  memSize = return Live

topVal :: ValType -> LiveVal
topVal TI32 = VI32 Live
topVal TI64 = VI64 Live
topVal TF32 = VF32 Live
topVal TF64 = VF64 Live

flowBwdAlgebra :: Monad m
               => FlowBwdAlgebra Live Live Live Live Live m
flowBwdAlgebra =
  FlowBwdAlgebra {
    bwdInstrAlgebra = bwdAlgebra
  , bwdCallExternal = \fIdx gs args res -> return (map (topVal . Ast.valType) gs, map topVal res)
  , bwdCallIndirect = \gs args res -> return (map (topVal . Ast.valType) gs, map topVal res, Live)
  }

bwdAlgebra :: Monad m
           => SimpleBwdAlgebra Live Live Live Live Live m
bwdAlgebra =
  SimpleBwdAlgebra {
    topI32 = return Live
  , topI64 = return Live
  , topF32 = return Live
  , topF64 = return Live

  , bottomI32 = return Dead
  , bottomI64 = return Dead
  , bottomF32 = return Dead
  , bottomF64 = return Dead

  , confluenceI32 = return .: confluenceLive
  , confluenceI64 = return .: confluenceLive
  , confluenceF32 = return .: confluenceLive
  , confluenceF64 = return .: confluenceLive

  , bwdUnopI32  = \_ x -> return x
  , bwdUnopI64  = \_ x -> return x
  , bwdBinopI32 = \_ x -> return (x, x)
  , bwdBinopI64 = \_ x -> return (x, x)

  , bwdUnopF32  = \_ x -> return x
  , bwdUnopF64  = \_ x -> return x
  , bwdBinopF32 = \_ x -> return (x, x)
  , bwdBinopF64 = \_ x -> return (x, x)

  , bwdTestopI32 = \_ x -> return x
  , bwdTestopI64 = \_ x -> return x

  , bwdRelopI32 = \_ x -> return (x, x)
  , bwdRelopI64 = \_ x -> return (x, x)
  , bwdRelopF32 = \_ x -> return (x, x)
  , bwdRelopF64 = \_ x -> return (x, x)

  , bwdSelectI32 = \x -> return (x, x, x)
  , bwdSelectI64 = \x -> return (x, x, x)
  , bwdSelectF32 = \x -> return (x, x, x)
  , bwdSelectF64 = \x -> return (x, x, x)
  
  , bwdExtend1I8toI32 = \_ x -> return x
  , bwdExtend2I8toI32 = \_ x -> return (x, x)
  , bwdExtend1I8toI64 = \_ x -> return x
  , bwdExtend2I8toI64 = \_ x -> return (x, x)
  , bwdExtend4I8toI64 = \_ x -> return (x, x, x, x)

  , bwdExtractI32to1I8 = return
  , bwdExtractI32to2I8 = \(x,y) -> return (confluenceLive x y)

  , bwdExtractI64to1I8 = return
  , bwdExtractI64to2I8 = return . uncurry confluenceLive
  , bwdExtractI64to4I8 = return . foldr4 confluenceLive Dead

  , bwdReinterpretI32to4I8 = return . foldr4 confluenceLive Dead
  , bwdReinterpretI64to8I8 = return . foldr8 confluenceLive Dead
  , bwdReinterpret4I8toI32 = \x -> return (x, x, x, x)
  , bwdReinterpret8I8toI64 = \x -> return (x,x,x,x,x,x,x,x)
  , bwdReinterpretI32toF32 = return
  , bwdReinterpretI64toF64 = return
  , bwdReinterpretF32toI32 = return
  , bwdReinterpretF64toI64 = return
  
  , bwdTruncF32toI32 = const $ const return
  , bwdTruncF64toI32 = const $ const return
  , bwdTruncF32toI64 = const $ const return
  , bwdTruncF64toI64 = const $ const return

  , bwdConvertF32toF64 = return
  , bwdConvertF64toF32 = return
  , bwdConvertI32toF32 = const return
  , bwdConvertI32toF64 = const return
  , bwdConvertI64toF32 = const return
  , bwdConvertI64toF64 = const return
  }


-- # Full Application to Graph #

-- | Performs the type checking on a deterministic process graph.
graphLiveness :: MonadFail m => InstrCtx -> ResultType -> GraphFrozen a -> m (IntMap LivenessState)
graphLiveness ctx rt g =
  do
    -- sfc rt g@(rootI,_,terminalI)
    let (rootI, _, terminalI) = g
    flow <- graphFlow ctx rt g
    let graphInvFlow  = AG.invertFlow flow rootI
        graphTransfer = Dataflow.mapFlow (transfer ctx) graphInvFlow
        terminal = terminalState ctx rt
        graphOrder = AG.quasiTopologicalOrder (IntSet.fromList . map snd . graphInvFlow) (IntSet.singleton terminalI)
        -- Definitely use the ordered fix-point algorithm. It takes about 1 minute for the Z3 program.
        -- The unordered algorithm takes about 20 minutes.
        res = Dataflow.fixOrder confluence (initValBwd terminalI terminal) graphTransfer (IntSet.singleton terminalI) graphOrder
    removeMaybes <$> mapM cleanLattice res
  where
  cleanLattice :: MonadFail m => LivenessLattice -> m (Maybe LivenessState)
  cleanLattice LiveInvalid = fail "Liveness failed"
  cleanLattice (LiveOk l)  = return (Just l)
  cleanLattice LiveUnknown = return Nothing
  removeMaybes :: IntMap (Maybe a) -> IntMap a
  removeMaybes = IntMap.fromList . mapMaybe (mapSndM id) . IntMap.toList

initValBwd :: Int -> LivenessState -> ( Int -> LivenessLattice )
initValBwd terminalI s nodeI
  | terminalI == nodeI  = LiveOk s
  | otherwise           = LiveUnknown -- bottom
