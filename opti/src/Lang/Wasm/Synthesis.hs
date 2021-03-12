{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveGeneric, TupleSections #-}

-- | Brute-force synthesis
--
-- This can generate any program, but takes (effectively) forever (/O(2^n)/).
-- It can replace (small) segments which /may/ contain side-effects.
module Lang.Wasm.Synthesis where
-- ncfToInstr
--   ( synthesize
--   , possibleInstrs
--   , RelevantInstrs (..)
--   ) where

import Melude
-- Stdlib imports
import           GHC.Generics ( Generic )
import qualified Data.Sequence as Seq
import           Data.Sequence ( Seq ((:|>)), (|>) )
import           Data.Foldable ( toList )
import qualified Data.List.NonEmpty as NE
-- Extra stdlib imports
import qualified Control.Monad.State as S
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
import           Data.Hashable ( Hashable )
import qualified Data.HashMap.Lazy as HashMap
import           Data.HashMap.Lazy ( HashMap )
-- External library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min ( MinPQueue )
-- Local library imports
import           Lang.Wasm.Ast as Ast
import           Lang.Wasm.Data as WD
import           Lang.Wasm.Solver as Sv
-- Local imports
import           Lang.Wasm.Concrete.ProgState as Con
import           Lang.Wasm.Symbolic.ProgramState as PS
import           Lang.Wasm.Symbolic.ProgramState.Exec ( execTree )
import           Lang.Wasm.Symbolics as Y
import           Lang.Wasm.Solver ( MonadSolver )
import           Lang.Wasm.Symbolic.Configuration ( Configuration )
import           Lang.Wasm.Dataflow.Liveness ( LivenessState )
import           Lang.Wasm.Process as P
import           Lang.Wasm.InstrCost ( simpleCost )
import qualified Lang.Wasm.Superopt.MonadSuperopt as SU
import Lang.Wasm.Superopt.MonadSuperopt ( MonadSuperopt (..) )

-- | Synthesizes an alternative linear program for the given /finite/ tree.
synthesize :: MonadSuperopt m
           => TimeMs
           -> InstrCtx
           -> Configuration env -- ^ Set of /input/ states for the tree
           -> P.Tree a          -- ^ Tree
           -> LivenessState     -- ^ Liveness of /output/ state of the tree
           -> MaybeT m [NcfInstr]
synthesize timeVerify ctx c t ls =
  do
    let relInstrs = relevantInstrs t (RelevantInstrs False False False False mempty mempty mempty mempty mempty False False False)

    if riHasIndirectCall relInstrs || riHasMem relInstrs || riHasBreak relInstrs then
      MaybeT $ return Nothing
    else
      do
        let (initState, (sym, constraint)) = PS.fromConfiguration c
        -- The tree is already finite. no need to bound it. TODO: remove from `execTree`
        let bound = 9999999
        Just (Just specFinalState, sym') <- runMaybeT $ Y.runExtSymbolicsStateT (execTree bound t True initState constraint) sym
        synthesize' timeVerify ctx (sym', constraint) initState specFinalState ls relInstrs

data RelevantInstrs =
  RelevantInstrs {
    riHasInt32         :: Bool
  , riHasInt64         :: Bool
  , riHasFloat32       :: Bool
  , riHasFloat64       :: Bool
  , riReadGlobals      :: IntSet
  , riReadLocals       :: IntSet
  , riWriteGlobals     :: IntSet
  , riWriteLocals      :: IntSet
  , riFuncs            :: IntSet
    -- Ignore these for now. Way too expensive
  , riHasIndirectCall  :: Bool
  , riHasMem           :: Bool
  , riHasBreak         :: Bool
  }

-- |
--
-- WARNING: The tree must be /finite/
relevantInstrs :: P.Tree a -> RelevantInstrs -> RelevantInstrs
relevantInstrs (Node _ (PteInstr (SConstI32 _) n)) = relevantInstrs n . markRelevantI32
relevantInstrs (Node _ (PteInstr (SConstI64 _) n)) = relevantInstrs n . markRelevantI64
relevantInstrs (Node _ (PteInstr (SConstF32 _) n)) = relevantInstrs n . markRelevantF32
relevantInstrs (Node _ (PteInstr (SConstF64 _) n)) = relevantInstrs n . markRelevantF64
relevantInstrs (Node _ (PteInstr (SUnopI32 _) n))  = relevantInstrs n . markRelevantI32
relevantInstrs (Node _ (PteInstr (SUnopI64 _) n))  = relevantInstrs n . markRelevantI64
relevantInstrs (Node _ (PteInstr (SBinopI32 _) n)) = relevantInstrs n . markRelevantI32
relevantInstrs (Node _ (PteInstr (SBinopI64 _) n)) = relevantInstrs n . markRelevantI64
relevantInstrs (Node _ (PteInstr (SUnopF32 _) n))  = relevantInstrs n . markRelevantF32
relevantInstrs (Node _ (PteInstr (SUnopF64 _) n))  = relevantInstrs n . markRelevantF64
relevantInstrs (Node _ (PteInstr (SBinopF32 _) n)) = relevantInstrs n . markRelevantF32
relevantInstrs (Node _ (PteInstr (SBinopF64 _) n)) = relevantInstrs n . markRelevantF64
relevantInstrs (Node _ (PteInstr (STestopI32 _) n)) = relevantInstrs n . markRelevantI32
relevantInstrs (Node _ (PteInstr (STestopI64 _) n)) = relevantInstrs n . markRelevantI64
relevantInstrs (Node _ (PteInstr (SRelopI32 _) n)) = relevantInstrs n . markRelevantI32
relevantInstrs (Node _ (PteInstr (SRelopI64 _) n)) = relevantInstrs n . markRelevantI64
relevantInstrs (Node _ (PteInstr (SRelopF32 _) n)) = relevantInstrs n . markRelevantF32
relevantInstrs (Node _ (PteInstr (SRelopF64 _) n)) = relevantInstrs n . markRelevantF64
relevantInstrs (Node _ (PteInstr (SCvtop _) n)) = relevantInstrs n -- ignore these for now
relevantInstrs (Node _ (PteInstr (SPrmInstrI32 _) n)) = relevantInstrs n . markRelevantI32
relevantInstrs (Node _ (PteInstr (SPrmInstrI64 _) n)) = relevantInstrs n . markRelevantI64
relevantInstrs (Node _ (PteInstr (SPrmInstrF32 _) n)) = relevantInstrs n . markRelevantF32
relevantInstrs (Node _ (PteInstr (SPrmInstrF64 _) n)) = relevantInstrs n . markRelevantF64
relevantInstrs (Node _ (PteInstr (SVarInstr (VLocalGet (LocalIdx i))) n)) =
  relevantInstrs n . markRelevantReadLocal i
relevantInstrs (Node _ (PteInstr (SVarInstr (VLocalSet (LocalIdx i))) n)) =
  relevantInstrs n . markRelevantWriteLocal i
relevantInstrs (Node _ (PteInstr (SVarInstr (VLocalTee (LocalIdx i))) n)) =
  relevantInstrs n . markRelevantWriteLocal i
relevantInstrs (Node _ (PteInstr (SVarInstr (VGlobalGet (GlobalIdx i))) n)) =
  relevantInstrs n . markRelevantReadGlobal i
relevantInstrs (Node _ (PteInstr (SVarInstr (VGlobalSet (GlobalIdx i))) n)) =
  relevantInstrs n . markRelevantWriteGlobal i
relevantInstrs (Node _ (PteInstr (SMemInstr _) n)) = relevantInstrs n . markRelevantMem
relevantInstrs (Node _ (PteIf _ a b))           = relevantInstrs a . relevantInstrs b
relevantInstrs (Node _ (PteCall (FuncIdx i) a)) = relevantInstrs a . markRelevantFunc i
relevantInstrs (Node _ (PteCallIndirect _ a))   = relevantInstrs a
relevantInstrs (Node _ (PteBr _))               = \r -> r { riHasBreak = True }
relevantInstrs (Node _ (PteBrIf _ a))           = \r -> relevantInstrs a $ r { riHasBreak = True }
relevantInstrs (Node _ (PteBrTable _ _))        = \r -> r { riHasBreak = True }
relevantInstrs (Node _ (PteKeepDrop _ _ a))     = relevantInstrs a
relevantInstrs (NodeNaturalEnd _) = id
relevantInstrs (NodeReturn _) = id
relevantInstrs (NodeTrapped _) = id
relevantInstrs (NodeTerminal _) = id
relevantInstrs NodeStaticError = id

markRelevantMem, markRelevantI32, markRelevantI64, markRelevantF32,
  markRelevantF64 :: RelevantInstrs -> RelevantInstrs
markRelevantMem ri = ri { riHasMem = True }
markRelevantI32 ri = ri { riHasInt32 = True }
markRelevantI64 ri = ri { riHasInt64 = True }
markRelevantF32 ri = ri { riHasFloat32 = True }
markRelevantF64 ri = ri { riHasFloat64 = True }

markRelevantReadGlobal, markRelevantWriteGlobal, markRelevantReadLocal,
  markRelevantWriteLocal, markRelevantFunc :: Int -> RelevantInstrs -> RelevantInstrs
markRelevantReadGlobal  i ri = ri { riReadGlobals  = IntSet.insert i (riReadGlobals ri) }
markRelevantReadLocal   i ri = ri { riReadLocals   = IntSet.insert i (riReadLocals ri) }
markRelevantWriteGlobal i ri = ri { riWriteGlobals = IntSet.insert i (riWriteGlobals ri) }
markRelevantWriteLocal  i ri = ri { riWriteLocals  = IntSet.insert i (riWriteLocals ri) }
markRelevantFunc        i ri = ri { riFuncs        = IntSet.insert i (riFuncs ri) }


-- | Finds another program that produces the same final state when executed on
-- the same initial state.
-- 
-- This /will run forever/ or until it finds a satisfying program.
-- It is recommended to timeout this call.
synthesize' :: MonadSuperopt m
            => TimeMs
            -> InstrCtx
            -> ExtEnvironment env
            -> SymbolicProgramState env -- ^ Initial State
            -> SymbolicProgramState env -- ^ Final State
            -> LivenessState
            -> RelevantInstrs
               -- ^ Available functions. `Nothing` signifies not to use it.
               --   `Nothing`s are included to preserve function indices.
            -> MaybeT m [NcfInstr]
synthesize' timeVerify ctx env initState finalState liveness relInstrs =
  case (,) <$> (initState^.sLocalState) <*> (finalState^.sLocalState) of
    Nothing -> MaybeT $ return Nothing
    Just (lInit, lFinal) ->
      do
        let initStack  = lInit^.sStack
            finalStack = lFinal^.sStack
        -- For now, assume all globals are mutable
            globalTypes = map (GlobalType MVar . valType . WD.gVal) $ IdList.toList (initState^.sGlobals)
        -- let localTypes = map valType locals
        -- let ctx = (globalTypes, localTypes)
            initStackTypes = NE.map (map WD.valType) initStack
            finalStackTypes = NE.map (map WD.valType) finalStack

            fVerify = fmap (maybe False snd) . timeout timeVerify . verifyProgramEq timeVerify env initState finalState liveness
        search ctx fVerify relInstrs initStackTypes finalStackTypes

-- | The instructions that can be executed on the current stack state.
possibleInstrs :: InstrCtx
               -> RelevantInstrs
               -> [ValType]
               -> [NcfInstr]
possibleInstrs ctx relInstrs stack =
  map NFInstrSimple
    $ mapMaybe (\i -> Ast.simpleInstrEffect ctx i >> return i)
    $ allSimpleInstrs relInstrs

-- | Returns `True` if the programs are surely equal. As the solver is /not/
-- guaranteed to be complete, some equal programs may be marked as unequal.
verifyProgramEq :: MonadSuperopt m
                => TimeMs
                -> ExtEnvironment env
                -> SymbolicProgramState env -- ^ Initial State
                -> SymbolicProgramState env -- ^ Final State
                -> LivenessState
                -> [NcfInstr]
                -> m Bool
verifyProgramEq timeVerify (sym, constraint) init final liveness xs =
  do
    -- trace ("verifying " ++ show xs) $ return ()
    let t = instrsTree xs
    let bound = 100 -- generating a 100 instructions is totally infeasible anyway
    res <- runMaybeT $ Y.runExtSymbolicsStateT (execTree bound t True init constraint) sym
    case res of
      Just (Just final2, sym') ->
        do
          res <- areUnequal (sym', constraint) init final final2 liveness
          -- Does a counter-example exist? If not, the programs are equal
          case res of
            EvUnsat -> return True
            _       -> return False
      _ -> return False

instrsTree :: [NcfInstr] -> P.Tree ()
instrsTree [] = NodeNaturalEnd ()
instrsTree (NFInstrSimple x:xs) = Node () (PteInstr x $ instrsTree xs)
instrsTree (NFInstrCall fIdx:xs) = Node () (PteCall fIdx $ instrsTree xs)
instrsTree (NFInstrCallIndirect ft:xs) = Node () (PteCallIndirect ft $ instrsTree xs)


type SearchNodeId = Int
type Cost = Int

-- | Root node always has ID 0
data SearchState =
  SearchState {
    srchNodes         :: IdList SearchNode
  , srchFrontier      :: MinPQueue Cost SearchNodeId
  , srchRefinedNodes  :: HashMap [Con.ProgState] SearchNodeId
  }

data SearchNode =
  SearchNode {
    snNextNodes  :: HashMap NcfInstr SearchNodeId
  , snPrevNodes  :: HashMap NcfInstr SearchNodeId
  , snStack      :: NonEmpty [ValType]
  , snCost       :: Cost
    -- For some programs it is possible to store the concrete stack values
  , snTestCases  :: Maybe [Con.ProgState]
  }

search :: forall m 
       .  MonadSuperopt m
       => InstrCtx -- ^ Context
       -> ( [NcfInstr] -> m Bool ) -- Verify function
       -> RelevantInstrs -- Functions that may be called
       -> NonEmpty [ValType] -- ^ Initial Stack
       -> NonEmpty [ValType] -- ^ Final Stack
       -> MaybeT m [NcfInstr] -- ^ Instructions
search ctx fVerify relInstrs initStack finalStack =
  let n = SearchNode HashMap.empty HashMap.empty initStack 0 (Just [])
      (i, n') = IdList.singleton n
      -- cost    = heuristic initStack finalStack
      s = SearchState {
            srchNodes        = n'
          , srchFrontier     = PQ.singleton 0 i
          , srchRefinedNodes = HashMap.empty
          }
  in
  evalStateT searchS s
  where
  searchS :: StateT SearchState (MaybeT m) [NcfInstr]
  searchS =
    do
      nodeI <- popFrontier
      node <- lookupNode nodeI

      remTime <- lift timeRemaining
      failIf (remTime < 0) "Out of time"

      if eqWithLenNE (eqWithLen (==)) (snStack node) finalStack then
        do
          program <- toList <$> extractNodeInstrs nodeI
          -- trace ("Verifying: " ++ show (snCost node) ++ " " ++ show program) $ return ()
          b <- lift $ lift $ fVerify $ toList program
          -- trace ("Verification done: " ++ show b) $ return ()
          if b then
            return program
          else
            step nodeI node
      else
        step nodeI node

  step :: SearchNodeId -> SearchNode -> StateT SearchState (MaybeT m) [NcfInstr]
  step nodeI node =
    do
      -- The outgoing edges
      let instrs = possibleInstrs ctx relInstrs (NE.head $ snStack node)
      nextNodes <- catMaybes <$> mapM (\i -> fmap (i,) <$> nextNode nodeI node i) instrs
      nextNodesI <- mapM (\(i,n) -> (i,n,) <$> storeNode n) nextNodes

      let snNextNodes' =
            foldr (\(instr,_,i) -> HashMap.insert instr i) (snNextNodes node) nextNodesI
      let node' = node { snNextNodes = snNextNodes' }
      updateNode nodeI node' 

      mapM_ (\(_,n,i) -> pushFrontier (i, n)) nextNodesI
      
      searchS

  popFrontier :: StateT SearchState (MaybeT m) SearchNodeId
  popFrontier =
    do
      frontier <- S.gets srchFrontier
      case PQ.minView frontier of
        Nothing -> StateT $ const $ MaybeT $ return Nothing
        Just (v, frontier') ->
          do
            S.modify $ \s -> s { srchFrontier = frontier' }
            return v
  
  -- | Pushes the nodes to the frontier with the cost heuristic
  pushFrontier :: (SearchNodeId, SearchNode) -> StateT SearchState (MaybeT m) ()
  pushFrontier (i, n) =
    do
      let totalCost = snCost n -- + heuristic (snStack n) finalStack
      S.modify $ \s -> s { srchFrontier = PQ.insert totalCost i (srchFrontier s) }

  lookupNode :: SearchNodeId -> StateT SearchState (MaybeT m) SearchNode
  lookupNode i = liftMaybeT $ S.gets (IdList.lookup i . srchNodes)

  liftMaybeT :: StateT s (MaybeT m) (Maybe a) -> StateT s (MaybeT m) a
  liftMaybeT s =
    do
      res <- s
      case res of
        Nothing -> StateT $ const $ MaybeT $ return Nothing
        Just s' -> return s'

  nextNode :: SearchNodeId
           -> SearchNode
           -> NcfInstr
           -> StateT SearchState (MaybeT m) (Maybe SearchNode)
  nextNode prevNodeI n x@(NFInstrSimple instr) =
    let mStack =
          do
            effect <- simpleInstrEffect ctx instr
            Ast.applyEffectFull effect (snStack n)
    in
    case mStack of
      Nothing -> return Nothing
      Just stack ->
        do
          let instrCost = simpleCost instr
          let nodeCost  = snCost n + instrCost

          instrs <- (|>) <$> extractNodeInstrs prevNodeI <*> pure x

          if isStupid instrs then
            return Nothing
          else
            return $ Just $ SearchNode {
                      snNextNodes = HashMap.empty
                    , snPrevNodes = HashMap.singleton x prevNodeI
                    , snStack     = stack
                    , snCost      = nodeCost
                    , snTestCases = Nothing
                    }
  
  storeNode :: Monad m2 => SearchNode -> StateT SearchState m2 SearchNodeId
  storeNode n =
    do
      nodes <- S.gets srchNodes
      let (i, nodes') = IdList.append n nodes
      S.modify $ \s -> s { srchNodes = nodes' }
      return i
  
  updateNode :: Monad m2 => SearchNodeId -> SearchNode -> StateT SearchState m2 ()
  updateNode i n =
    do
      nodes <- S.gets srchNodes
      let nodes' = IdList.replace i n nodes
      S.modify $ \s -> s { srchNodes = nodes' }
  
  -- | Returns the sequence of instructions that leads to the node
  extractNodeInstrs :: SearchNodeId -> StateT SearchState (MaybeT m) (Seq NcfInstr)
  extractNodeInstrs nodeI =
    do
      node <- lookupNode nodeI
      prevNodes <- mapM (\(instr,i) -> (instr,i,) <$> lookupNode i) (HashMap.toList $ snPrevNodes node)
      case argMin (\(_,_,n) -> snCost n) prevNodes of
        Nothing -> return Seq.empty
        Just (bestInstr, bestNodeI, _) ->
          (|>) <$> extractNodeInstrs bestNodeI <*> pure bestInstr

-- | Non-control-flow instruction
data NcfInstr
  = NFInstrSimple SimpleInstr
  | NFInstrCall FuncIdx
  | NFInstrCallIndirect FuncType
  deriving (Show, Eq, Generic)

ncfToInstr :: NcfInstr -> Instr
ncfToInstr (NFInstrSimple s) = InstrSimple s
ncfToInstr (NFInstrCall fIdx) = InstrCall fIdx
ncfToInstr (NFInstrCallIndirect ft) = InstrCallIndirect ft

areEqualStructures :: SymbolicProgramState env
                   -> SymbolicProgramState env
                   -> Bool
areEqualStructures a b =
  -- Needless to check other things, as those remain invariant during execution
  areEqLocal (a^.sLocalState) (b^.sLocalState)
  where
  areEqLocal :: Maybe (SymbolicLocalState env) -> Maybe (SymbolicLocalState env) -> Bool
  areEqLocal (Just a) (Just b) = NE.map (map WD.valType) (a^.sStack) == NE.map (map WD.valType) (b^.sStack)


-- WARNING: This heuristic is admissible, but /not/ consistent.
-- TODO: Review this later
--
-- | Provides an /under-approximation/ of the cost for reaching the target
-- stack. This is the A* heuristic.
--
-- This relies upon the definitions in `Lang.Wasm.InstrCost`.
-- Popping any value costs at least 2. Pushing any value costs at least 2.
-- heuristic :: [ValType] -> [ValType] -> Int
-- heuristic cur dst =
--   let curLen = length cur
--       dstLen = length dst
--       numMissing = max 0 (dstLen - curLen)
--       numTooMuch = max 0 (curLen - dstLen)
--       cur' = drop numTooMuch cur
--       dst' = drop numMissing dst
--   in
--   -- For simplicity, assume a function exists that can produce a fully correct
--   -- stack at a cost of 200.
--   min funcCost (2 * (numMissing + numTooMuch + numToPop cur' dst'))
--   where
--   -- | Returns the number of elements to pop until the lists are equal.
--   -- Effectively, this relies upon equality between the suffices.
--   numToPop :: Eq a => [a] -> [a] -> Int
--   numToPop (x:xs) (y:ys)
--     | (x:xs) == (y:ys)  = 0
--     | otherwise         = 1 + numToPop xs ys
--   numToPop _ _ = 0


-- | Assume that calling any function costs 200.
--
-- While this may be wildly inaccurate, it does /not/ affect finding a
-- minimum-cost program. As every correct program includes /identical/
-- function calls. This means the function must be included anyway.
funcCost :: Int
funcCost = 200

-- | Returns all instructions applicable

-- Specifically exclude float-int conversion instructions. Those waste time.
--
-- Variable are externally included, as these depend on the available variables.
--
-- Note that the caller is responsible for filtering these on applicability.
-- That is, it makes no sense to apply a f32 binary operator when the stack
-- does not have two f32's on top.
allSimpleInstrs :: RelevantInstrs -> [SimpleInstr]
allSimpleInstrs ri =
  concat
    [ if riHasInt32 ri then
        concat
          [ map SConstI32 allConstI32
          , map SUnopI32 allIUnop
          , map SBinopI32 allIBinop
          , map STestopI32 allITestop
          , map SRelopI32 allIRelop
          ]
      else
        []
    , if riHasInt64 ri then
        concat
          [ map SConstI64 allConstI64
          , map SUnopI64 allIUnop
          , map SBinopI64 allIBinop
          , map STestopI64 allITestop
          , map SRelopI64 allIRelop
          ]
      else
        []
    , map SPrmInstrI32 allPrmInstr
    , map SPrmInstrI64 allPrmInstr
    , map SPrmInstrF32 allPrmInstr
    , map SPrmInstrF64 allPrmInstr
    , map SVarInstr $ allReadLocals (riReadLocals ri)
    , map SVarInstr $ allWriteLocals (riWriteLocals ri)
    , map SVarInstr $ allReadGlobals (riReadGlobals ri)
    , map SVarInstr $ allWriteGlobals (riWriteGlobals ri)
    -- , if riHasMem ri then
    --     map SMemInstr allMemInstr
    --   else
    --     []
    , if riHasFloat32 ri then
        concat
          [ map SUnopF32 allFUnop
          , map SBinopF32 allFBinop
          , map SRelopF32 allFRelop
          ]
      else
        []
    , if riHasFloat64 ri then
        concat
          [ map SUnopF64 allFUnop
          , map SBinopF64 allFBinop
          , map SRelopF64 allFRelop
          ]
      else
        []
    ]

-- | These return all `VarInstr`s that are included in the argument sets.
--
-- Mainly, this approach ensures only variables are accessed that are also
-- accessed by the original fragment.
allReadLocals, allReadGlobals, allWriteLocals, allWriteGlobals :: IntSet -> [VarInstr]
allReadLocals locals =
  let localIds = IntSet.toList locals
  in map (VLocalGet . LocalIdx) localIds
allWriteLocals locals =
  let localIds = IntSet.toList locals
  in map (VLocalSet . LocalIdx) localIds
  ++ map (VLocalTee . LocalIdx) localIds
allReadGlobals globals =
  let globalIds = IntSet.toList globals
  in map (VGlobalGet . GlobalIdx) globalIds
allWriteGlobals globals =
  let globalIds = IntSet.toList globals
  in map (VGlobalSet . GlobalIdx) globalIds

-- | 
allConstI32 :: [WI32]
allConstI32 =
  map WI32 $
    0 : 0xFFFFFFFF : map (2^) [0..5]

allConstI64 :: [WI64]
allConstI64 =
  map WI64 $
    0 : 0xFFFFFFFFFFFFFFFF : map (2^) [0..6]

allIUnop :: [IUnop a]
allIUnop = [ IUnClz, IUnCtz, IUnPopcnt ]

allIBinop :: [IBinop a]
allIBinop =
  [ IBinAdd, IBinSub, IBinMul, IBinDiv U, IBinDiv S, IBinRem U, IBinRem S
  , IBinAnd, IBinOr, IBinXor, IBinShl, IBinShr U, IBinShr S, IBinRotl, IBinRotr
  ]

allITestop :: [ITestop a]
allITestop = [ ITestEqz ]

allIRelop :: [IRelop a]
allIRelop =
  [ IRelEq, IRelNe, IRelLt U, IRelLt S, IRelGt U, IRelGt S, IRelLe U, IRelLe S
  , IRelGe U, IRelGe S
  ]

allFUnop :: [FUnop a]
allFUnop =
  [ FUnAbs, FUnNeg, FUnSqrt, FUnCeil, FUnFloor, FUnTrunc, FUnNearest ]

allFBinop :: [FBinop a]
allFBinop = [ FBinAdd, FBinSub, FBinMul, FBinDiv, FBinMin, FBinMax, FBinCopysign ]

allFRelop :: [FRelop a]
allFRelop = [ FRelEq, FRelNe, FRelLt, FRelGt, FRelLe, FRelGe ]

allPrmInstr :: [PrmInstr a]
allPrmInstr = [ PDrop, PSelect ]

allMemInstr :: [MemInstr]
allMemInstr =
  [ MMemorySize, MMemoryGrow ]
  -- Note that float memory operations are missing
  ++ applyAll
      [ MI32Load, MI64Load, MI32Store, MI64Store, MI32Load8 U
      , MI32Load8 S, MI64Load8 U, MI64Load8 S
      , MI32Load16 U, MI32Load16 S, MI64Load16 U, MI64Load16 S, MI64Load32 U
      , MI64Load32 S, MI32Store8, MI64Store8, MI32Store16, MI64Store16, MI64Store32
      ] ma
  where
  ma :: MemArg
  ma = MemArg 0 0
 
isStupid :: Seq NcfInstr -> Bool
isStupid (_ :|> NFInstrSimple _ :|> NFInstrSimple (SPrmInstrI32 PDrop)) = True
isStupid (_ :|> NFInstrSimple _ :|> NFInstrSimple (SPrmInstrI64 PDrop)) = True
isStupid (_ :|> NFInstrSimple _ :|> NFInstrSimple (SPrmInstrF32 PDrop)) = True
isStupid (_ :|> NFInstrSimple _ :|> NFInstrSimple (SPrmInstrF64 PDrop)) = True
isStupid (_ :|> NFInstrSimple (SConstI32 _) :|> NFInstrSimple (STestopI32 _)) = True
isStupid (_ :|> NFInstrSimple (SConstI64 _) :|> NFInstrSimple (STestopI64 _)) = True
isStupid (_ :|> NFInstrSimple (SConstI32 _) :|> NFInstrSimple (SUnopI32 _)) = True
isStupid (_ :|> NFInstrSimple (SConstI64 _) :|> NFInstrSimple (SUnopI64 _)) = True
isStupid (_ :|> NFInstrSimple (SConstI32 _) :|> NFInstrSimple (SConstI32 _) :|> NFInstrSimple (SBinopI32 _)) = True
isStupid (_ :|> NFInstrSimple (SConstI64 _) :|> NFInstrSimple (SConstI64 _) :|> NFInstrSimple (SBinopI64 _)) = True
isStupid (_ :|> NFInstrSimple (SConstI32 _) :|> NFInstrSimple (SConstI32 _) :|> NFInstrSimple (SRelopI32 _)) = True
isStupid (_ :|> NFInstrSimple (SConstI64 _) :|> NFInstrSimple (SConstI64 _) :|> NFInstrSimple (SRelopI64 _)) = True
isStupid _ = False

instance Hashable NcfInstr
