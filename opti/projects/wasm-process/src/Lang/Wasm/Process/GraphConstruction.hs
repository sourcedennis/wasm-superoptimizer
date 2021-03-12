{-# LANGUAGE KindSignatures, ScopedTypeVariables, FlexibleContexts, RankNTypes #-}

module Lang.Wasm.Process.GraphConstruction
  ( buildGraph
  , graphToAst
  ) where

import Melude
    ( lift,
      mapSndM,
      dropEqPrefix,
      allEqVal )
-- Stdlib imports
-- import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import qualified Control.Monad.RWS as RWS
import           Control.Monad.RWS ( RWS, RWST )
import           Control.Monad.Fail ( MonadFail )
import qualified Data.IntMap.Strict as IntMap
import           Data.IntMap.Strict ( IntMap )
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty ((:|)) )
import           Data.Maybe ( fromMaybe )
-- Extra stdlib imports
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!), (!?) )
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import Lang.Wasm.Ast
  ( LabelIdx (..), Instr (..), FuncIdx (..), ValType (..)
  , InstrCtx (..), simpleInstrEffect
  )
-- Local imports
import           Algorithm.Graph ( removeIndirection )
import qualified Lang.Wasm.Process.Structures as P
import           Lang.Wasm.Process.Structures
  -- ( SingleNode (..), SingleGraph, SingleId, SingleEdge, DropKeep (..)
  -- , DroppedVals, GraphNode (..), KeptVals
  -- , newDropKeep, freezeSingle, freezeMulti
  -- )

import qualified Control.Monad.State.Strict as S
import           Control.Monad.State.Strict ( MonadState, StateT, runStateT )

buildGraph
  :: forall m
  .  MonadFail m
  => InstrCtx
  -> [Instr]
  -> m (GraphMut ())
buildGraph ctx xs =
  do
    (startI, g) <- runStateT (buildInstrs xs) IdList.empty
    let (endI, g') = IdList.append (NodeTerminal ()) g
    return (startI, g', endI)
  where
  buildInstrs :: [Instr] -> StateT (IdList (GraphNode ())) m NodeId
  buildInstrs [] = addNode $ NodeNaturalEnd ()
  buildInstrs (x:xs) =
    do
      xsI <- buildInstrs xs
      buildInstr x xsI
  
  buildInstr :: Instr -> NodeId -> StateT (IdList (GraphNode ())) m NodeId
  buildInstr InstrNop            i = return i
  buildInstr InstrUnreachable    _ = addNode $ NodeTrapped ()
  buildInstr (InstrSimple instr) i = addNode $ Node () (PgeInstr instr i)
  buildInstr (InstrBlock ft xs)  i =
    do
      xsG <- buildInstrs xs
      addNode $ Node () (PgeBlock ft xsG i)
  buildInstr (InstrLoop ft xs)  i =
    do
      xsG <- buildInstrs xs
      addNode $ Node () (PgeLoop ft xsG i)
  buildInstr (InstrIf ft xs ys)  i =
    do
      xsG <- buildInstrs xs
      ysG <- buildInstrs ys
      addNode $ Node () (PgeIf ft xsG ysG i)
  buildInstr (InstrBr lIdx)            i = addNode $ Node () (PgeBr lIdx)
  buildInstr (InstrBrIf lIdx)          i = addNode $ Node () (PgeBrIf lIdx i)
  buildInstr (InstrBrTable lIdxs lIdx) i = addNode $ Node () (PgeBrTable lIdxs lIdx)
  buildInstr InstrReturn               i = addNode $ NodeReturn ()
  buildInstr (InstrCall fIdx)          i = addNode $ Node () (PgeCall fIdx i)
  buildInstr (InstrCallIndirect ft)    i = addNode $ Node () (PgeCallIndirect ft i)

  addNode :: GraphNode () -> StateT (IdList (GraphNode ())) m NodeId
  addNode = S.state . IdList.append

graphToAst
  :: forall m a
  .  MonadFail m
  => InstrCtx
  -> GraphFrozen a
  -> m [Instr]
graphToAst ctx (startI, g, _) = visit startI
  where
  visit :: NodeId -> m [Instr]
  visit i =
    case g !? i of
      Nothing -> fail "Missing node" -- statically invalid graph
      Just (Node _ (PgeInstr x j)) ->
        (InstrSimple x:) <$> visit j
      Just (Node _ (PgeIf ft ifI elseI j)) ->
        (:) <$> (InstrIf ft <$> visit ifI <*> visit elseI) <*> visit j
      Just (Node _ (PgeBlock ft blockI j)) ->
        (:) <$> (InstrBlock ft <$> visit blockI) <*> visit j
      Just (Node _ (PgeLoop ft blockI j)) ->
        (:) <$> (InstrLoop ft <$> visit blockI) <*> visit j
      Just (Node _ (PgeBr lIdx)) ->
        return [InstrBr lIdx]
      Just (Node _ (PgeBrIf lIdx j)) ->
        (InstrBrIf lIdx:) <$> visit j
      Just (Node _ (PgeBrTable lIdxs lIdx)) ->
        return [InstrBrTable lIdxs lIdx]
      Just (Node _ (PgeCall fIdx j)) ->
        (InstrCall fIdx:) <$> visit j
      Just (Node _ (PgeCallIndirect ft j)) ->
        (InstrCallIndirect ft:) <$> visit j
      Just (NodeNaturalEnd _) -> return []
      Just (NodeReturn _)     -> return [InstrReturn]
      Just (NodeTrapped _)    -> return [InstrUnreachable]
      Just (NodeTerminal _)   -> return []

-- -- | Constructs a deterministic process graph from the function body.
-- buildGraph
--   :: forall m
--   .  MonadFail m
--   => InstrCtx  -- ^ Module/Function context
--   -> [ValType] -- ^ Output signature
--   -> [Instr]   -- ^ Function body
--   -> m (SingleId, SingleGraph ()) -- ^ The graph with its root node id
-- buildGraph ctx outSignature xs =
--   do
--     -- trace (show xs) $ return ()
--     -- The graph is constructed in two stages:
--     -- + First, a partial graph is constructed. This one may contain indirections.
--     --   Indirections are necessary when constructing looping sequences, like:
--     --   @(loop ... (br 0))@.
--     -- + Secondly, the indirections are removed. Infinite-loops in the
--     --   indirections are replaced by `GraphForever`.
--     (pRootI, pGraph) <- runStateT (buildGraph' xs) IdList.empty
--     return $ removeIntermediateNodes (pRootI, pGraph)
--   where
--   buildGraph' :: [Instr] -> StateT PartialGraph m SingleId
--   buildGraph' xs =
--     do
--       firstI <- emptyNode
--       mLastI <- instrsGraphS (Stack [] []) firstI xs
--       maybe (return ()) (`updateNode` GraphTerminal ()) mLastI
--       return firstI

--   -- |
--   --
--   -- Returns the id of the next empty node after the inserted node.
--   instrsGraphS :: Stack -> SingleId -> [Instr] -> StateT PartialGraph m (Maybe SingleId)
--   instrsGraphS s selfI []     = return $ Just selfI
--   instrsGraphS s selfI (x:xs) =
--     do
--       mRes <- instrGraphS s selfI x
--       case mRes of
--         Just (ft, nextI) ->
--           do
--             s' <- lift $ applyFuncType ft s
--             instrsGraphS s' nextI xs
--         Nothing -> return Nothing

--   -- Returns the id of the next empty node after the inserted node.
--   instrGraphS :: MonadFail m
--               => Stack
--               -> SingleId
--               -> Instr
--               -> StateT PartialGraph m (Maybe (FuncType, SingleId))
--   instrGraphS s selfI InstrNop = return $ Just (FuncType [] [], selfI)
--   instrGraphS s selfI InstrUnreachable =
--     do
--       -- trace (show "Unreachable") $ return ()
--       nextI <- updateDropKeep selfI [] (completeStack s)
--       updateNode nextI (GraphTrapped ())
--       return Nothing
--   instrGraphS s selfI (InstrSimple instr) =
--     do
--       -- trace (showString "Simple " . shows instr $ "") $ return ()
--       (xs, ys) <- lift $ simpleInstrEffect ctx instr
--       afterI <- emptyNode
--       updateNode selfI $ GraphNode () (P.PeInstr instr afterI)
--       return $ Just (FuncType xs ys, afterI)
--   instrGraphS s selfI (InstrBlock bt xs) =
--     do
--       -- trace (showString "Block " . shows bt $ "") $ return ()
--       let bt' = toFt bt
--           inVals    = ftParams bt'
--           labelVals = ftResults bt'
--       afterI <- emptyNode
--       s' <- lift $ pushLabel afterI inVals labelVals s
--       mAfterI <- instrsGraphS s' selfI xs
--       maybe (return ()) (`updateIndirection` afterI) mAfterI
--       return $ Just (bt', afterI)
--   instrGraphS s selfI (InstrLoop bt xs) =
--     do
--       -- trace (showString "Loop " . shows bt $ "") $ return ()
--       let bt' = toFt bt
--           inVals    = ftParams bt'
--           labelVals = inVals -- A break to the loop label jumps back to the start
--       s' <- lift $ pushLabel selfI inVals labelVals s 
--       mAfterI <- instrsGraphS s' selfI xs
--       case mAfterI of
--         Nothing -> return Nothing
--         Just afterI ->
--           if afterI == selfI then
--             do
--               updateNode selfI $ GraphForever ()
--               return Nothing
--           else
--             return $ Just (bt', afterI)
--   instrGraphS s selfI (InstrIf bt xs ys) =
--     do
--       -- trace (showString "If " . shows bt $ "") $ return ()
--       let bt' = toFt bt
--       let inVals    = ftParams bt'
--           labelVals = ftResults bt'
--       afterI <- emptyNode
--       s' <- lift $ pushLabel afterI inVals labelVals =<< popVals [TI32] s
--       ifI <- emptyNode
--       elseI <- emptyNode
--       updateNode selfI (GraphNode () (P.PeIf (DKKeepAll, ifI) elseI))

--       maybe (return ()) (`updateIndirection` afterI) =<< instrsGraphS s' ifI xs
--       maybe (return ()) (`updateIndirection` afterI) =<< instrsGraphS s' elseI ys
--       return $ Just (FuncType (ftParams bt' ++ [TI32]) (ftResults bt'), afterI)
--   instrGraphS s selfI (InstrBr lIdx) =
--     do
--       -- trace "Br" $ return ()
--       (kv, (dv, labelI)) <- lift $ labelEffect lIdx s
--       afterI <- updateDropKeep selfI kv dv
--       updateIndirection afterI labelI
--       return Nothing
--   instrGraphS s selfI (InstrBrIf lIdx) =
--     do
--       -- trace "Brif" $ return ()
--       (kv, (dv, labelI)) <- lift $ labelEffect lIdx =<< popVals [TI32] s
--       afterI <- emptyNode
--       updateEdge selfI $ P.PeIf (newDropKeep kv dv, labelI) afterI
--       return $ Just (FuncType [TI32] [], afterI)
--   instrGraphS s selfI (InstrBrTable lIdxs lIdx) =
--     do
--       -- trace "Brtable" $ return ()
--       s' <- lift $ popVals [TI32] s
--       lEffects <- lift $ mapM (`labelEffect` s') (lIdx :| lIdxs)
--       kv <- lift $ allEqVal $ NE.map fst lEffects
--       let (defTarget :| targets) = NE.map snd lEffects
--       updateEdge selfI (P.PeTable kv targets defTarget)
--       return Nothing
--   instrGraphS s selfI InstrReturn =
--     do
--       -- trace "Return" $ return ()
--       let fullStack = completeStack s
--           keptVals = reverse outSignature
--       droppedVals <- dropEqPrefix keptVals fullStack
--       afterI <- updateDropKeep selfI keptVals droppedVals
--       updateNode afterI $ GraphTerminal ()
--       return Nothing
--   instrGraphS s selfI (InstrCall fIdx) =
--     do
--       -- trace "Call" $ return ()
--       ft <- fmap toFt $ lift $ Ast.itxFuncType ctx fIdx
--       afterI <- emptyNode
--       updateEdge selfI $ P.PeCall fIdx afterI
--       return $ Just (ft, afterI)
--   instrGraphS s selfI (InstrCallIndirect ft) =
--     do
--       -- trace "CallIndirect" $ return ()
--       -- One I32 is popped, which references the function in the table.
--       let ft' = toFt ft
--       let t = FuncType (ftParams ft' ++ [TI32]) (ftResults ft')
--       afterI <- emptyNode
--       updateEdge selfI $ P.PeCallIndirect ft afterI
--       return $ Just (t, afterI)

--   -- | Add a explicit node to the partial graph.
--   emptyNode :: MonadState PartialGraph s => s SingleId
--   emptyNode = S.state $ IdList.append $ Right $ GraphTrapped ()
  
--   -- | Add a explicit node to the partial graph.
--   updateNode :: MonadState PartialGraph s => SingleId -> SingleNode () -> s ()
--   updateNode i = S.modify . IdList.replace i . Right

--   -- |
--   updateEdge :: MonadState PartialGraph s => SingleId -> SingleEdge -> s ()
--   updateEdge i = updateNode i . GraphNode ()

--   -- | Updates a node in the partial graph to a indirection node.
--   updateIndirection :: MonadState PartialGraph s => SingleId -> SingleId -> s ()
--   updateIndirection i j = S.modify $ IdList.replace i (Left j)
  
--   updateDropKeep :: SingleId -> KeptVals -> DroppedVals -> StateT PartialGraph m SingleId
--   updateDropKeep selfI _  []     = return selfI
--   updateDropKeep selfI kv (x:xs) =
--     do
--       nextI <- emptyNode
--       updateEdge selfI (P.PeDropMany kv (x:|xs) nextI)
--       return nextI

--   -- | Returns the effect (and node) of jumping to the label.
--   labelEffect :: LabelIdx -> Stack -> m (KeptVals, (DroppedVals, SingleId))
--   labelEffect (LabelIdx lIdx) (Stack xs ys) =
--     do
--       -- The full stack until the label
--       (nodeI, keptVals, fullStacks) <- labelEffect' lIdx xs
--       let fullStack = concat fullStacks
--       droppedVals <- dropEqPrefix keptVals fullStack
--       return (keptVals, (droppedVals, nodeI))
--     where
--     labelEffect' :: Int -> [LabelStack] -> m (SingleId, KeptVals, [SimpleStack])
--     labelEffect' 0 (x:_)  = pure (lsNode x, lsSignature x, [lsStack x])
--     labelEffect' n (x:xs) =
--       do
--         (nodeI, nk, zs) <- labelEffect' (n-1) xs
--         pure (nodeI, nk, lsStack x : zs)
--     labelEffect' _ [] = fail "Invalid label"

--   -- | Returns /all/ values on the stack.
--   completeStack :: Stack -> SimpleStack
--   completeStack (Stack xs ys) = concat (map lsStack xs ++ [ys])

--   -- WARNING: The `ValType` arrays are in /reverse/ order from the stack. (In line
--   --   with `FuncType` in the specification)
--   pushLabel :: SingleId -> [ValType] -> [ValType] -> Stack -> m Stack
--   pushLabel nodeI inVals labelVals s =
--     do
--       let labelStack = reverse inVals
--       Stack xs ys <- popVals labelStack s
--       let newLabel = LabelStack (reverse labelVals) nodeI labelStack
--       return $ Stack (newLabel : xs) ys

--   applyFuncType :: FuncType -> Stack -> m Stack
--   applyFuncType (FuncType xs ys) =
--     fmap (pushVals $ reverse ys) . popVals (reverse xs)

--   popVals :: SimpleStack -> Stack -> m Stack
--   popVals xs (Stack [] ys) =
--     do
--       ys' <- dropEqPrefix xs ys
--       return $ Stack [] ys'
--   popVals xs (Stack (LabelStack a n ys : zs) ws) =
--     do
--       ys' <- dropEqPrefix xs ys
--       return $ Stack (LabelStack a n ys' : zs) ws

--   pushVals :: SimpleStack -> Stack -> Stack
--   pushVals xs (Stack [] ys) =
--     Stack [] (xs ++ ys)
--   pushVals xs (Stack (LabelStack a n ys : zs) ws) =
--     Stack (LabelStack a n (xs ++ ys) : zs) ws


-- -- # Internal Data Structures #

-- -- | A stack of values within a label/stackframe scope. (top = head)
-- type SimpleStack = [ValType]

-- -- | A stackframe for a label (on `Stack`).
-- --
-- -- TODO: Rename `LabelFrame`
-- data LabelStack =
--   LabelStack {
--     -- | When jumping to the label, keep these top values on the stack.
--     lsSignature  :: !KeptVals
--     -- | The label's node, to which `br`-family instructions jump to
--   , lsNode       :: !SingleId
--   , lsStack      :: !SimpleStack
--   }

-- -- | A stack within a function call. Label frame are explicitly represented.
-- -- the top is at the left-most label. The second argument is the main stack
-- -- before any labels.
-- data Stack = Stack ![LabelStack] !SimpleStack

-- -- | A node in `PartialGraph`
-- type PartialNode = Either SingleId (SingleNode ())

-- -- | The partial graph contains indirections (e.g., `Left 4`) to other nodes in
-- -- the graph. These indirections are convenient during construction, but are
-- -- useless externally (and are thus removed by `removeIntermediateNodes`).
-- type PartialGraph = IdList PartialNode

-- -- | Non-`Vector` version of `Ast.FuncType`; which is more convenient for use in
-- -- this module (and avoids repeated `Vector` -> `[ValType]` conversions).
-- data FuncType =
--   FuncType {
--     ftParams   :: ![ValType]
--   , ftResults  :: ![ValType]
--   }

-- -- | Converts the AST Vector `Ast.FuncType` to the non-Vector `FuncType`.
-- toFt :: Ast.FuncType -> FuncType
-- toFt (Ast.FuncType xs ys) = FuncType (Vector.toList xs) (Vector.toList ys)

-- -- | Removes the indirection nodes.
-- removeIntermediateNodes :: (SingleId, PartialGraph) -> (SingleId, SingleGraph ())
-- removeIntermediateNodes (rootI, graph) =
--   let graphVec = Vector.fromList $ IdList.toList graph
--   in removeIndirection rebuildNode (graphVec !) (GraphForever ()) rootI
--   where
--   rebuildNode :: Monad m => ( Int -> m Int ) -> SingleNode a -> m (SingleNode a)
--   rebuildNode f (GraphNode a e) = GraphNode a <$> rebuildEdge f e
--   rebuildNode _ n               = return n

--   rebuildEdge :: Monad m => ( Int -> m Int ) -> SingleEdge -> m SingleEdge
--   rebuildEdge f (P.PeInstr instr x)              = P.PeInstr instr <$> f x
--   rebuildEdge f (P.PeIf (s,x) y)                 = P.PeIf <$> ((s,) <$> f x) <*> f y
--   rebuildEdge f (P.PeTable numKeep xs x)         = P.PeTable numKeep <$> mapM (mapSndM f) xs <*> mapSndM f x
--   rebuildEdge f (P.PeCall fIdx x)                = P.PeCall fIdx <$> f x
--   rebuildEdge f (P.PeCallIndirect ft x)          = P.PeCallIndirect ft <$> f x
--   rebuildEdge f (P.PeDropMany numKeep numDrop x) = P.PeDropMany numKeep numDrop <$> f x
