
module Lang.Wasm.Process.Flow
  ( JmpTableIdx (..)
  , FlowEdge (..)
  , DroppedVals
  , KeptVals
  , graphFlow
  , graphFwdFlow
  ) where

import Melude
-- Stdlib imports
import qualified Data.List.NonEmpty as NE
-- Extra stdlib imports
import           Control.Monad.Fail ( MonadFail )
import qualified Control.Monad.State as S
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import Lang.Wasm.Ast
  ( WI32 (..), SimpleInstr, FuncIdx, FuncType (..), ResultType, ValType (..)
  , InstrCtx, LabelIdx (..)
  )
import Data.MapUtils ( intMapLookupFunc )
-- Local imports
import Lang.Wasm.Process.Structures
import Lang.Wasm.Process.Stack
import qualified Lang.Wasm.Process.Stack as Stack


data JmpTableIdx
  = JmpTableIdx WI32
  | JmpTableGeq WI32
  deriving Show

type NumLabels = Int

type IsLoopLabel = Bool

-- -- | Top is at the head
type DroppedVals = [ValType]
-- | Top is at the head
type KeptVals = [ValType]

-- | When `IsLoopJmp` is `True`, the final label is /not/ popped.
-- As illustrated by this program, where the loop's label is preserved.
-- (loop (param i32)
--   (br 0)
-- )

-- | While `Edge` ensures mutual exclusivity on its branches, this flow edge
-- does not.
data FlowEdge
  = PfeInstr SimpleInstr
  | PfeCall FuncIdx
  | PfeCallIndirect FuncType
  | PfeIfTrue KeptVals -- enters a if-else block
  | PfeIfFalse KeptVals -- enters a if-else block
  | PfeEnter KeptVals -- enters a block/loop
  | PfeBr KeptVals (NonEmpty DroppedVals) IsLoopLabel
  | PfeBrIf KeptVals (NonEmpty DroppedVals) IsLoopLabel
  | PfeBrElse
  | PfeBrTable JmpTableIdx KeptVals (NonEmpty DroppedVals) IsLoopLabel
    -- "magic" conjures values of the given type (left field), and drops all others.
  | PfeMagic [ValType] (NonEmpty DroppedVals)

instance Show FlowEdge where
  showsPrec d (PfeInstr x) =
    showParen (d>10) (ß"PfeInstr " . shows x)
  showsPrec d (PfeCall fIdx) =
    showParen (d>10) (ß"PfeCall " . shows fIdx)
  showsPrec d (PfeCallIndirect ft) =
    showParen (d>10) (ß"PfeCallIndirect " . shows ft)
  showsPrec d (PfeIfTrue kv) =
    showParen (d>10) (ß"PfeIfTrue " . shows kv)
  showsPrec d (PfeIfFalse kv) =
    showParen (d>10) (ß"PfeIfFalse " . shows kv)
  showsPrec d (PfeEnter kv) =
    showParen (d>10) (ß"PfeEnter " . shows kv)
  showsPrec d (PfeBr kv dv isLoop) =
    let loopStr = if isLoop then "loop" else "no-loop"
    in showParen (d>10) (ß"PfeBr " . shows kv . ß" " . shows (NE.toList dv) . ß" " . ß loopStr)
  showsPrec d (PfeBrIf kv dv isLoop) =
    let loopStr = if isLoop then "loop" else "no-loop"
    in showParen (d>10) (ß"PfeBrIf " . shows kv . ß" " . shows (NE.toList dv) . ß" " . ß loopStr)
  showsPrec d PfeBrElse = ß"PfeBrElse"
  showsPrec d (PfeBrTable tIdx kv dv isLoop) =
    let loopStr = if isLoop then "loop" else "no-loop"
    in showParen (d>10) (ß"PfeBrTable " . shows tIdx . ß" " . shows kv . ß" " . shows (NE.toList dv) . ß" " . ß loopStr)

graphFlow :: MonadFail m
          => InstrCtx
          -> ResultType
          -> GraphFrozen a
          -> m ( NodeId -> [ ( FlowEdge, NodeId ) ] )
graphFlow ctx rt (rootI, g, endI) =
  do
    let s = FullStack [LabelFrame [] rt (False, endI)]
    flow <- execStateT (visit ctx g s rootI (rt, endI)) IntMap.empty
    -- \i -> IntMap.lookup i flow
    return $ intMapLookupFunc [] flow

-- |
--
-- As the graph represents /structured control flow/, its edges can be separated
-- into two sets:
-- * Forward edges (forming a DAG)
-- * Backward edges (A,B), where B dominates A
-- This functions returns only the /forward/ flow edges. The advantage of
-- forward flow edges is that a topological order surely exists.
graphFwdFlow :: MonadFail m
             => InstrCtx
             -> ResultType
             -> GraphFrozen a
             -> m ( NodeId -> [ ( FlowEdge, NodeId ) ] )
graphFwdFlow ctx rt g =
  do
    fFlow <- graphFlow ctx rt g
    return $ filter (not . isBackEdge . fst) . fFlow
  where
  isBackEdge :: FlowEdge -> Bool
  isBackEdge (PfeBr _ _ isLoop)        = isLoop
  isBackEdge (PfeBrIf _ _ isLoop)      = isLoop
  isBackEdge (PfeBrTable _ _ _ isLoop) = isLoop
  isBackEdge _                         = False


visit :: MonadFail m
      => InstrCtx
      -> Vector (GraphNode a)
      -> FullStack (IsLoopLabel, NodeId)
      -> NodeId
      -> (ResultType, NodeId)
      -> StateT (IntMap [(FlowEdge, NodeId)]) m ()
visit ctx g s i after@(outType, afterI) =
  case g !? i of
    Nothing -> fail "Invalid graph"
    Just (Node a (PgeInstr x j)) ->
      do
        S.modify (IntMap.insert i [(PfeInstr x, j)])
        eff <- Ast.simpleInstrEffect ctx x
        s' <- lift $ failMaybeMsg "invalid instr input" $ Stack.applyEffect eff s
        visit ctx g s' j after
    Just (Node a (PgeIf ft ifI elseI j)) ->
      do
        -- Pop the branch condition
        s' <- lift $ failMaybeMsg "invalid branch condition" $ Stack.applyEffect ([TI32],[]) s

        let kv = reverse $ Vector.toList $ Ast.ftParams ft
        S.modify (IntMap.insert i [(PfeIfTrue kv, ifI), (PfeIfFalse kv, elseI)])

        sBody <- failMaybeMsg "invalid if label" $ Stack.applyPushLabel ft (False, j) s'
        visit ctx g sBody ifI (Ast.ftResults ft, j)
        visit ctx g sBody elseI (Ast.ftResults ft, j)

        sAfter <- lift $ failMaybeMsg "invalid stack after" $ Stack.applyFuncType ft s'
        visit ctx g sAfter j after
    Just (Node a (PgeBlock ft bodyG j)) ->
      do
        let kv = reverse $ Vector.toList $ Ast.ftParams ft
        S.modify (IntMap.insert i [(PfeEnter kv, bodyG)])

        sBody <- failMaybeMsg "invalid block label" $ Stack.applyPushLabel ft (False, j) s
        visit ctx g sBody bodyG (Ast.ftResults ft, j)

        sAfter <- lift $ failMaybeMsg "invalid stack after" $
                    Stack.applyFuncType ft s
        visit ctx g sAfter j after
    Just (Node a (PgeLoop ft bodyI j)) ->
      do
        let kv = reverse $ Vector.toList $ Ast.ftParams ft
        S.modify (IntMap.insert i [(PfeEnter kv, bodyI)])

        sBody <- failMaybeMsg "invalid loop label" $
                  Stack.applyPushLabel (FuncType (Ast.ftParams ft) (Ast.ftParams ft)) (True, bodyI) s
        visit ctx g sBody bodyI (Ast.ftResults ft, j)

        sAfter <- lift $ failMaybeMsg "invalid stack after" $
                    Stack.applyFuncType ft s
        visit ctx g sAfter j after
    Just (Node a (PgeBr lIdx)) ->
      do
        (x:|xs, rt, (isLoop, j), _) <- Stack.popLabel lIdx s
        let kv = reverse (Vector.toList rt) -- :: KeptValues
        x' <- failMaybeMsg "invalid br" $ dropEqPrefix kv x
        S.modify (IntMap.insert i [(PfeBr kv (x':|xs) isLoop, j)])
        -- Don't visit further. The outer scope does that
    Just (Node a (PgeBrIf lIdx j)) ->
      do
        -- Pop the branch condition
        s' <- lift $ Stack.applyEffect ([TI32],[]) s

        (x:|xs, rt, (isLoop, breakI), _) <- Stack.popLabel lIdx s'
        let kv = reverse (Vector.toList rt) -- :: KeptValues
        x' <- failMaybeMsg "invalid br_if" $ dropEqPrefix kv x
        S.modify (IntMap.insert i [(PfeBrIf kv (x':|xs) isLoop, breakI), (PfeBrElse, j)])
        visit ctx g s' j after
    Just (Node a (PgeBrTable lIdxs lIdxDef)) ->
      do
        -- Pop the label index
        s' <- lift $ Stack.applyEffect ([TI32],[]) s

        jmps   <- zipWithM (\idx lIdx -> labelEdge (JmpTableIdx $ WI32 idx) lIdx s') [0..] lIdxs
        defJmp <- labelEdge (JmpTableGeq $ WI32 $ toNum $ length lIdxs) lIdxDef s'

        S.modify (IntMap.insert i (defJmp:jmps))
      where
      labelEdge :: MonadFail m
                => JmpTableIdx
                -> LabelIdx
                -> FullStack (IsLoopLabel, NodeId)
                -> m (FlowEdge, NodeId)
      labelEdge tIdx lIdx s =
        do
          (x:|xs, rt, (isLoop, breakI), _) <- Stack.popLabel lIdx s
          let kv = reverse (Vector.toList rt) -- :: KeptValues
          x' <- failMaybeMsg "invalid br_table" $ dropEqPrefix kv x
          return (PfeBrTable tIdx kv (x':|xs) isLoop, breakI)
    Just (Node a (PgeCall fIdx j)) ->
      do
        ft <- Ast.itxFuncType ctx fIdx
        s' <- lift $ Stack.applyFuncType ft s
        S.modify (IntMap.insert i [(PfeCall fIdx, j)])
        visit ctx g s' j after
    Just (Node a (PgeCallIndirect ft j)) ->
      do
        s' <- lift $ (Stack.applyFuncType ft <=< Stack.applyEffect ([TI32],[])) s
        S.modify (IntMap.insert i [(PfeCallIndirect ft, j)])
        visit ctx g s' j after
    Just (NodeNaturalEnd a) ->
      do
        (x:|[], _, _, _) <- Stack.popLabel (LabelIdx 0) s
        let kv = reverse (Vector.toList outType) -- :: KeptValues
        lift $ failIf (x /= kv) "Invalid stack"
        S.modify (IntMap.insert i [(PfeBr kv ([]:|[]) False, afterI)])
        -- Don't visit further. The outer scope does that
    Just (NodeReturn a) ->
      do
        (x:|xs, rt, (_,j)) <- Stack.popAll s
        let kv = reverse (Vector.toList rt) -- :: KeptValues
        x' <- dropEqPrefix kv x
        S.modify (IntMap.insert i [(PfeBr kv (x':|xs) False, j)])
        -- Don't visit further. The outer scope does that
    Just (NodeTerminal a) -> return ()
    Just (NodeTrapped a) ->
      do
        (xs, rt, (_,j)) <- Stack.popAll s
        let kv = reverse (Vector.toList rt)
        S.modify (IntMap.insert i [(PfeMagic kv xs, j)])
        -- Don't visit further. The outer scope does that

-- edgeFlow (PgeInstr instr i) = [(PfeInstr instr, i)]
-- edgeFlow (PgeIf (dk, a) b) = [(PfeIfTrue dk, a), (PfeIfFalse, b)]
-- edgeFlow (PgeTable nk xs x) =
--   zipWith (tableEdge nk) (JmpTableGeq (WI32 $ toNum $ length xs) : map (JmpTableIdx . WI32) [0..]) (x:xs)
--   where
--   tableEdge :: KeptVals -> JmpTableIdx -> (DroppedVals, a) -> ( FlowEdge, a )
--   tableEdge kv idx (x:xs, a) = (PfeTable idx (DKKeep nk (x :| xs)), a)
--   tableEdge _  idx ([],   a) = (PfeTable idx DKKeepAll, a)
-- edgeFlow (PgeCall fIdx i) = [(PfeCallExternal fIdx, i)]
-- edgeFlow (PgeCallIndirect ft i) = [(PfeCallIndirect ft, i)]
-- edgeFlow (PgeDropMany nk dv i) = [(PfeDropMany nk dv, i)]

-- edgeFlow (PgeInstr instr i) = [(PfeInstr instr, i)]

--  | PgeIf !FuncType !NodeId !NodeId !NodeId -- if, else, next
--  | PgeBlock !FuncType !NodeId !NodeId -- block, next
--  | PgeLoop !FuncType !NodeId !NodeId -- loop, next
--  | PgeBr !LabelIdx
--  | PgeBrIf !LabelIdx !NodeId
--  | PgeBrTable ![LabelIdx] !LabelIdx
--  | PgeCall !FuncIdx !NodeId
--  | PgeCallIndirect !FuncType !NodeId
