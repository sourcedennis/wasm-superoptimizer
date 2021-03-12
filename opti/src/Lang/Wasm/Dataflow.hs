{-# LANGUAGE RankNTypes, KindSignatures #-}

-- | Data-flow representation that more-or-less corresponds to the process graph
-- representation. It differs on the fact that no mutual exclusivity on
-- branches is not ensured; This makes it generally impossible to reconstruct
-- structured control flow, but is essential for dataflow analysis.
module Lang.Wasm.Dataflow where

import Melude
-- Stdlib imports
import Control.Monad ( void, replicateM, when )
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty ((:|)) )
-- Extra stdlib 
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
import           Control.Monad.State ( State )
-- External library imports
import qualified Data.IdList as IdList
-- Local library imports
import Lang.Wasm.Ast
  ( FuncIdx, FuncType, SimpleInstr, ValType (..), WI32 (..), FuncType (..)
  , PVal (..) )
import Lang.Wasm.Data ( PGlobal (..) )
-- Local imports
import qualified Lang.Wasm.Algebra as A
import           Lang.Wasm.Algebra ( SimpleAlgebra, MonadWasmState )
import qualified Lang.Wasm.AlgebraBwd as AB
import           Lang.Wasm.AlgebraBwd ( SimpleBwdAlgebra )
import qualified Lang.Wasm.Process as P
import           Lang.Wasm.Process


data FlowAlgebra i32 i64 f32 f64 mem ( m :: * -> * ) =
  FlowAlgebra {
    instrAlgebra  :: SimpleAlgebra i32 i64 f32 f64 m
  , callExternal  :: FuncIdx
                  -> [PVal i32 i64 f32 f64] -- globals
                  -> mem
                  -> [PVal i32 i64 f32 f64] -- args
                  -> [ValType] -- results
                  -> m ([PVal i32 i64 f32 f64], mem, [PVal i32 i64 f32 f64]) -- globals, mem, result
  , callIndirect  :: i32
                  -> [PVal i32 i64 f32 f64] -- globals
                  -> mem
                  -> [PVal i32 i64 f32 f64] -- args
                  -> [ValType] -- results
                  -> m ([PVal i32 i64 f32 f64], mem, [PVal i32 i64 f32 f64]) -- globals, mem, result
  
    -- Assertions as obtained from branches (mainly path constraints)
  , assertTrue     :: i32 -> m ()
  , assertConstI32 :: i32 -> WI32 -> m ()
  , assertGeq      :: i32 -> WI32 -> m ()
  }

data FlowBwdAlgebra i8 i32 i64 f32 f64 ( m :: * -> * ) =
  FlowBwdAlgebra {
    bwdInstrAlgebra  :: SimpleBwdAlgebra i8 i32 i64 f32 f64 m
  , bwdCallExternal  :: FuncIdx
                     -> [PVal i32 i64 f32 f64] -- globals
                     -> [PVal i32 i64 f32 f64] -- args
                     -> [ValType] -- result types
                     -> m ([PVal i32 i64 f32 f64], [PVal i32 i64 f32 f64]) -- globals, results
  , bwdCallIndirect  :: [PVal i32 i64 f32 f64]  -- globals
                     -> [PVal i32 i64 f32 f64] -- args
                     -> [ValType] -- result types
                     -> m ([PVal i32 i64 f32 f64], [PVal i32 i64 f32 f64], i32) -- globals, results
  }

-- |
--
-- There are some subtleties in the implementation concerning reversing lists.
-- When popping values, then must be pushed back in reverse order. For functions
-- values, the right-most element (in both params and results) is the stack's
-- top-most element.
execFlowAlgebra :: MonadWasmState i32 i64 f32 f64 mem m
                => ( FuncIdx -> FuncType )
                -> FlowAlgebra i32 i64 f32 f64 mem m
                -> FlowEdge
                -> m ()
execFlowAlgebra fType a (PfeInstr instr) = A.execAlgebra (instrAlgebra a) instr
execFlowAlgebra fType a (PfeCall fIdx) =
  do
    let FuncType xs ys = fType fIdx
    args <- reverse <$> mapM popStackTypeVal (reverse $ Vector.toList xs)
    gs <- A.getGlobals
    mem <- A.getMemory
    (gs', mem', results) <- callExternal a fIdx gs mem args (Vector.toList ys)
    A.setGlobals gs'
    A.setMemory mem'
    mapM_ A.pushStack results
execFlowAlgebra fType a (PfeCallIndirect (FuncType xs ys)) =
  do
    fIdx <- A.popStackI32
    args <- reverse <$> mapM popStackTypeVal (reverse $ Vector.toList xs)
    gs <- A.getGlobals
    mem <- A.getMemory
    (gs', mem', results) <- callIndirect a fIdx gs mem args (Vector.toList ys)
    A.setGlobals gs'
    A.setMemory mem'
    mapM_ A.pushStack results
execFlowAlgebra fType a (PfeIfTrue kv)   =
  do
    cond <- A.popStackI32
    args <- mapM popStackTypeVal kv
    A.pushLabel
    mapM_ A.pushStack (reverse args)
    assertTrue a cond
execFlowAlgebra fType a (PfeIfFalse kv)   =
  do
    cond <- A.popStackI32
    args <- mapM popStackTypeVal kv
    A.pushLabel
    mapM_ A.pushStack (reverse args)
    assertFalse a cond
execFlowAlgebra fType a (PfeEnter kv)   =
  do
    args <- mapM popStackTypeVal kv
    A.pushLabel
    mapM_ A.pushStack (reverse args)
execFlowAlgebra fType a (PfeBr kv dv isLoop)   =
  do
    results <- mapM popStackTypeVal kv
    dropLabels dv
    -- When jumping to a loop, its label is preserved. Otherwise, the label is
    -- popped.
    when isLoop A.pushLabel
    mapM_ A.pushStack (reverse results)
execFlowAlgebra fType a (PfeBrIf kv dv isLoop)   =
  do
    cond <- A.popStackI32
    results <- mapM popStackTypeVal kv
    dropLabels dv
    when isLoop A.pushLabel
    mapM_ A.pushStack (reverse results)
    assertTrue a cond
execFlowAlgebra fType a PfeBrElse =
  do
    cond <- A.popStackI32
    assertFalse a cond
execFlowAlgebra fType a (PfeBrTable tIdx kv dv isLoop) =
  do
    labelId <- A.popStackI32
    results <- mapM popStackTypeVal kv
    dropLabels dv
    when isLoop A.pushLabel
    mapM_ A.pushStack (reverse results)
    
    case tIdx of
      JmpTableIdx i -> assertConstI32 a labelId i
      JmpTableGeq i -> assertGeq a labelId i
execFlowAlgebra fType a (PfeMagic kv dv) =
  do
    dropLabels dv
    mapM_ A.pushStack =<< mapM (A.magic $ instrAlgebra a) (reverse kv)

dropLabels :: MonadWasmState i32 i64 f32 f64 mem m => NonEmpty DroppedVals -> m ()
dropLabels (xs:|[])       = mapM_ popStackTypeVal xs >> A.popLabel
dropLabels (xs:|(ys:yss)) = mapM_ popStackTypeVal xs >> A.popLabel >> dropLabels (ys:|yss)

assertFalse :: FlowAlgebra i32 i64 f32 f64 mem m -> i32 -> m ()
assertFalse a i = assertConstI32 a i (WI32 0)


-- # Backward #

execBwdFlowAlgebra :: MonadWasmState i32 i64 f32 f64 mem m
                   => ( FuncIdx -> FuncType )
                   -> FlowBwdAlgebra i8 i32 i64 f32 f64 m
                   -> FlowEdge
                   -> m ()
execBwdFlowAlgebra fType a (PfeInstr instr) = AB.execBwdAlgebra (bwdInstrAlgebra a) instr
execBwdFlowAlgebra fType a (PfeCall fIdx) =
  do
    let FuncType xs ys = fType fIdx
    results <- reverse <$> mapM popStackTypeVal (reverse $ Vector.toList ys)
    globals <- A.getGlobals
    (globals', args) <- bwdCallExternal a fIdx globals results $ Vector.toList xs
    mapM_ A.pushStack args
    A.setGlobals globals'
execBwdFlowAlgebra fType a (PfeCallIndirect (FuncType xs ys)) =
  do
    results <- reverse <$> mapM popStackTypeVal (reverse $ Vector.toList ys)
    globals <- A.getGlobals
    (globals', args, fIdx) <- bwdCallIndirect a globals results $ Vector.toList xs
    mapM_ A.pushStack args
    A.pushStackI32 fIdx
    A.setGlobals globals'
execBwdFlowAlgebra fType a (PfeIfTrue kv) =
  do
    args <- mapM popStackTypeVal kv
    A.popLabel
    mapM_ A.pushStack (reverse args)
    A.pushStackI32 =<< AB.topI32 (bwdInstrAlgebra a) -- condition (True. /=0)
execBwdFlowAlgebra fType a (PfeIfFalse kv) =
  do
    args <- mapM popStackTypeVal kv
    A.popLabel
    mapM_ A.pushStack (reverse args)
    A.pushStackI32 =<< AB.topI32 (bwdInstrAlgebra a) -- condition (False. ==0)
execBwdFlowAlgebra fType a (PfeEnter kv) =
  do
    args <- mapM popStackTypeVal kv
    A.popLabel
    mapM_ A.pushStack (reverse args)
execBwdFlowAlgebra fType a (PfeBr kv dv isLoop) =
  do
    results <- mapM popStackTypeVal kv
    when isLoop A.popLabel
    undropLabels a dv
    mapM_ A.pushStack (reverse results)
execBwdFlowAlgebra fType a (PfeBrIf kv dv isLoop) =
  do
    results <- mapM popStackTypeVal kv
    when isLoop A.popLabel
    undropLabels a dv
    mapM_ A.pushStack (reverse results)
    A.pushStackI32 =<< AB.topI32 (bwdInstrAlgebra a) -- condition (True. /=0)
execBwdFlowAlgebra fType a PfeBrElse =
  A.pushStackI32 =<< AB.topI32 (bwdInstrAlgebra a) -- condition (False. ==0)
execBwdFlowAlgebra fType a (PfeBrTable tIdx kv dv isLoop) =
  do
    results <- mapM popStackTypeVal kv
    when isLoop A.popLabel
    undropLabels a dv
    mapM_ A.pushStack (reverse results)
    A.pushStackI32 =<< AB.topI32 (bwdInstrAlgebra a) -- label index
execBwdFlowAlgebra fType a (PfeMagic kv dv) =
  do
    mapM_ popStackTypeVal kv
    undropLabels a dv

undropLabels :: MonadWasmState i32 i64 f32 f64 mem m
             => FlowBwdAlgebra i8 i32 i64 f32 f64 m
             -> NonEmpty DroppedVals
             -> m ()
undropLabels a (xs:|[])       =
  A.pushLabel >> mapM_ (undropVal a) (reverse xs)
undropLabels a (xs:|(ys:yss)) =
  undropLabels a (ys:|yss) >> A.pushLabel >> mapM_ (undropVal a) (reverse xs)

undropVal :: MonadWasmState i32 i64 f32 f64 mem m
          => FlowBwdAlgebra i8 i32 i64 f32 f64 m
          -> ValType
          -> m ()
undropVal a v = AB.bottom (bwdInstrAlgebra a) v >>= A.pushStack

dropStackTypeVal :: MonadWasmState i32 i64 f32 f64 mem m => ValType -> m ()
dropStackTypeVal = void . popStackTypeVal

popStackTypeVal :: MonadWasmState i32 i64 f32 f64 mem m => ValType -> m (PVal i32 i64 f32 f64)
popStackTypeVal TI32 = VI32 <$> A.popStackI32
popStackTypeVal TI64 = VI64 <$> A.popStackI64
popStackTypeVal TF32 = VF32 <$> A.popStackF32
popStackTypeVal TF64 = VF64 <$> A.popStackF64
