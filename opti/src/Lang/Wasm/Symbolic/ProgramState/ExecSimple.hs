{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, ConstraintKinds,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses
#-}

-- | Symbolic execution for simple instructions
module Lang.Wasm.Symbolic.ProgramState.ExecSimple where

import Melude
-- Stdlib imports
import           Data.Word ( Word32 )
-- Extra stdlib imports
import           Control.Monad ( void )
import qualified Control.Monad.State as S
import           Control.Monad.State ( StateT (..), execStateT )
import           Control.Monad.Reader ( ReaderT (..) )
import           Control.Monad.Trans ( lift )
import           Control.Monad.Trans.Maybe ( MaybeT (..) )
-- Local imports
import qualified Lang.Wasm.Ast as Ast
import Lang.Wasm.Ast
  ( WI32 (..), WI64 (..), WF32, WF64, TI32, TI64, TF32, TF64, IUnop (..), IBinop (..)
  , FUnop (..), FBinop (..), ITestop (..), IRelop (..), FRelop (..), Cvtop (..)
  , PrmInstr (..), VarInstr (..), MemInstr (..), SimpleInstr (..), MemArg (..)
  , Sx (..), PVal (..)
  )
import qualified Lang.Wasm.Data as WD
import           Lang.Wasm.Markers ( TMem, TI33 )
import qualified Lang.Wasm.Symbolics as Y
import           Lang.Wasm.Symbolics ( MonadSymbolics, Symbolic, SymbolicVal (..) )
import qualified Lang.Wasm.Symbolic.General as SG
import           Lang.Wasm.Symbolic.ProgramState
  ( SymbolicProgramState (..), SymbolicLocalState (..)
  , sLocalState, sGlobals, sMem, sWorld, sIsTrapped, sActivation, sStack
  )
import           Lang.Wasm.Algebra ( SimpleAlgebra (..), MonadWasmState (..), execAlgebra )


-- | Symbolically executes a simple instruction. (Simple instructions don't
-- include control flow).
--
-- It produces a `Nothing` (in `MaybeT`) if the stack is malformed for the
-- provided instructions.
execSimpleInstr :: Y.MonadExtSymbolics env m
                => SymbolicProgramState env
                -> SimpleInstr
                -> MaybeT m (SymbolicProgramState env)
execSimpleInstr s instr = execStateT (execAlgebra symbolicAlgebra instr) s

type SymbolicAlgebra env =
  SimpleAlgebra
    (Symbolic env TI32)
    (Symbolic env TI64)
    (Symbolic env TF32)
    (Symbolic env TF64)

-- |
--
-- This is implemented as an algebra, as now the algebra execution deals with
-- the "plumbing" of popping and pushing type-correct arguments from/to the
-- stack.
symbolicAlgebra :: Y.MonadExtSymbolics env m
                => SymbolicAlgebra env (StateT (SymbolicProgramState env) (MaybeT m))
symbolicAlgebra =
  SimpleAlgebra {
    constI32 = Y.vI32Const
  , constI64 = Y.vI64Const
  , constF32 = Y.vF32Const
  , constF64 = Y.vF64Const
  
  , magicI32 = Y.vI32Unknown
  , magicI64 = Y.vI64Unknown
  , magicF32 = Y.vF32Unknown
  , magicF64 = Y.vF64Unknown

  , unopI32  = Y.vI32Unop
  , unopI64  = Y.vI64Unop
  , binopI32 = execBinopI32
  , binopI64 = execBinopI64

  , unopF32  = Y.vF32Unop
  , unopF64  = Y.vF64Unop
  , binopF32 = Y.vF32Binop
  , binopF64 = Y.vF64Binop

  , testopI32 = Y.vI32Bool <<=< Y.vI32Testop
  , testopI64 = Y.vI32Bool <<=< Y.vI64Testop

  , relopI32 = Y.vI32Bool <<<=< Y.vI32Relop
  , relopI64 = Y.vI32Bool <<<=< Y.vI64Relop
  , relopF32 = Y.vI32Bool <<<=< Y.vF32Relop
  , relopF64 = Y.vI32Bool <<<=< Y.vF64Relop

  , selectI32 = \t f c -> join (Y.vI32Select <$> Y.vBI32 c <*> pure t <*> pure f)
  , selectI64 = \t f c -> join (Y.vI64Select <$> Y.vBI32 c <*> pure t <*> pure f)
  , selectF32 = \t f c -> join (Y.vF32Select <$> Y.vBI32 c <*> pure t <*> pure f)
  , selectF64 = \t f c -> join (Y.vF64Select <$> Y.vBI32 c <*> pure t <*> pure f)

  , extend8toI32S   = Y.vI32Extend8S
  , extend16toI32S  = Y.vI32Extend16S
  , extend8toI64S   = Y.vI64Extend8S
  , extend16toI64S  = Y.vI64Extend16S
  , extend32toI64S  = Y.vI64Extend32S
  , extractI64toI32 = Y.vI32WrapI64
  , extendI32toI64  = Y.vI64ExtendI32

  , reinterpretF32toI32 = Y.vI32ReinterpretF32
  , reinterpretF64toI64 = Y.vI64ReinterpretF64
  , reinterpretI32toF32 = Y.vF32ReinterpretI32
  , reinterpretI64toF64 = Y.vF64ReinterpretI64

  , convertF32toF64 = Y.vF64PromoteF32
  , convertF64toF32 = Y.vF32DemoteF64
  , convertI32toF32 = Y.vF32ConvertI32
  , convertI32toF64 = Y.vF64ConvertI32
  , convertI64toF32 = Y.vF32ConvertI64
  , convertI64toF64 = Y.vF64ConvertI64

  , truncF32toI32 = Y.vI32TruncF32
  , truncF64toI32 = Y.vI32TruncF64
  , truncF32toI64 = Y.vI64TruncF32
  , truncF64toI64 = Y.vI64TruncF64
  }

execBinopI32 :: Y.MonadExtSymbolics env m
             => IBinop WI32
             -> Symbolic env TI32
             -> Symbolic env TI32
             -> StateT (SymbolicProgramState env) (MaybeT m) (Symbolic env TI32)
execBinopI32 op@(IBinDiv sx) a b = trapDivRemI32 sx a b >> Y.vI32Binop op a b
execBinopI32 op@(IBinRem sx) a b = trapDivRemI32 sx a b >> Y.vI32Binop op a b
execBinopI32 op a b = Y.vI32Binop op a b

execBinopI64 :: Y.MonadExtSymbolics env m
             => IBinop WI64
             -> Symbolic env TI64
             -> Symbolic env TI64
             -> StateT (SymbolicProgramState env) (MaybeT m) (Symbolic env TI64)
execBinopI64 op@(IBinDiv sx) a b = trapDivRemI64 sx a b >> Y.vI64Binop op a b
execBinopI64 op@(IBinRem sx) a b = trapDivRemI64 sx a b >> Y.vI64Binop op a b
execBinopI64 op a b = Y.vI64Binop op a b

-- ## Helpers for Symbolic Operations

-- | Integer division and remainder operators may trap. Either by division by
-- zero, or by signed division of @-2^31/-1 = 2^31@, which is unrepresentable.
trapDivRemI32 :: Y.MonadExtSymbolics env m
              => Sx
              -> Symbolic env TI32
              -> Symbolic env TI32
              -> StateT (SymbolicProgramState env) (MaybeT m) ()
trapDivRemI32 sx a b =
  do
    isDiv0 <- Y.vI32Eqz b
    if sx == U then
      SG.trapIf isDiv0
    else -- Also trap on -2^31/-1, as 2^31 is unrepresentable
      do
        isEqMin  <- Y.vI32Eq a =<< Y.vI32Const (WI32 0x80000000) -- -2^31
        isEqNeg1 <- Y.vI32Eq b =<< Y.vI32Const (WI32 0xFFFFFFFF) -- -1
        isOverflow <- Y.vBAnd isEqMin isEqNeg1
        SG.trapIf =<< Y.vBOr isOverflow isDiv0

-- | Integer division and remainder operators may trap. Either by division by
-- zero, or by signed division of @-2^63/-1 = 2^63@, which is unrepresentable.
trapDivRemI64 :: Y.MonadExtSymbolics env m
              => Sx
              -> Symbolic env TI64
              -> Symbolic env TI64
              -> StateT (SymbolicProgramState env) (MaybeT m) ()
trapDivRemI64 sx a b =
  do
    isDiv0 <- Y.vI64Eqz b
    if sx == U then
      SG.trapIf isDiv0
    else -- Also trap on -2^31/-1, as 2^31 is unrepresentable
      do
        isEqMin  <- Y.vI64Eq a =<< Y.vI64Const (WI64 0x8000000000000000) -- -2^63
        isEqNeg1 <- Y.vI64Eq b =<< Y.vI64Const (WI64 0xFFFFFFFFFFFFFFFF) -- -1
        isOverflow <- Y.vBAnd isEqMin isEqNeg1
        SG.trapIf =<< Y.vBOr isOverflow isDiv0
