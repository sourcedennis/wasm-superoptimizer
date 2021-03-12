{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables, ConstraintKinds,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses
#-}

-- | Symbolic execution for simple instructions
module Lang.Wasm.Symbolic.Configuration.ExecSimple where

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
-- Local 
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
import qualified Lang.Wasm.Symbolic.Configuration as C
import           Lang.Wasm.Symbolic.Configuration
  ( Configuration, ConfigurationCore
  )
import           Lang.Wasm.Algebra ( SimpleAlgebra (..), MonadWasmState (..), execAlgebra )


-- | Symbolically executes a simple instruction. (Simple instructions don't
-- include control flow).
--
-- It produces a `Nothing` (in `MaybeT`) if the stack is malformed for the
-- provided instructions.
execSimpleInstr :: Configuration env
                -> SimpleInstr
                -> Maybe (Configuration env)
execSimpleInstr s instr =
  let res  = execAlgebra configurationAlgebra instr
      res2 = execStateT res (s^. C.cfgCore)
      (core, sym) = Y.runSymbolicsState (runMaybeT res2) (s ^. C.cfgSymbolics)
  in C.Configuration <$> core <*> pure sym

type SymbolicAlgebra env =
  SimpleAlgebra
    (Symbolic env TI32)
    (Symbolic env TI64)
    ()
    ()

-- |
--
-- This is implemented as an algebra, as now the algebra execution deals with
-- the "plumbing" of popping and pushing type-correct arguments from/to the
-- stack.
configurationAlgebra :: Y.MonadSymbolics env m
                     => SymbolicAlgebra env (StateT (ConfigurationCore env) (MaybeT m))
configurationAlgebra =
  SimpleAlgebra {
    constI32 = Y.vI32Const
  , constI64 = Y.vI64Const
  , constF32 = const $ return ()
  , constF64 = const $ return ()
  
  , magicI32 = Y.vI32Unknown
  , magicI64 = Y.vI64Unknown
  , magicF32 = return ()
  , magicF64 = return ()

  , unopI32  = Y.vI32Unop
  , unopI64  = Y.vI64Unop
  , binopI32 = execBinopI32
  , binopI64 = execBinopI64

  , unopF32  = const $ const $ return ()
  , unopF64  = const $ const $ return ()
  , binopF32 = const $ const $ const $ return ()
  , binopF64 = const $ const $ const $ return ()

  , testopI32 = Y.vI32Bool <<=< Y.vI32Testop
  , testopI64 = Y.vI32Bool <<=< Y.vI64Testop

  , relopI32 = Y.vI32Bool <<<=< Y.vI32Relop
  , relopI64 = Y.vI32Bool <<<=< Y.vI64Relop
  , relopF32 = \_ _ _ -> Y.vI32Bool =<< Y.vBoolUnknown
  , relopF64 = \_ _ _ -> Y.vI32Bool =<< Y.vBoolUnknown

  , selectI32 = \t f c -> join (Y.vI32Select <$> Y.vBI32 c <*> pure t <*> pure f)
  , selectI64 = \t f c -> join (Y.vI64Select <$> Y.vBI32 c <*> pure t <*> pure f)
  , selectF32 = \t f c -> return ()
  , selectF64 = \t f c -> return ()

  , extend8toI32S   = Y.vI32Extend8S
  , extend16toI32S  = Y.vI32Extend16S
  , extend8toI64S   = Y.vI64Extend8S
  , extend16toI64S  = Y.vI64Extend16S
  , extend32toI64S  = Y.vI64Extend32S
  , extractI64toI32 = Y.vI32WrapI64
  , extendI32toI64  = Y.vI64ExtendI32

  , reinterpretF32toI32 = const Y.vI32Unknown
  , reinterpretF64toI64 = const Y.vI64Unknown
  , reinterpretI32toF32 = const $ return ()
  , reinterpretI64toF64 = const $ return ()

  , convertF32toF64 = const $ return ()
  , convertF64toF32 = const $ return ()
  , convertI32toF32 = \_ _ -> return ()
  , convertI32toF64 = \_ _ -> return ()
  , convertI64toF32 = \_ _ -> return ()
  , convertI64toF64 = \_ _ -> return ()

  , truncF32toI32 = \_ _ _ -> Y.vI32Unknown
  , truncF64toI32 = \_ _ _ -> Y.vI32Unknown
  , truncF32toI64 = \_ _ _ -> Y.vI64Unknown
  , truncF64toI64 = \_ _ _ -> Y.vI64Unknown
  }

execBinopI32 :: Y.MonadSymbolics env m
             => IBinop WI32
             -> Symbolic env TI32
             -> Symbolic env TI32
             -> StateT (ConfigurationCore env) (MaybeT m) (Symbolic env TI32)
execBinopI32 op@(IBinDiv sx) a b = trapDivRemI32 sx a b >> Y.vI32Binop op a b
execBinopI32 op@(IBinRem sx) a b = trapDivRemI32 sx a b >> Y.vI32Binop op a b
execBinopI32 op a b = Y.vI32Binop op a b

execBinopI64 :: Y.MonadSymbolics env m
             => IBinop WI64
             -> Symbolic env TI64
             -> Symbolic env TI64
             -> StateT (ConfigurationCore env) (MaybeT m) (Symbolic env TI64)
execBinopI64 op@(IBinDiv sx) a b = trapDivRemI64 sx a b >> Y.vI64Binop op a b
execBinopI64 op@(IBinRem sx) a b = trapDivRemI64 sx a b >> Y.vI64Binop op a b
execBinopI64 op a b = Y.vI64Binop op a b


-- ## Helpers for Symbolic Operations

-- | Integer division and remainder operators may trap. Either by division by
-- zero, or by signed division of @-2^31/-1 = 2^31@, which is unrepresentable.
trapDivRemI32 :: Y.MonadSymbolics env m
              => Sx
              -> Symbolic env TI32
              -> Symbolic env TI32
              -> StateT (ConfigurationCore env) (MaybeT m) ()
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
trapDivRemI64 :: Y.MonadSymbolics env m
              => Sx
              -> Symbolic env TI64
              -> Symbolic env TI64
              -> StateT (ConfigurationCore env) (MaybeT m) ()
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
