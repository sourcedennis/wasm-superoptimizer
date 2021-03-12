{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, UndecidableInstances,
             RankNTypes, Rank2Types, TemplateHaskell, FunctionalDependencies,
             GeneralizedNewtypeDeriving, KindSignatures
#-}

-- | Deep embedding AST for formulas obtained from WebAssembly programs, which
-- stores constraints placed upon symbolic values.
--
-- Symbolic values are either atoms, or composed of other symbolic values with
-- operators. Each symbolic value is referenced by a unique identifier, which
-- facilitates reuse of subtrees. For example:
-- @c=(a+b)*(a+b)@ is represented as @d=a+b@ and @c=d*d@.
-- So, conceptually, expressions are represented as DAGs.
--
-- Note that it does not /actually/ constraint the values, it just /describes/
-- the constraints (which are boolean values). Assertions can utilize these
-- values to actually constrain other values. (See also `Environment` and
-- `ExtEnvironment`)
--
-- TODO: This file got a bit messy, as it unifies over symbolic states and
--       configurations. Maybe separate these?
-- TODO: Extract `SymbolicsCombiner`
module Lang.Wasm.Symbolics
  ( -- * Type markers
    TWorld
  , TFunc
  , TMemSize
    -- * Symbolics Instance
  , Symbolics (..)
  , ExtSymbolics (..)
  , Environment
  , ExtEnvironment
    -- * Symbolic Values
  , Symbolic (..)
  , SymbolicVal (..)
  , ExtSymbolicVal (..)
  , SymbolicPVal (..)
    -- * Internals
  , ComposedVal (..)
  , AnyComposedVal (..)
  , ExtAnyComposedVal (..)
    -- ** Lenses
  , _ACVBool, _ACVI32, _ACVI33, _ACVI64, _ACVMem
  , _ECVBase, _ECVWorld, _ECVFunc, _ECVF32, _ECVF64
    -- * Helper Monads
  , MonadSymbolics (..)
  , MonadExtSymbolics (..)
  , SymbolicsStateT (..)
  , ExtSymbolicsStateT (..)
  , SymbolicsState (..)
  , ExtSymbolicsState (..)
    -- * Constructions
  , empty
  , emptyExt
    -- * Constructors
  , vI32Const, vI64Const, vF32Const, vF64Const
  , vI32Unknown, vI64Unknown, vF32Unknown, vF64Unknown, vUnknown
  , vConstZero
  , vUnknownExt, vConstZeroExt
    -- * I32
    -- ** Unary Operators
  , vI32Unop
  , vI32Clz, vI32Ctz, vI32Popcnt
    -- ** Binary Operators
  , vI32Binop
  , vI32Add, vI32Sub, vI32Mul, vI32And, vI32Or, vI32Xor, vI32Shl, vI32Rotl
  , vI32Rotr, vI32Div, vI32Rem, vI32Shr
    -- ** Test Operators
  , vI32Testop
  , vI32Eqz
    -- ** Relative Operators
  , vI32Relop
  , vI32Eq, vI32Ne, vI32Lt, vI32Le, vI32Gt, vI32Ge
    -- * I64
    -- ** Unary Operators
  , vI64Unop
  , vI64Clz, vI64Ctz, vI64Popcnt
    -- ** Binary Operators
  , vI64Binop
  , vI64Add, vI64Sub, vI64Mul, vI64And, vI64Or, vI64Xor, vI64Shl, vI64Rotl
  , vI64Rotr, vI64Div, vI64Rem, vI64Shr
    -- ** Test Operators
  , vI64Testop
  , vI64Eqz
    -- ** Relative Operators
  , vI64Relop
  , vI64Eq, vI64Ne, vI64Lt, vI64Le, vI64Gt, vI64Ge
    -- * F32
    -- ** Unary Operators
  , vF32Unop
  , vF32Abs, vF32Neg, vF32Sqrt, vF32Ceil, vF32Floor, vF32Trunc, vF32Nearest
    -- ** Binary Operators
  , vF32Binop
  , vF32Add, vF32Sub, vF32Mul, vF32Div, vF32Min, vF32Max, vF32Copysign
    -- ** Relative Operators
  , vF32Relop
  , vF32Eq, vF32Ne, vF32Lt, vF32Gt, vF32Le, vF32Ge
    -- * F64
    -- ** Unary Operators
  , vF64Unop
  , vF64Abs, vF64Neg, vF64Sqrt, vF64Ceil, vF64Floor, vF64Trunc, vF64Nearest
    -- ** Binary Operators
  , vF64Binop
  , vF64Add, vF64Sub, vF64Mul, vF64Div, vF64Min, vF64Max, vF64Copysign
    -- ** Relative Operators
  , vF64Relop
  , vF64Eq, vF64Ne, vF64Lt, vF64Gt, vF64Le, vF64Ge
    -- * Parametric Operators
  , vI32Select, vI64Select, vF32Select, vF64Select, vI33Select
  , vBoolSelect, vMemSelect, vWorldSelect
    -- * Memory Operators
  , vI32Load, vI64Load, vF32Load, vF64Load, vI32Load8, vI64Load8, vI32Load16
  , vI64Load16, vI64Load32
  , vI32Store, vI64Store, vF32Store, vF64Store, vI32Store8, vI64Store8, vI32Store16
  , vI64Store16, vI64Store32
    -- * Conversion Operators
  , vI32Extend8S, vI32Extend16S, vI64Extend8S, vI64Extend16S, vI64Extend32S
  , vI32WrapI64, vI64ExtendI32, vI32TruncF32, vI32TruncF64, vI64TruncF32
  , vI64TruncF64, vF32DemoteF64, vF64PromoteF32, vF32ConvertI32
  , vF32ConvertI64, vF64ConvertI32, vF64ConvertI64, vI32ReinterpretF32
  , vI64ReinterpretF64, vF32ReinterpretI32, vF64ReinterpretI64
    -- * Extra Operators
  , vWorldUnknown, vFuncUnknown, vMemUnknown, vBoolUnknown
  , vBoolConst, vI33Unknown, vI33Const, vI33MulPage, vI33Address, vI33Lt
    -- * Extra Operators (Booleans)
  , vBAnd, vBOr, vBNot, vBI32, vI32Bool
    -- * Side-effect Operators
  , vMemGrowWorld, vMemGrowBool, vFuncCallWorld, vFuncCallMem
  , vFuncCallResultI32, vFuncCallResultI64, vFuncCallResultF32
  , vFuncCallResultF64
    -- * Combiner
  , SymbolicsCombiner
  , runCombiner
  , combineBool, combineI32, combineI33, combineI64, combineMem, combineVal
    -- # Helpers
  , vI32Zero, vI64Zero, vF32Zero, vF64Zero
  , liftExt
  , size
  , isEqSymbolic
  , runSymbolicsStateT
  , runExtSymbolicsStateT
  , runSymbolicsState
  , runExtSymbolicsState
  , getSymbolics
  , getExtSymbolics
  , getsSymbolics
  , getsExtSymbolics
  ) where

import Melude
-- Stdlib imports
import Data.Word ( Word32 )
import Numeric.Natural ( Natural )
import Numeric ( showHex )
-- Extra stdlib imports
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
import           Control.Monad.State ( MonadState, StateT )
import           Control.Monad.Trans
-- External library 
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
-- Local library imports
import qualified Lang.Wasm.Data as WD
import Lang.Wasm.Ast
  ( TI32, TI64, TF32, TF64, Sx (..), WI32 (..), WI64 (..), WF32 (..), WF64 (..)
  , Instr (..), IUnop (..), IBinop (..), ITestop (..), FUnop (..), FBinop (..)
  , IRelop (..), FRelop (..), Cvtop (..), SimpleInstr (SCvtop), ValType (..)
  , Sat (..), PVal (..)
  )
-- Local imports
import Lang.Wasm.Markers ( TI33, TMem )
import qualified Lang.Wasm.Numerics as Num


-- # Markers

data TWorld

data TFunc

-- | The number of /memory pages/
type TMemSize = TI32


-- # Symbolics Instance

-- | A collection of symbolic values with their relations
--
-- `env` is a phantom parameter, which may be used to associate symbolic value
-- with their environment.
newtype Symbolics env = Symbolics (IdList (AnyComposedVal env))

-- | A collection of symbolic values with their relations, which also includes
-- (programmatic) functions and worlds.
--
-- `env` is a phantom parameter, which may be used to associate symbolic value
-- with their environment.
newtype ExtSymbolics env = ExtSymbolics (IdList (ExtAnyComposedVal env))

-- | An environment constrains an state assignment.
type Environment env = (Symbolics env, Symbolic env Bool)

-- | An environment constrains an state assignment.
type ExtEnvironment env = (ExtSymbolics env, Symbolic env Bool)


-- # Symbolic Values

-- | A `Symbolic` value. Note that this is only defined within the context of
-- a `Symbolics` or `ExtSymbolics` instance.
--
-- `env` is a phantom parameter, which may be used to associate symbolic value
-- with their environment.
--
-- Symbolic instances:
-- * TI32   - Primitive value. 32-bit bitvector
-- * TI33   - A memory address
-- * TI64   - Primitive value. 64-bit bitvector
-- * TF32   - Primitive value. 32-bit float
-- * TF64   - Primitive value. 64-bit float
-- * TMem   - Infinite WebAssembly memory block
-- * TWorld - Opaque value that models the outside world (for side-effects)
data Symbolic env a where
  -- Index into the `Symbolics` `IntMap`
  Symbolic  :: Int -> Symbolic env a

  SymConstI32   :: WI32 -> Symbolic env TI32
  SymConstI33   :: Natural -> Symbolic env TI33
  SymConstI64   :: WI64 -> Symbolic env TI64
  SymConstF32   :: WF32 -> Symbolic env TF32
  SymConstF64   :: WF64 -> Symbolic env TF64
  SymConstBool  :: Bool -> Symbolic env Bool

-- | Any symbolic /primitive/ value
type SymbolicVal env = SymbolicPVal env () ()
    
type ExtSymbolicVal env = SymbolicPVal env (Symbolic env TF32) (Symbolic env TF64)

type SymbolicPVal env f32 f64 =
  PVal
    (Symbolic env TI32)
    (Symbolic env TI64)
    f32
    f64

-- | Values that are composed of symbolic values.
--
-- These instructions are defined at:
-- https://webassembly.github.io/spec/core/syntax/instructions.html
--
-- This is a GADT to facilitate selective pattern matching.
--
-- `env` is a phantom parameter, which may be used to associate symbolic value
-- with their environment.
data ComposedVal env a where
  VI32Unknown  :: ComposedVal env TI32
  VI64Unknown  :: ComposedVal env TI64
  VF32Unknown  :: ComposedVal env TF32
  VF64Unknown  :: ComposedVal env TF64
  VMemUnknown  :: ComposedVal env TMem

  -- # I32
  VI32Unop   :: IUnop TI32  -> Symbolic env TI32 -> ComposedVal env TI32
  VI32Binop  :: IBinop TI32 -> Symbolic env TI32 -> Symbolic env TI32 -> ComposedVal env TI32
  VI32Relop  :: IRelop TI32 -> Symbolic env TI32 -> Symbolic env TI32 -> ComposedVal env Bool
  
  -- # I64
  VI64Unop   :: IUnop TI64  -> Symbolic env TI64 -> ComposedVal env TI64
  VI64Binop  :: IBinop TI64 -> Symbolic env TI64 -> Symbolic env TI64 -> ComposedVal env TI64
  VI64Relop  :: IRelop TI64 -> Symbolic env TI64 -> Symbolic env TI64 -> ComposedVal env Bool
  
  -- # F32
  VF32Unop   :: FUnop TF32 -> Symbolic env TF32 -> ComposedVal env TF32
  VF32Binop  :: FBinop TF32 -> Symbolic env TF32 -> Symbolic env TF32 -> ComposedVal env TF32
  VF32Relop  :: FRelop TF32 -> Symbolic env TF32 -> Symbolic env TF32 -> ComposedVal env Bool
  
  -- # F64
  VF64Unop   :: FUnop TF64 -> Symbolic env TF64 -> ComposedVal env TF64
  VF64Binop  :: FBinop TF64 -> Symbolic env TF64 -> Symbolic env TF64 -> ComposedVal env TF64
  VF64Relop  :: FRelop TF64 -> Symbolic env TF64 -> Symbolic env TF64 -> ComposedVal env Bool

  -- # Parametric Instructions
  VSelect  :: Symbolic env Bool -> Symbolic env a -> Symbolic env a -> ComposedVal env a -- a ? b : c

  -- # Conversion
  VI32Extend8S        :: Symbolic env TI32 -> ComposedVal env TI32
  VI32Extend16S       :: Symbolic env TI32 -> ComposedVal env TI32
  VI64Extend8S        :: Symbolic env TI64 -> ComposedVal env TI64
  VI64Extend16S       :: Symbolic env TI64 -> ComposedVal env TI64
  VI64Extend32S       :: Symbolic env TI64 -> ComposedVal env TI64
  VI32WrapI64         :: Symbolic env TI64 -> ComposedVal env TI32
  VI64ExtendI32       :: Sx -> Symbolic env TI32 -> ComposedVal env TI64
  VI32TruncF32        :: Sat -> Sx -> Symbolic env TF32 -> ComposedVal env TI32
  VI32TruncF64        :: Sat -> Sx -> Symbolic env TF64 -> ComposedVal env TI32
  VI64TruncF32        :: Sat -> Sx -> Symbolic env TF32 -> ComposedVal env TI64
  VI64TruncF64        :: Sat -> Sx -> Symbolic env TF64 -> ComposedVal env TI64
  VF32DemoteF64       :: Symbolic env TF64 -> ComposedVal env TF32
  VF64PromoteF32      :: Symbolic env TF32 -> ComposedVal env TF64
  VF32ConvertI32      :: Sx -> Symbolic env TI32 -> ComposedVal env TF32
  VF32ConvertI64      :: Sx -> Symbolic env TI64 -> ComposedVal env TF32
  VF64ConvertI32      :: Sx -> Symbolic env TI32 -> ComposedVal env TF64
  VF64ConvertI64      :: Sx -> Symbolic env TI64 -> ComposedVal env TF64
  VI32ReinterpretF32  :: Symbolic env TF32 -> ComposedVal env TI32
  VI64ReinterpretF64  :: Symbolic env TF64 -> ComposedVal env TI64
  VF32ReinterpretI32  :: Symbolic env TI32 -> ComposedVal env TF32
  VF64ReinterpretI64  :: Symbolic env TI64 -> ComposedVal env TF64

  -- # Memory Operators
  VI32Load    :: Symbolic env TMem -> Symbolic env TI33 -> ComposedVal env TI32
  VI64Load    :: Symbolic env TMem -> Symbolic env TI33 -> ComposedVal env TI64
  VF32Load    :: Symbolic env TMem -> Symbolic env TI33 -> ComposedVal env TF32
  VF64Load    :: Symbolic env TMem -> Symbolic env TI33 -> ComposedVal env TF64
  VI32Load8   :: Sx -> Symbolic env TMem -> Symbolic env TI33 -> ComposedVal env TI32
  VI64Load8   :: Sx -> Symbolic env TMem -> Symbolic env TI33 -> ComposedVal env TI64
  VI32Load16  :: Sx -> Symbolic env TMem -> Symbolic env TI33 -> ComposedVal env TI32
  VI64Load16  :: Sx -> Symbolic env TMem -> Symbolic env TI33 -> ComposedVal env TI64
  VI64Load32  :: Sx -> Symbolic env TMem -> Symbolic env TI33 -> ComposedVal env TI64

  VI32Store    :: Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI32 -> ComposedVal env TMem
  VI64Store    :: Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI64 -> ComposedVal env TMem
  VF32Store    :: Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TF32 -> ComposedVal env TMem
  VF64Store    :: Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TF64 -> ComposedVal env TMem
  VI32Store8   :: Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI32 -> ComposedVal env TMem
  VI64Store8   :: Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI64 -> ComposedVal env TMem
  VI32Store16  :: Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI32 -> ComposedVal env TMem
  VI64Store16  :: Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI64 -> ComposedVal env TMem
  VI64Store32  :: Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI64 -> ComposedVal env TMem
  

  -- # Extra (not from spec, but necessary for complete state representation)
  VI33Unknown  :: ComposedVal env TI33
  -- VI33Const    :: Natural -> ComposedVal env TI33
  VI33Address  :: Symbolic env TI32 -> Word32 -> ComposedVal env TI33
  VI33Lt       :: Symbolic env TI33 -> Symbolic env TI33 -> ComposedVal env Bool
  VI33MulPage  :: Symbolic env TI32 -> ComposedVal env TI33
  
  VBoolUnknown :: ComposedVal env Bool
  VBAnd        :: Symbolic env Bool -> Symbolic env Bool -> ComposedVal env Bool
  VBOr         :: Symbolic env Bool -> Symbolic env Bool -> ComposedVal env Bool
  VBImplies    :: Symbolic env Bool -> Symbolic env Bool -> ComposedVal env Bool
  VBNot        :: Symbolic env Bool -> ComposedVal env Bool

  VBI32     :: Symbolic env TI32 -> ComposedVal env Bool
  VI32Bool  :: Symbolic env Bool -> ComposedVal env TI32

  -- # Side effects (only applicable to `ExtSymbolics` - enforced by env marker)
  VWorldUnknown  :: ComposedVal env TWorld
  -- | Note that memory (`TMem`) is infinite. However, the symbolic executor
  -- ensures bounds checks. The success of memory growth, however, depends on
  -- the runtime system (e.g., failure upon out-of-memory).
  VMemGrowWorld  :: Symbolic env TWorld -> Symbolic env TMemSize -> ComposedVal env TWorld
  VMemGrowBool   :: Symbolic env TWorld -> Symbolic env TMemSize -> ComposedVal env Bool

  -- | An unknown function, which is assumed to have side-effects.
  VFuncUnknown   :: [ValType] -> [ValType] -> ComposedVal env TFunc
  -- Functions are called a bit clumsily. A function has multiple effects;
  -- namely its change upon the world, memory, and return values on the stack.
  -- As each of the results much be individually addressable, they are
  -- represented by their own handle.
  VFuncCallWorld    :: Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> ComposedVal env TWorld
  VFuncCallMem      :: Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> ComposedVal env TMem
  VFuncCallMemSize  :: Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> ComposedVal env TMemSize
  -- No type checking is performed on the results.
  VFuncCallResultI32  :: Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> Natural -> ComposedVal env TI32
  VFuncCallResultI64  :: Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> Natural -> ComposedVal env TI64
  VFuncCallResultF32  :: Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> Natural -> ComposedVal env TF32
  VFuncCallResultF64  :: Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> Natural -> ComposedVal env TF64

data AnyComposedVal env
  = ACVBool (ComposedVal env Bool)
  | ACVI32  (ComposedVal env TI32)
  | ACVI33  (ComposedVal env TI33)
  | ACVI64  (ComposedVal env TI64)
  | ACVMem  (ComposedVal env TMem)

data ExtAnyComposedVal env
  = ECVBase  (AnyComposedVal env)
  | ECVWorld (ComposedVal env TWorld)
  | ECVFunc  (ComposedVal env TFunc)
    -- Currently, keep floats in the extension, as these cannot be fully solved.
  | ECVF32  (ComposedVal env TF32)
  | ECVF64  (ComposedVal env TF64)

makePrisms ''AnyComposedVal
makePrisms ''ExtAnyComposedVal


-- # Construction

empty :: Symbolics env
empty = Symbolics IdList.empty

emptyExt :: ExtSymbolics env
emptyExt = ExtSymbolics IdList.empty


-- # Constructors

vI32Const :: MonadSymbolics env m => WI32 -> m (Symbolic env TI32)
vI32Const = return . SymConstI32 -- freshValI32 . VI32Const

vI64Const :: MonadSymbolics env m => WI64 -> m (Symbolic env TI64)
vI64Const = return . SymConstI64 -- freshValI64 . VI64Const

vF32Const :: MonadExtSymbolics env m => WF32 -> m (Symbolic env TF32)
vF32Const = return . SymConstF32 -- freshValF32 . VF32Const

vF64Const :: MonadExtSymbolics env m => WF64 -> m (Symbolic env TF64)
vF64Const = return . SymConstF64 -- freshValF64 . VF64Const

vI32Unknown :: MonadSymbolics env m => m (Symbolic env TI32)
vI32Unknown = freshValI32 VI32Unknown

vI64Unknown :: MonadSymbolics env m => m (Symbolic env TI64)
vI64Unknown = freshValI64 VI64Unknown

vF32Unknown :: MonadExtSymbolics env m => m (Symbolic env TF32)
vF32Unknown = freshValF32 VF32Unknown

vF64Unknown :: MonadExtSymbolics env m => m (Symbolic env TF64)
vF64Unknown = freshValF64 VF64Unknown

vUnknown :: MonadSymbolics env m => ValType -> m (SymbolicVal env)
vUnknown TI32 = VI32 <$> vI32Unknown
vUnknown TI64 = VI64 <$> vI64Unknown
vUnknown TF32 = return $ VF32 ()
vUnknown TF64 = return $ VF64 ()

vConstZero :: MonadSymbolics env m => ValType -> m (SymbolicVal env)
vConstZero TI32 = VI32 <$> vI32Const (WI32 0)
vConstZero TI64 = VI64 <$> vI64Const (WI64 0)
vConstZero TF32 = return $ VF32 ()
vConstZero TF64 = return $ VF64 ()

vUnknownExt :: MonadExtSymbolics env m => ValType -> m (ExtSymbolicVal env)
vUnknownExt TI32 = VI32 <$> vI32Unknown
vUnknownExt TI64 = VI64 <$> vI64Unknown
vUnknownExt TF32 = VF32 <$> vF32Unknown
vUnknownExt TF64 = VF64 <$> vF64Unknown

vConstZeroExt :: MonadExtSymbolics env m => ValType -> m (ExtSymbolicVal env)
vConstZeroExt TI32 = VI32 <$> vI32Const (WI32 0)
vConstZeroExt TI64 = VI64 <$> vI64Const (WI64 0)
vConstZeroExt TF32 = VF32 <$> vF32Const (WF32 0)
vConstZeroExt TF64 = VF64 <$> vF64Const (WF64 0)


-- # I32 iunops

vI32Unop :: MonadSymbolics env m => IUnop TI32 -> Symbolic env TI32 -> m (Symbolic env TI32)
vI32Unop IUnClz    (SymConstI32 a) = return $ SymConstI32 $ Num.i32clz    a
vI32Unop IUnCtz    (SymConstI32 a) = return $ SymConstI32 $ Num.i32ctz    a
vI32Unop IUnPopcnt (SymConstI32 a) = return $ SymConstI32 $ Num.i32popcnt a
vI32Unop op a = freshValI32 $ VI32Unop op a

vI32Clz, vI32Ctz, vI32Popcnt :: MonadSymbolics env m => Symbolic env TI32 -> m (Symbolic env TI32)
vI32Clz    = vI32Unop IUnClz    -- count leading zeroes
vI32Ctz    = vI32Unop IUnCtz    -- count trailing zeroes
vI32Popcnt = vI32Unop IUnPopcnt -- count 1 bits


-- # I32 ibinop

vI32Binop :: MonadSymbolics env m => IBinop TI32 -> Symbolic env TI32 -> Symbolic env TI32 -> m (Symbolic env TI32)
vI32Binop IBinAdd  (SymConstI32 a) (SymConstI32 b) = return $ SymConstI32 $ Num.i32add  a b
vI32Binop IBinSub  (SymConstI32 a) (SymConstI32 b) = return $ SymConstI32 $ Num.i32sub  a b
vI32Binop IBinMul  (SymConstI32 a) (SymConstI32 b) = return $ SymConstI32 $ Num.i32mul  a b
vI32Binop IBinAnd  (SymConstI32 a) (SymConstI32 b) = return $ SymConstI32 $ Num.i32and  a b
vI32Binop IBinOr   (SymConstI32 a) (SymConstI32 b) = return $ SymConstI32 $ Num.i32or   a b
vI32Binop IBinXor  (SymConstI32 a) (SymConstI32 b) = return $ SymConstI32 $ Num.i32xor  a b
vI32Binop IBinShl  (SymConstI32 a) (SymConstI32 b) = return $ SymConstI32 $ Num.i32shl  a b
vI32Binop IBinRotl (SymConstI32 a) (SymConstI32 b) = return $ SymConstI32 $ Num.i32rotl a b
vI32Binop IBinRotr (SymConstI32 a) (SymConstI32 b) = return $ SymConstI32 $ Num.i32rotr a b

-- Undefined values evaluate to 0. Externally, a trapping condition must be enforced.
-- As the logic cannot "crash", it must evaluate to some arbitrary value.
vI32Binop op@(IBinDiv sx) a (SymConstI32 (WI32 0)) =
  return $ SymConstI32 $ WI32 0
vI32Binop op@(IBinDiv sx) (SymConstI32 a) (SymConstI32 b) =
  return $ SymConstI32 $ fromMaybe (WI32 0) $ Num.i32div sx a b

vI32Binop op@(IBinRem sx) a (SymConstI32 (WI32 0)) =
  return $ SymConstI32 $ WI32 0
vI32Binop op@(IBinRem sx) (SymConstI32 a) (SymConstI32 b) =
  return $ SymConstI32 $ fromMaybe (WI32 0) $ Num.i32rem sx a b

vI32Binop (IBinShr sx) (SymConstI32 a) (SymConstI32 b) =
  return $ SymConstI32 $ Num.i32shr sx a b

vI32Binop op a b = freshValI32 $ VI32Binop op a b


vI32Add, vI32Sub, vI32Mul, vI32And, vI32Or, vI32Xor, vI32Shl, vI32Rotl, vI32Rotr
  :: MonadSymbolics env m => Symbolic env TI32 -> Symbolic env TI32 -> m (Symbolic env TI32)
vI32Add  = vI32Binop IBinAdd
vI32Sub  = vI32Binop IBinSub
vI32Mul  = vI32Binop IBinMul
vI32And  = vI32Binop IBinAnd
vI32Or   = vI32Binop IBinOr
vI32Xor  = vI32Binop IBinXor
vI32Shl  = vI32Binop IBinShl
vI32Rotl = vI32Binop IBinRotl
vI32Rotr = vI32Binop IBinRotr

vI32Div, vI32Rem, vI32Shr
  :: MonadSymbolics env m => Sx -> Symbolic env TI32 -> Symbolic env TI32 -> m (Symbolic env TI32)
vI32Div = vI32Binop . IBinDiv
vI32Rem = vI32Binop . IBinRem
vI32Shr = vI32Binop . IBinShr


-- # I32 itestop

vI32Testop :: MonadSymbolics env m => ITestop TI32 -> Symbolic env TI32 -> m (Symbolic env Bool)
vI32Testop ITestEqz (SymConstI32 a) = return $ SymConstBool $ Num.i32eqz a
vI32Testop ITestEqz a = vI32Eq a =<< vI32Zero

vI32Eqz :: MonadSymbolics env m => Symbolic env TI32 -> m (Symbolic env Bool)
vI32Eqz = vI32Testop ITestEqz


-- # I32 irelop

vI32Relop :: MonadSymbolics env m => IRelop TI32 -> Symbolic env TI32 -> Symbolic env TI32 -> m (Symbolic env Bool)
-- eq
vI32Relop IRelEq (SymConstI32 a) (SymConstI32 b) =
  return $ SymConstBool $ Num.i32eq a b
vI32Relop op@IRelEq a b
  | isEqSymbolic a b  = return $ SymConstBool True -- a == a  ==  False
  | otherwise         = freshValBool $ VI32Relop op a b
-- ne
vI32Relop IRelNe (SymConstI32 a) (SymConstI32 b) =
  return $ SymConstBool $ Num.i32ne a b
vI32Relop op@IRelNe a b
  | isEqSymbolic a b  = return $ SymConstBool False -- a != a  ==  False
  | otherwise         = freshValBool $ VI32Relop op a b
-- lt
vI32Relop (IRelLt sx) (SymConstI32 a) (SymConstI32 b) =
  return $ SymConstBool $ Num.i32lt sx a b
vI32Relop op@(IRelLt sx) a b
  | isEqSymbolic a b  = return $ SymConstBool False -- a < a  ==  False
  | otherwise         = freshValBool $ VI32Relop op a b
-- le
vI32Relop (IRelLe sx) (SymConstI32 a) (SymConstI32 b) =
  return $ SymConstBool $ Num.i32le sx a b
vI32Relop op@(IRelLe sx) a b
  | isEqSymbolic a b  = return $ SymConstBool True -- a <= a  ==  True
  | otherwise         = freshValBool $ VI32Relop op a b
-- gt
vI32Relop (IRelGt sx) (SymConstI32 a) (SymConstI32 b) =
  return $ SymConstBool $ Num.i32gt sx a b
vI32Relop op@(IRelGt sx) a b
  | isEqSymbolic a b  = return $ SymConstBool False -- a > a  ==  False
  | otherwise         = freshValBool $ VI32Relop op a b
-- ge
vI32Relop (IRelGe sx) (SymConstI32 a) (SymConstI32 b) =
  return $ SymConstBool $ Num.i32ge sx a b
vI32Relop op@(IRelGe sx) a b
  | isEqSymbolic a b  = return $ SymConstBool True  -- a >= a  ==  True
  | otherwise         = freshValBool $ VI32Relop op a b

vI32Eq, vI32Ne :: MonadSymbolics env m => Symbolic env TI32 -> Symbolic env TI32 -> m (Symbolic env Bool)
vI32Eq = vI32Relop IRelEq
vI32Ne = vI32Relop IRelNe

vI32Lt, vI32Le, vI32Gt, vI32Ge :: MonadSymbolics env m => Sx -> Symbolic env TI32 -> Symbolic env TI32 -> m (Symbolic env Bool)
vI32Lt = vI32Relop . IRelLt
vI32Le = vI32Relop . IRelLe
vI32Gt = vI32Relop . IRelGt
vI32Ge = vI32Relop . IRelGe


-- # I64 iunops

vI64Unop :: MonadSymbolics env m => IUnop TI64 -> Symbolic env TI64 -> m (Symbolic env TI64)
vI64Unop IUnClz    (SymConstI64 a) = return $ SymConstI64 $ Num.i64clz    a
vI64Unop IUnCtz    (SymConstI64 a) = return $ SymConstI64 $ Num.i64ctz    a
vI64Unop IUnPopcnt (SymConstI64 a) = return $ SymConstI64 $ Num.i64popcnt a
vI64Unop op a = freshValI64 $ VI64Unop op a

vI64Clz, vI64Ctz, vI64Popcnt :: MonadSymbolics env m => Symbolic env TI64 -> m (Symbolic env TI64)
vI64Clz    = vI64Unop IUnClz    -- count leading zeroes
vI64Ctz    = vI64Unop IUnCtz    -- count trailing zeroes
vI64Popcnt = vI64Unop IUnPopcnt -- count 1 bits


-- # I64 ibinop

vI64Binop :: MonadSymbolics env m => IBinop TI64 -> Symbolic env TI64 -> Symbolic env TI64 -> m (Symbolic env TI64)
vI64Binop IBinAdd  (SymConstI64 a) (SymConstI64 b) = return $ SymConstI64 $ Num.i64add  a b
vI64Binop IBinSub  (SymConstI64 a) (SymConstI64 b) = return $ SymConstI64 $ Num.i64sub  a b
vI64Binop IBinMul  (SymConstI64 a) (SymConstI64 b) = return $ SymConstI64 $ Num.i64mul  a b
vI64Binop IBinAnd  (SymConstI64 a) (SymConstI64 b) = return $ SymConstI64 $ Num.i64and  a b
vI64Binop IBinOr   (SymConstI64 a) (SymConstI64 b) = return $ SymConstI64 $ Num.i64or   a b
vI64Binop IBinXor  (SymConstI64 a) (SymConstI64 b) = return $ SymConstI64 $ Num.i64xor  a b
vI64Binop IBinShl  (SymConstI64 a) (SymConstI64 b) = return $ SymConstI64 $ Num.i64shl  a b
vI64Binop IBinRotl (SymConstI64 a) (SymConstI64 b) = return $ SymConstI64 $ Num.i64rotl a b
vI64Binop IBinRotr (SymConstI64 a) (SymConstI64 b) = return $ SymConstI64 $ Num.i64rotr a b

-- Undefined values evaluate to 0. Externally, a trapping condition must be enforced.
-- As the logic cannot "crash", it must evaluate to some arbitrary value.
vI64Binop op@(IBinDiv sx) a (SymConstI64 (WI64 0)) =
  return $ SymConstI64 $ WI64 0
vI64Binop op@(IBinDiv sx) (SymConstI64 a) (SymConstI64 b) =
  return $ SymConstI64 $ fromMaybe (WI64 0) $ Num.i64div sx a b

vI64Binop op@(IBinRem sx) a (SymConstI64 (WI64 0)) =
  return $ SymConstI64 $ WI64 0
vI64Binop op@(IBinRem sx) (SymConstI64 a) (SymConstI64 b) =
  return $ SymConstI64 $ fromMaybe (WI64 0) $ Num.i64rem sx a b

vI64Binop (IBinShr sx) (SymConstI64 a) (SymConstI64 b) =
  return $ SymConstI64 $ Num.i64shr sx a b

vI64Binop op a b = freshValI64 $ VI64Binop op a b


vI64Add, vI64Sub, vI64Mul, vI64And, vI64Or, vI64Xor, vI64Shl, vI64Rotl, vI64Rotr
  :: MonadSymbolics env m => Symbolic env TI64 -> Symbolic env TI64 -> m (Symbolic env TI64)
vI64Add  = vI64Binop IBinAdd
vI64Sub  = vI64Binop IBinSub
vI64Mul  = vI64Binop IBinMul
vI64And  = vI64Binop IBinAnd
vI64Or   = vI64Binop IBinOr
vI64Xor  = vI64Binop IBinXor
vI64Shl  = vI64Binop IBinShl
vI64Rotl = vI64Binop IBinRotl
vI64Rotr = vI64Binop IBinRotr

vI64Div, vI64Rem, vI64Shr
  :: MonadSymbolics env m => Sx -> Symbolic env TI64 -> Symbolic env TI64 -> m (Symbolic env TI64)
vI64Div = vI64Binop . IBinDiv
vI64Rem = vI64Binop . IBinRem
vI64Shr = vI64Binop . IBinShr


-- # I64 itestop

vI64Testop :: MonadSymbolics env m => ITestop TI64 -> Symbolic env TI64 -> m (Symbolic env Bool)
vI64Testop ITestEqz (SymConstI64 a) = return $ SymConstBool $ Num.i64eqz a
vI64Testop ITestEqz a = vI64Eq a =<< vI64Zero

vI64Eqz :: MonadSymbolics env m => Symbolic env TI64 -> m (Symbolic env Bool)
vI64Eqz = vI64Testop ITestEqz


-- # I64 irelop

vI64Relop :: MonadSymbolics env m => IRelop TI64 -> Symbolic env TI64 -> Symbolic env TI64 -> m (Symbolic env Bool)
-- eq
vI64Relop IRelEq (SymConstI64 a) (SymConstI64 b) =
  return $ SymConstBool $ Num.i64eq a b
vI64Relop op@IRelEq a b
  | isEqSymbolic a b  = return $ SymConstBool True -- a == a  ==  False
  | otherwise         = freshValBool $ VI64Relop op a b
-- ne
vI64Relop IRelNe (SymConstI64 a) (SymConstI64 b) =
  return $ SymConstBool $ Num.i64ne a b
vI64Relop op@IRelNe a b
  | isEqSymbolic a b  = return $ SymConstBool False -- a != a  ==  False
  | otherwise         = freshValBool $ VI64Relop op a b
-- lt
vI64Relop (IRelLt sx) (SymConstI64 a) (SymConstI64 b) =
  return $ SymConstBool $ Num.i64lt sx a b
vI64Relop op@(IRelLt sx) a b
  | isEqSymbolic a b  = return $ SymConstBool False -- a < a  ==  False
  | otherwise         = freshValBool $ VI64Relop op a b
-- le
vI64Relop (IRelLe sx) (SymConstI64 a) (SymConstI64 b) =
  return $ SymConstBool $ Num.i64le sx a b
vI64Relop op@(IRelLe sx) a b
  | isEqSymbolic a b  = return $ SymConstBool True -- a <= a  ==  True
  | otherwise         = freshValBool $ VI64Relop op a b
-- gt
vI64Relop (IRelGt sx) (SymConstI64 a) (SymConstI64 b) =
  return $ SymConstBool $ Num.i64gt sx a b
vI64Relop op@(IRelGt sx) a b
  | isEqSymbolic a b  = return $ SymConstBool False -- a > a  ==  False
  | otherwise         = freshValBool $ VI64Relop op a b
-- ge
vI64Relop (IRelGe U) _ (SymConstI64 (WI64 0)) =
  return $ SymConstBool True -- unsigned (x >= 0)  ==  True
vI64Relop (IRelGe S) _ (SymConstI64 (WI64 0x800000000000)) =
  return $ SymConstBool True -- signed (x >= -2^63)  ==  True
vI64Relop (IRelGe sx) (SymConstI64 a) (SymConstI64 b) =
  return $ SymConstBool $ Num.i64ge sx a b
vI64Relop op@(IRelGe sx) a b
  | isEqSymbolic a b  = return $ SymConstBool True  -- a >= a  ==  True
  | otherwise         = freshValBool $ VI64Relop op a b


vI64Eq, vI64Ne :: MonadSymbolics env m => Symbolic env TI64 -> Symbolic env TI64 -> m (Symbolic env Bool)
vI64Eq = vI64Relop IRelEq
vI64Ne = vI64Relop IRelNe

vI64Lt, vI64Le, vI64Gt, vI64Ge :: MonadSymbolics env m => Sx -> Symbolic env TI64 -> Symbolic env TI64 -> m (Symbolic env Bool)
vI64Lt = vI64Relop . IRelLt
vI64Le = vI64Relop . IRelLe
vI64Gt = vI64Relop . IRelGt
vI64Ge = vI64Relop . IRelGe


-- # F32 funops

vF32Unop :: MonadExtSymbolics env m => FUnop TF32 -> Symbolic env TF32 -> m (Symbolic env TF32)
vF32Unop = freshValF32 .: VF32Unop

vF32Abs, vF32Neg, vF32Sqrt, vF32Ceil, vF32Floor, vF32Trunc, vF32Nearest
  :: MonadExtSymbolics env m => Symbolic env TF32 -> m (Symbolic env TF32)
vF32Abs     = vF32Unop FUnAbs
vF32Neg     = vF32Unop FUnNeg
vF32Sqrt    = vF32Unop FUnSqrt
vF32Ceil    = vF32Unop FUnCeil
vF32Floor   = vF32Unop FUnFloor
vF32Trunc   = vF32Unop FUnTrunc
vF32Nearest = vF32Unop FUnNearest


-- # F32 fbinop

vF32Binop :: MonadExtSymbolics env m => FBinop TF32 -> Symbolic env TF32 -> Symbolic env TF32 -> m (Symbolic env TF32)
vF32Binop = freshValF32 .:. VF32Binop

vF32Add, vF32Sub, vF32Mul, vF32Div, vF32Min, vF32Max, vF32Copysign
  :: MonadExtSymbolics env m => Symbolic env TF32 -> Symbolic env TF32 -> m (Symbolic env TF32)
vF32Add      = vF32Binop FBinAdd
vF32Sub      = vF32Binop FBinSub
vF32Mul      = vF32Binop FBinMul
vF32Div      = vF32Binop FBinDiv
vF32Min      = vF32Binop FBinMin
vF32Max      = vF32Binop FBinMax
vF32Copysign = vF32Binop FBinCopysign


-- # F32 frelop

vF32Relop :: MonadExtSymbolics env m => FRelop TF32 -> Symbolic env TF32 -> Symbolic env TF32 -> m (Symbolic env Bool)
-- No optimizations can be applied here. a == a does /not/ always evaluate to
-- `True`; e.g., for NaNs. Lots of weird stuff, so let the solver deal with it.
vF32Relop = freshValBool .:. VF32Relop

vF32Eq, vF32Ne, vF32Lt, vF32Gt, vF32Le, vF32Ge
  :: MonadExtSymbolics env m => Symbolic env TF32 -> Symbolic env TF32 -> m (Symbolic env Bool)
vF32Eq = vF32Relop FRelEq
vF32Ne = vF32Relop FRelNe
vF32Lt = vF32Relop FRelLt
vF32Gt = vF32Relop FRelGt
vF32Le = vF32Relop FRelLe
vF32Ge = vF32Relop FRelGe


-- # F64 funops

vF64Unop :: MonadExtSymbolics env m => FUnop TF64 -> Symbolic env TF64 -> m (Symbolic env TF64)
vF64Unop = freshValF64 .: VF64Unop

vF64Abs, vF64Neg, vF64Sqrt, vF64Ceil, vF64Floor, vF64Trunc, vF64Nearest
  :: MonadExtSymbolics env m => Symbolic env TF64 -> m (Symbolic env TF64)
vF64Abs     = vF64Unop FUnAbs
vF64Neg     = vF64Unop FUnNeg
vF64Sqrt    = vF64Unop FUnSqrt
vF64Ceil    = vF64Unop FUnCeil
vF64Floor   = vF64Unop FUnFloor
vF64Trunc   = vF64Unop FUnTrunc
vF64Nearest = vF64Unop FUnNearest


-- # F64 fbinop

vF64Binop :: MonadExtSymbolics env m => FBinop TF64 -> Symbolic env TF64 -> Symbolic env TF64 -> m (Symbolic env TF64)
vF64Binop = freshValF64 .:. VF64Binop

vF64Add, vF64Sub, vF64Mul, vF64Div, vF64Min, vF64Max, vF64Copysign
  :: MonadExtSymbolics env m => Symbolic env TF64 -> Symbolic env TF64 -> m (Symbolic env TF64)
vF64Add      = vF64Binop FBinAdd
vF64Sub      = vF64Binop FBinSub
vF64Mul      = vF64Binop FBinMul
vF64Div      = vF64Binop FBinDiv
vF64Min      = vF64Binop FBinMin
vF64Max      = vF64Binop FBinMax
vF64Copysign = vF64Binop FBinCopysign


-- # F64 frelop

vF64Relop :: MonadExtSymbolics env m => FRelop TF64 -> Symbolic env TF64 -> Symbolic env TF64 -> m (Symbolic env Bool)
vF64Relop = freshValBool .:. VF64Relop

vF64Eq, vF64Ne, vF64Lt, vF64Gt, vF64Le, vF64Ge
  :: MonadExtSymbolics env m => Symbolic env TF64 -> Symbolic env TF64 -> m (Symbolic env Bool)
vF64Eq = vF64Relop FRelEq
vF64Ne = vF64Relop FRelNe
vF64Lt = vF64Relop FRelLt
vF64Gt = vF64Relop FRelGt
vF64Le = vF64Relop FRelLe
vF64Ge = vF64Relop FRelGe


-- # Parametric operators

-- While these all use the same constructor, symbolic types are not allowed to
-- be of all types. So, avoid existential instantiation.

vI32Select :: MonadSymbolics env m => Symbolic env Bool -> Symbolic env TI32 -> Symbolic env TI32 -> m (Symbolic env TI32)
vI32Select = vAnySelect freshValI32

vI64Select :: MonadSymbolics env m => Symbolic env Bool -> Symbolic env TI64 -> Symbolic env TI64 -> m (Symbolic env TI64)
vI64Select = vAnySelect freshValI64

vF32Select :: MonadExtSymbolics env m => Symbolic env Bool -> Symbolic env TF32 -> Symbolic env TF32 -> m (Symbolic env TF32)
vF32Select = vAnySelect freshValF32

vF64Select :: MonadExtSymbolics env m => Symbolic env Bool -> Symbolic env TF64 -> Symbolic env TF64 -> m (Symbolic env TF64)
vF64Select = vAnySelect freshValF64

vI33Select :: MonadSymbolics env m => Symbolic env Bool -> Symbolic env TI33 -> Symbolic env TI33 -> m (Symbolic env TI33)
vI33Select = vAnySelect freshValI33

vBoolSelect :: MonadSymbolics env m => Symbolic env Bool -> Symbolic env Bool -> Symbolic env Bool -> m (Symbolic env Bool)
vBoolSelect c a (SymConstBool False) = vBAnd c a -- c?a:F  =  c&a
vBoolSelect c (SymConstBool False) b = join (vBAnd <$> vBNot c <*> pure b) -- c?F:b  =  !c&b
vBoolSelect c a b = vAnySelect freshValBool c a b

vMemSelect :: MonadSymbolics env m => Symbolic env Bool -> Symbolic env TMem -> Symbolic env TMem -> m (Symbolic env TMem)
vMemSelect = vAnySelect freshValMem

vWorldSelect :: MonadExtSymbolics env m => Symbolic env Bool -> Symbolic env TWorld -> Symbolic env TWorld -> m (Symbolic env TWorld)
vWorldSelect = vAnySelect freshValWorld

-- | Generalization over the @select@ functions.
vAnySelect :: Monad m
           => ( ComposedVal env a -> m (Symbolic env a) )
           -> Symbolic env Bool
           -> Symbolic env a
           -> Symbolic env a
           -> m (Symbolic env a)
vAnySelect _ (SymConstBool True)  iTrue _      = return iTrue
vAnySelect _ (SymConstBool False) _     iFalse = return iFalse
vAnySelect fFresh iCond iTrue iFalse
  | isEqSymbolic iTrue iFalse  = return iTrue -- If both cases are surely equal anyway, no need to select
  | otherwise                  = fFresh $ VSelect iCond iTrue iFalse


-- # Memory operators

vI32Load :: MonadSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> m (Symbolic env TI32)
vI32Load = freshValI32 .: VI32Load

vI64Load :: MonadSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> m (Symbolic env TI64)
vI64Load = freshValI64 .: VI64Load

vF32Load :: MonadExtSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> m (Symbolic env TF32)
vF32Load = freshValF32 .: VF32Load

vF64Load :: MonadExtSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> m (Symbolic env TF64)
vF64Load = freshValF64 .: VF64Load

vI32Load8 :: MonadSymbolics env m => Sx -> Symbolic env TMem -> Symbolic env TI33 -> m (Symbolic env TI32)
vI32Load8 = freshValI32 .:. VI32Load8

vI64Load8 :: MonadSymbolics env m => Sx -> Symbolic env TMem -> Symbolic env TI33 -> m (Symbolic env TI64)
vI64Load8 = freshValI64 .:. VI64Load8

vI32Load16 :: MonadSymbolics env m => Sx -> Symbolic env TMem -> Symbolic env TI33 -> m (Symbolic env TI32)
vI32Load16 = freshValI32 .:. VI32Load16

vI64Load16 :: MonadSymbolics env m => Sx -> Symbolic env TMem -> Symbolic env TI33 -> m (Symbolic env TI64)
vI64Load16 = freshValI64 .:. VI64Load16

vI64Load32 :: MonadSymbolics env m => Sx -> Symbolic env TMem -> Symbolic env TI33 -> m (Symbolic env TI64)
vI64Load32 = freshValI64 .:. VI64Load32

vI32Store :: MonadSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI32 -> m (Symbolic env TMem)
vI32Store = freshValMem .:. VI32Store

vI64Store :: MonadSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI64 -> m (Symbolic env TMem)
vI64Store = freshValMem .:. VI64Store

vF32Store :: MonadSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TF32 -> m (Symbolic env TMem)
vF32Store = freshValMem .:. VF32Store

vF64Store :: MonadSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TF64 -> m (Symbolic env TMem)
vF64Store = freshValMem .:. VF64Store

vI32Store8 :: MonadSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI32 -> m (Symbolic env TMem)
vI32Store8 = freshValMem .:. VI32Store8

vI64Store8 :: MonadSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI64 -> m (Symbolic env TMem)
vI64Store8 = freshValMem .:. VI64Store8

vI32Store16 :: MonadSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI32 -> m (Symbolic env TMem)
vI32Store16 = freshValMem .:. VI32Store16

vI64Store16 :: MonadSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI64 -> m (Symbolic env TMem)
vI64Store16 = freshValMem .:. VI64Store16

vI64Store32 :: MonadSymbolics env m => Symbolic env TMem -> Symbolic env TI33 -> Symbolic env TI64 -> m (Symbolic env TMem)
vI64Store32 = freshValMem .:. VI64Store32


-- # Conversion

vI32Extend8S :: MonadSymbolics env m => Symbolic env TI32 -> m (Symbolic env TI32)
vI32Extend8S (SymConstI32 a) = return $ SymConstI32 $ Num.i32extend8s a
vI32Extend8S a = freshValI32 $ VI32Extend8S a

vI32Extend16S :: MonadSymbolics env m => Symbolic env TI32 -> m (Symbolic env TI32)
vI32Extend16S (SymConstI32 a) = return $ SymConstI32 $ Num.i32extend16s a
vI32Extend16S a = freshValI32 $ VI32Extend16S a

vI64Extend8S :: MonadSymbolics env m => Symbolic env TI64 -> m (Symbolic env TI64)
vI64Extend8S (SymConstI64 a) = return $ SymConstI64 $ Num.i64extend8s a
vI64Extend8S a = freshValI64 $ VI64Extend8S a

vI64Extend16S :: MonadSymbolics env m => Symbolic env TI64 -> m (Symbolic env TI64)
vI64Extend16S (SymConstI64 a) = return $ SymConstI64 $ Num.i64extend16s a
vI64Extend16S a = freshValI64 $ VI64Extend16S a

vI64Extend32S :: MonadSymbolics env m => Symbolic env TI64 -> m (Symbolic env TI64)
vI64Extend32S (SymConstI64 a) = return $ SymConstI64 $ Num.i64extend32s a
vI64Extend32S a = freshValI64 $ VI64Extend32S a

vI32WrapI64 :: MonadSymbolics env m => Symbolic env TI64 -> m (Symbolic env TI32)
vI32WrapI64 (SymConstI64 (WI64 a)) = return $ SymConstI32 $ WI32 $ toNum ( a `mod` 2^32 )
vI32WrapI64 a = freshValI32 $ VI32WrapI64 a

vI64ExtendI32 :: MonadSymbolics env m => Sx -> Symbolic env TI32 -> m (Symbolic env TI64)
vI64ExtendI32 sx (SymConstI32 a) = return $ SymConstI64 $ Num.i64extendI32 sx a
vI64ExtendI32 sx a = freshValI64 $ VI64ExtendI32 sx a

vI32TruncF32 :: MonadExtSymbolics env m => Sat -> Sx -> Symbolic env TF32 -> m (Symbolic env TI32)
vI32TruncF32 = freshValI32 .:. VI32TruncF32

vI32TruncF64 :: MonadExtSymbolics env m => Sat -> Sx -> Symbolic env TF64 -> m (Symbolic env TI32)
vI32TruncF64 = freshValI32 .:. VI32TruncF64

vI64TruncF32 :: MonadExtSymbolics env m => Sat -> Sx -> Symbolic env TF32 -> m (Symbolic env TI64)
vI64TruncF32 = freshValI64 .:. VI64TruncF32

vI64TruncF64 :: MonadExtSymbolics env m => Sat -> Sx -> Symbolic env TF64 -> m (Symbolic env TI64)
vI64TruncF64 = freshValI64 .:. VI64TruncF64

vF32DemoteF64 :: MonadExtSymbolics env m => Symbolic env TF64 -> m (Symbolic env TF32)
vF32DemoteF64 = freshValF32 . VF32DemoteF64

vF64PromoteF32 :: MonadExtSymbolics env m => Symbolic env TF32 -> m (Symbolic env TF64)
vF64PromoteF32 = freshValF64 . VF64PromoteF32

vF32ConvertI32 :: MonadExtSymbolics env m => Sx -> Symbolic env TI32 -> m (Symbolic env TF32)
vF32ConvertI32 = freshValF32 .: VF32ConvertI32

vF32ConvertI64 :: MonadExtSymbolics env m => Sx -> Symbolic env TI64 -> m (Symbolic env TF32)
vF32ConvertI64 = freshValF32 .: VF32ConvertI64

vF64ConvertI32 :: MonadExtSymbolics env m => Sx -> Symbolic env TI32 -> m (Symbolic env TF64)
vF64ConvertI32 = freshValF64 .: VF64ConvertI32

vF64ConvertI64 :: MonadExtSymbolics env m => Sx -> Symbolic env TI64 -> m (Symbolic env TF64)
vF64ConvertI64 = freshValF64 .: VF64ConvertI64

vI32ReinterpretF32 :: MonadExtSymbolics env m => Symbolic env TF32 -> m (Symbolic env TI32)
vI32ReinterpretF32 = freshValI32 . VI32ReinterpretF32

vI64ReinterpretF64 :: MonadExtSymbolics env m => Symbolic env TF64 -> m (Symbolic env TI64)
vI64ReinterpretF64 = freshValI64 . VI64ReinterpretF64

vF32ReinterpretI32 :: MonadExtSymbolics env m => Symbolic env TI32 -> m (Symbolic env TF32)
vF32ReinterpretI32 = freshValF32 . VF32ReinterpretI32

vF64ReinterpretI64 :: MonadExtSymbolics env m => Symbolic env TI64 -> m (Symbolic env TF64)
vF64ReinterpretI64 = freshValF64 . VF64ReinterpretI64


-- # Extra operators

vMemUnknown :: MonadSymbolics env m => m (Symbolic env TMem)
vMemUnknown = freshValMem VMemUnknown

vWorldUnknown :: MonadExtSymbolics env m => m (Symbolic env TWorld)
vWorldUnknown = freshValWorld VWorldUnknown

vFuncUnknown :: MonadExtSymbolics env m => [ValType] -> [ValType] -> m (Symbolic env TFunc)
vFuncUnknown params results = freshValFunc $ VFuncUnknown params results

vBoolConst :: MonadSymbolics env m => Bool -> m (Symbolic env Bool)
vBoolConst = return . SymConstBool -- freshValBool . VBoolConst

vI33Unknown :: MonadSymbolics env m => m (Symbolic env TI33)
vI33Unknown = freshValI33 VI33Unknown

vBoolUnknown :: MonadSymbolics env m => m (Symbolic env Bool)
vBoolUnknown = freshValBool VBoolUnknown

vI33Const :: MonadSymbolics env m => Natural -> m (Symbolic env TI33)
vI33Const = return . SymConstI33 -- freshValI33 . VI33Const

-- | Multiply the page count by the page size (64*1024).
vI33MulPage :: MonadSymbolics env m => Symbolic env TI32 -> m (Symbolic env TI33)
vI33MulPage (SymConstI32 (WI32 a)) = return $ SymConstI33 $ boundAddr (toNum a * 64 * 1024)
vI33MulPage a = freshValI33 $ VI33MulPage a

vI33Address :: MonadSymbolics env m => Symbolic env TI32 -> Word32 -> m (Symbolic env TI33)
vI33Address (SymConstI32 (WI32 a)) offset = return $ SymConstI33 $ boundAddr (toNum a + toNum offset)
vI33Address v offset = freshValI33 $ VI33Address v offset

vI33Lt :: MonadSymbolics env m => Symbolic env TI33 -> Symbolic env TI33 -> m (Symbolic env Bool)
vI33Lt (SymConstI33 a) (SymConstI33 b) = return $ SymConstBool (a < b)
vI33Lt a b
  | isEqSymbolic a b  = return $ SymConstBool False -- a < a  ==  False
  | otherwise         = freshValBool $ VI33Lt a b

boundAddr :: Natural -> Natural
boundAddr = min (2^33-1 :: Natural)


-- # Extra operators (Booleans)

vBAnd :: MonadSymbolics env m => Symbolic env Bool -> Symbolic env Bool -> m (Symbolic env Bool)
vBAnd (SymConstBool False) _ = return $ SymConstBool False
vBAnd _ (SymConstBool False) = return $ SymConstBool False
vBAnd (SymConstBool True) b = return b
vBAnd a (SymConstBool True) = return a
vBAnd a b = freshValBool $ VBAnd a b

vBOr :: MonadSymbolics env m => Symbolic env Bool -> Symbolic env Bool -> m (Symbolic env Bool)
vBOr (SymConstBool True) _ = return $ SymConstBool True
vBOr _ (SymConstBool True) = return $ SymConstBool True
vBOr (SymConstBool False) b = return b
vBOr a (SymConstBool False) = return a
vBOr a b = freshValBool $ VBOr a b

vBImplies :: MonadSymbolics env m => Symbolic env Bool -> Symbolic env Bool -> m (Symbolic env Bool)
vBImplies (SymConstBool False) _ = return $ SymConstBool True
vBImplies _ (SymConstBool True) = return $ SymConstBool True
vBImplies (SymConstBool True) b = return b
vBImplies a (SymConstBool False) = vBNot a
vBImplies a b = freshValBool $ VBImplies a b

vBNot :: MonadSymbolics env m => Symbolic env Bool -> m (Symbolic env Bool)
vBNot (SymConstBool v) = return $ SymConstBool (not v)
vBNot a = freshValBool $ VBNot a

-- | Converts an integer to a boolean. @bool(x) = (x/=0)@
vBI32 :: MonadSymbolics env m => Symbolic env TI32 -> m (Symbolic env Bool)
vBI32 (SymConstI32 (WI32 v)) = return $ SymConstBool (v /= 0)
vBI32 a = freshValBool $ VBI32 a

-- | Converts a boolean to an integer. @ibool(True) = 1@, @ibool(False) = 0@.
vI32Bool :: MonadSymbolics env m => Symbolic env Bool -> m (Symbolic env TI32)
vI32Bool (SymConstBool b) = return $ SymConstI32 $ WI32 (if b then 1 else 0)
vI32Bool a = freshValI32 $ VI32Bool a


-- # Side effect operators

-- | Grows the memory by a number of /memory pages/ (of size 64KiB). As this
-- operator depends on the system resources, it transforms the world.
vMemGrowWorld :: MonadExtSymbolics env m => Symbolic env TWorld -> Symbolic env TI32 -> m (Symbolic env TWorld)
vMemGrowWorld w numPagesGrow = freshValWorld $ VMemGrowWorld w numPagesGrow

-- | Returns `True` if growing memory succeeded.
vMemGrowBool :: MonadExtSymbolics env m => Symbolic env TWorld -> Symbolic env TI32 -> m (Symbolic env Bool)
vMemGrowBool w numPagesGrow = freshValBool $ VMemGrowBool w numPagesGrow

vFuncCallWorld :: MonadExtSymbolics env m => Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> m (Symbolic env TWorld)
vFuncCallWorld = freshValWorld .::. VFuncCallWorld

vFuncCallMem :: MonadExtSymbolics env m => Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> m (Symbolic env TMem)
vFuncCallMem = freshValMem .::. VFuncCallMem

vFuncCallResultI32 :: MonadExtSymbolics env m => Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> Natural -> m (Symbolic env TI32)
vFuncCallResultI32 = freshValI32 .::: VFuncCallResultI32

vFuncCallResultI64 :: MonadExtSymbolics env m => Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> Natural -> m (Symbolic env TI64)
vFuncCallResultI64 = freshValI64 .::: VFuncCallResultI64

vFuncCallResultF32 :: MonadExtSymbolics env m => Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> Natural -> m (Symbolic env TF32)
vFuncCallResultF32 = freshValF32 .::: VFuncCallResultF32

vFuncCallResultF64 :: MonadExtSymbolics env m => Symbolic env TFunc -> Symbolic env TWorld -> Symbolic env TMem -> Symbolic env TMemSize -> [ExtSymbolicVal env] -> Natural -> m (Symbolic env TF64)
vFuncCallResultF64 = freshValF64 .::: VFuncCallResultF64


-- # Helpers

vI32Zero :: MonadSymbolics env m => m (Symbolic env TI32)
vI32Zero = vI32Const (WI32 0)

vI64Zero :: MonadSymbolics env m => m (Symbolic env TI64)
vI64Zero = vI64Const (WI64 0)

vF32Zero :: MonadExtSymbolics env m => m (Symbolic env TF32)
vF32Zero = vF32Const (WF32 0)

vF64Zero :: MonadExtSymbolics env m => m (Symbolic env TF64)
vF64Zero = vF64Const (WF64 0)

-- | Lifts the symbolics instance to an extended symbolics instance (which also
-- tracks side-effects)
liftExt :: Symbolics env -> ExtSymbolics env
liftExt (Symbolics vals) = ExtSymbolics $ IdList.map ECVBase vals

size :: Symbolics env -> Int
size (Symbolics xs) = IdList.size xs

justifyZeroHex :: (Integral a, Show a) => Int -> a -> ShowS
justifyZeroHex numChars v =
  let vStr = showHex v ""
  in ß "0x" . ß (replicate (numChars - length vStr) '0') . ß vStr

-- I don't want to derive `Eq` for `Symbolic`. This checks simple equality of
-- the data structures.
isEqSymbolic :: Symbolic env a -> Symbolic env a -> Bool
isEqSymbolic (Symbolic a)     (Symbolic b)      = a == b
isEqSymbolic (SymConstI32 a)  (SymConstI32 b)   = a == b
isEqSymbolic (SymConstI33 a)  (SymConstI33 b)   = a == b
isEqSymbolic (SymConstI64 a)  (SymConstI64 b)   = a == b
isEqSymbolic (SymConstF32 a)  (SymConstF32 b)   = a == b
isEqSymbolic (SymConstF64 a)  (SymConstF64 b)   = a == b
isEqSymbolic (SymConstBool a) (SymConstBool b)  = a == b
isEqSymbolic _ _ = False

newtype SymbolicsCombiner envSrc envDst ( m :: * -> * ) a
  = SymbolicsCombiner
      (RWST
        (Vector (AnyComposedVal envSrc))
        ()
        (Symbolics envDst, IntMap Int)
        m
        a
      )
    deriving (Functor, Applicative, Monad)

uc :: SymbolicsCombiner envSrc envDst m a
   -> RWST
        (Vector (AnyComposedVal envSrc))
        ()
        (Symbolics envDst, IntMap Int)
        m
        a
uc (SymbolicsCombiner c) = c

runCombiner :: Monad m
            => SymbolicsCombiner src dst m a
            -> Symbolics src
            -> Symbolics dst
            -> m (a, Symbolics dst)
runCombiner (SymbolicsCombiner c) (Symbolics s) dst =
  do
    (a, (dst', _), _) <- runRWST c (IdList.toVector s) (dst, IntMap.empty)
    return (a, dst')

combineBool :: MonadFail m => Symbolic src Bool -> SymbolicsCombiner src dst m (Symbolic dst Bool)
combineBool (SymConstBool b) = return $ SymConstBool b
combineBool (Symbolic i) =
  SymbolicsCombiner $
    do
      mNewI <- S.gets (IntMap.lookup i . snd)
      case mNewI of
        Just newI -> return $ Symbolic newI -- was already inserted into `dst`
        Nothing   ->
          do
            ACVBool res <- (lift . failMaybeMsg "Invalid symbolic") =<< R.asks (!? i)
            Symbolic newI <-
              case res of
                VI32Relop op a b ->
                  uc $ join (vI32Relop op <$> combineI32 a <*> combineI32 b)
                VI64Relop op a b ->
                  uc $ join (vI64Relop op <$> combineI64 a <*> combineI64 b)
                VSelect c t f ->
                  uc $ join (vBoolSelect <$> combineBool c <*> combineBool t <*> combineBool f)
                VI33Lt a b ->
                  uc $ join (vI33Lt <$> combineI33 a <*> combineI33 b)
                VBoolUnknown -> uc vBoolUnknown
                VBAnd a b -> uc $ join (vBAnd <$> combineBool a <*> combineBool b)
                VBOr a b -> uc $ join (vBOr <$> combineBool a <*> combineBool b)
                VBImplies a b ->
                  uc $ join (vBImplies <$> combineBool a <*> combineBool b)
                VBNot a -> uc (vBNot =<< combineBool a)
                VBI32 a -> uc (vBI32 =<< combineI32 a)
                _ -> fail ("Invalid operator Bool " ++ show res)
            S.modify $ mapSnd $ IntMap.insert i newI
            return $ Symbolic newI

combineVal :: MonadFail m => SymbolicVal src -> SymbolicsCombiner src dst m (SymbolicVal dst)
combineVal (VI32 x) = VI32 <$> combineI32 x
combineVal (VI64 x) = VI64 <$> combineI64 x
combineVal (VF32 x) = return $ VF32 ()
combineVal (VF64 x) = return $ VF64 ()

combineI32 :: MonadFail m => Symbolic src TI32 -> SymbolicsCombiner src dst m (Symbolic dst TI32)
combineI32 (SymConstI32 b) = return $ SymConstI32 b
combineI32 (Symbolic i) =
  SymbolicsCombiner $
    do
      mNewI <- S.gets (IntMap.lookup i . snd)
      case mNewI of
        Just newI -> return $ Symbolic newI -- was already inserted into `dst`
        Nothing   ->
          do
            ACVI32 res <- (lift . failMaybeMsg "Invalid symbolic") =<< R.asks (!? i)
            Symbolic newI <-
              case res of
                VI32Unknown -> uc vI32Unknown
                VI32Unop op a -> uc (vI32Unop op =<< combineI32 a)
                VI32Binop op a b ->
                  uc $ join (vI32Binop op <$> combineI32 a <*> combineI32 b)
                VSelect c t f ->
                  uc $ join (vI32Select <$> combineBool c <*> combineI32 t <*> combineI32 f)
                VI32Extend8S a -> uc (vI32Extend8S =<< combineI32 a)
                VI32Extend16S a -> uc (vI32Extend16S =<< combineI32 a)
                VI32WrapI64 a -> uc (vI32WrapI64 =<< combineI64 a)
                VI32Load m a ->
                  uc $ join (vI32Load <$> combineMem m <*> combineI33 a)
                VI32Load8 sx m a ->
                  uc $ join (vI32Load8 sx <$> combineMem m <*> combineI33 a)
                VI32Load16 sx m a ->
                  uc $ join (vI32Load16 sx <$> combineMem m <*> combineI33 a)
                VI32Bool b -> uc (vI32Bool =<< combineBool b)
                _ -> fail ("Invalid operator I32 " ++ show res)
            S.modify $ mapSnd $ IntMap.insert i newI
            return $ Symbolic newI

combineI33 :: MonadFail m => Symbolic src TI33 -> SymbolicsCombiner src dst m (Symbolic dst TI33)
combineI33 (SymConstI33 b) = return $ SymConstI33 b
combineI33 (Symbolic i) =
  SymbolicsCombiner $
    do
      mNewI <- S.gets (IntMap.lookup i . snd)
      case mNewI of
        Just newI -> return $ Symbolic newI -- was already inserted into `dst`
        Nothing   ->
          do
            ACVI33 res <- (lift . failMaybeMsg "Invalid symbolic") =<< R.asks (!? i)
            Symbolic newI <-
              case res of
                VI33Unknown -> uc vI33Unknown
                VI33Address a off ->
                  uc $ join (vI33Address <$> combineI32 a <*> pure off)
                VI33MulPage x -> uc (vI33MulPage =<< combineI32 x)
                _ -> fail ("Invalid operator I33 " ++ show res)
            S.modify $ mapSnd $ IntMap.insert i newI
            return $ Symbolic newI

combineI64 :: MonadFail m => Symbolic src TI64 -> SymbolicsCombiner src dst m (Symbolic dst TI64)
combineI64 (SymConstI64 b) = return $ SymConstI64 b
combineI64 (Symbolic i) =
  SymbolicsCombiner $
    do
      mNewI <- S.gets (IntMap.lookup i . snd)
      case mNewI of
        Just newI -> return $ Symbolic newI -- was already inserted into `dst`
        Nothing   ->
          do
            ACVI64 res <- (lift . failMaybeMsg "Invalid symbolic") =<< R.asks (!? i)
            Symbolic newI <-
              case res of
                VI64Unknown -> uc vI64Unknown
                VI64Unop op a -> uc (vI64Unop op =<< combineI64 a)
                VI64Binop op a b ->
                  uc $ join (vI64Binop op <$> combineI64 a <*> combineI64 b)
                VSelect c t f ->
                  uc $ join (vI64Select <$> combineBool c <*> combineI64 t <*> combineI64 f)
                VI64Extend8S a      -> uc (vI64Extend8S =<< combineI64 a)
                VI64Extend16S a     -> uc (vI64Extend16S =<< combineI64 a)
                VI64Extend32S a     -> uc (vI64Extend32S =<< combineI64 a)
                VI64ExtendI32 sx a  -> uc (vI64ExtendI32 sx =<< combineI32 a)
                VI64Load mem a      -> uc $ join (vI64Load <$> combineMem mem <*> combineI33 a)
                VI64Load8 sx mem a  -> uc $ join (vI64Load8 sx <$> combineMem mem <*> combineI33 a)
                VI64Load16 sx mem a -> uc $ join (vI64Load16 sx <$> combineMem mem <*> combineI33 a)
                VI64Load32 sx mem a -> uc $ join (vI64Load32 sx <$> combineMem mem <*> combineI33 a)
                _ -> fail ("Invalid operator I64 " ++ show res)
            S.modify $ mapSnd $ IntMap.insert i newI
            return $ Symbolic newI

combineMem :: MonadFail m => Symbolic src TMem -> SymbolicsCombiner src dst m (Symbolic dst TMem)
combineMem (Symbolic i) =
  SymbolicsCombiner $
    do
      mNewI <- S.gets (IntMap.lookup i . snd)
      case mNewI of
        Just newI -> return $ Symbolic newI -- was already inserted into `dst`
        Nothing   ->
          do
            ACVMem res <- (lift . failMaybeMsg "Invalid symbolic") =<< R.asks (!? i)
            Symbolic newI <-
              case res of
                VMemUnknown -> uc vMemUnknown
                VSelect c t f ->
                  uc $ join (vMemSelect <$> combineBool c <*> combineMem t <*> combineMem f)
                VI32Store m a v ->
                  uc $ join (vI32Store <$> combineMem m <*> combineI33 a <*> combineI32 v)
                VI64Store m a v ->
                  uc $ join (vI64Store <$> combineMem m <*> combineI33 a <*> combineI64 v)
                VI32Store8 m a v ->
                  uc $ join (vI32Store8 <$> combineMem m <*> combineI33 a <*> combineI32 v)
                VI32Store16 m a v ->
                  uc $ join (vI32Store16 <$> combineMem m <*> combineI33 a <*> combineI32 v)
                VI64Store8 m a v ->
                  uc $ join (vI64Store8 <$> combineMem m <*> combineI33 a <*> combineI64 v)
                VI64Store16 m a v ->
                  uc $ join (vI64Store16 <$> combineMem m <*> combineI33 a <*> combineI64 v)
                VI64Store32 m a v ->
                  uc $ join (vI64Store32 <$> combineMem m <*> combineI33 a <*> combineI64 v)
                _ -> fail ("Invalid operator Mem " ++ show res)
            S.modify $ mapSnd $ IntMap.insert i newI
            return $ Symbolic newI

instance Monad m => MonadSymbolics env (SymbolicsCombiner envSrc env m) where
  freshValBool = fmap Symbolic . freshValC . ACVBool
  freshValI32  = fmap Symbolic . freshValC . ACVI32
  freshValI33  = fmap Symbolic . freshValC . ACVI33
  freshValI64  = fmap Symbolic . freshValC . ACVI64
  freshValMem  = fmap Symbolic . freshValC . ACVMem

freshValC :: Monad m => AnyComposedVal env -> SymbolicsCombiner envSrc env m Int
freshValC c =
  SymbolicsCombiner $
    do
      Symbolics vals <- S.gets fst
      let (i, vals') = IdList.append c vals
      S.modify $ mapFst $ const $ Symbolics vals'
      return i


-- # Instances

class Monad m => MonadSymbolics env m | m -> env where
  freshValBool  :: ComposedVal env Bool -> m (Symbolic env Bool)
  freshValI32   :: ComposedVal env TI32 -> m (Symbolic env TI32)
  freshValI33   :: ComposedVal env TI33 -> m (Symbolic env TI33)
  freshValI64   :: ComposedVal env TI64 -> m (Symbolic env TI64)
  freshValMem   :: ComposedVal env TMem -> m (Symbolic env TMem)

class MonadSymbolics env m => MonadExtSymbolics env m | m -> env where
  freshValWorld  :: ComposedVal env TWorld -> m (Symbolic env TWorld)
  freshValFunc   :: ComposedVal env TFunc  -> m (Symbolic env TFunc)
  freshValF32    :: ComposedVal env TF32   -> m (Symbolic env TF32)
  freshValF64    :: ComposedVal env TF64   -> m (Symbolic env TF64)

-- | `StateT` wrapper. Avoids overlapping instances with nested `State`s.
newtype SymbolicsStateT env m a = SymbolicsStateT (StateT (Symbolics env) m a)
  deriving (Functor, Applicative, Monad)

-- | `StateT` wrapper. Avoids overlapping instances with nested `State`s.
newtype ExtSymbolicsStateT env m a = ExtSymbolicsStateT (StateT (ExtSymbolics env) m a)
  deriving (Functor, Applicative, Monad)

type SymbolicsState env a = SymbolicsStateT env Identity a

type ExtSymbolicsState env a = ExtSymbolicsStateT env Identity a

runSymbolicsStateT :: SymbolicsStateT env m a -> Symbolics env -> m (a, Symbolics env)
runSymbolicsStateT (SymbolicsStateT m) = runStateT m

runExtSymbolicsStateT :: ExtSymbolicsStateT env m a -> ExtSymbolics env -> m (a, ExtSymbolics env)
runExtSymbolicsStateT (ExtSymbolicsStateT m) = runStateT m

runSymbolicsState :: SymbolicsState env a -> Symbolics env -> (a, Symbolics env)
runSymbolicsState (SymbolicsStateT m) = runState m

runExtSymbolicsState :: ExtSymbolicsState env a -> ExtSymbolics env -> (a, ExtSymbolics env)
runExtSymbolicsState (ExtSymbolicsStateT m) = runState m

instance MonadTrans (SymbolicsStateT env) where
  lift = SymbolicsStateT . lift
  
instance MonadTrans (ExtSymbolicsStateT env) where
  lift = ExtSymbolicsStateT . lift
  
-- instance Monad m => MonadState (ExtSymbolics env) (ExtSymbolicsStateT env m) where
--   state = ExtSymbolicsStateT . S.state
  
-- instance Monad m => MonadState (Symbolics env) (SymbolicsStateT env m) where
--   state = SymbolicsStateT . S.state

getSymbolics :: Monad m => SymbolicsStateT env m (Symbolics env)
getSymbolics = SymbolicsStateT S.get

getExtSymbolics :: Monad m => ExtSymbolicsStateT env m (ExtSymbolics env)
getExtSymbolics = ExtSymbolicsStateT S.get

getsSymbolics :: Monad m => ( Symbolics env -> a ) -> SymbolicsStateT env m a
getsSymbolics = SymbolicsStateT . S.gets

getsExtSymbolics :: Monad m => ( ExtSymbolics env -> a ) -> ExtSymbolicsStateT env m a
getsExtSymbolics = ExtSymbolicsStateT . S.gets

instance Monad m => MonadSymbolics env (SymbolicsStateT env m) where
  freshValBool = fmap Symbolic . freshVal . ACVBool
  freshValI32  = fmap Symbolic . freshVal . ACVI32
  freshValI33  = fmap Symbolic . freshVal . ACVI33
  freshValI64  = fmap Symbolic . freshVal . ACVI64
  freshValMem  = fmap Symbolic . freshVal . ACVMem

instance Monad m => MonadSymbolics env (ExtSymbolicsStateT env m) where
  freshValBool = fmap Symbolic . extFreshVal . ECVBase . ACVBool
  freshValI32  = fmap Symbolic . extFreshVal . ECVBase . ACVI32
  freshValI33  = fmap Symbolic . extFreshVal . ECVBase . ACVI33
  freshValI64  = fmap Symbolic . extFreshVal . ECVBase . ACVI64
  freshValMem  = fmap Symbolic . extFreshVal . ECVBase . ACVMem

instance Monad m => MonadExtSymbolics env (ExtSymbolicsStateT env m) where
  freshValWorld = fmap Symbolic . extFreshVal . ECVWorld
  freshValFunc  = fmap Symbolic . extFreshVal . ECVFunc
  freshValF32   = fmap Symbolic . extFreshVal . ECVF32
  freshValF64   = fmap Symbolic . extFreshVal . ECVF64

instance MonadSymbolics env m => MonadSymbolics env (MaybeT m) where
  freshValBool = lift . freshValBool
  freshValI32  = lift . freshValI32
  freshValI33  = lift . freshValI33
  freshValI64  = lift . freshValI64
  freshValMem  = lift . freshValMem

instance MonadExtSymbolics env m => MonadExtSymbolics env (MaybeT m) where
  freshValWorld = lift . freshValWorld
  freshValFunc  = lift . freshValFunc
  freshValF32   = lift . freshValF32
  freshValF64   = lift . freshValF64

instance MonadSymbolics env m => MonadSymbolics env (ReaderT r m) where
  freshValBool = lift . freshValBool
  freshValI32  = lift . freshValI32
  freshValI33  = lift . freshValI33
  freshValI64  = lift . freshValI64
  freshValMem  = lift . freshValMem

instance MonadExtSymbolics env m => MonadExtSymbolics env (ReaderT r m) where
  freshValWorld = lift . freshValWorld
  freshValFunc  = lift . freshValFunc
  freshValF32   = lift . freshValF32
  freshValF64   = lift . freshValF64

instance MonadSymbolics env m => MonadSymbolics env (StateT r m) where
  freshValBool = lift . freshValBool
  freshValI32  = lift . freshValI32
  freshValI33  = lift . freshValI33
  freshValI64  = lift . freshValI64
  freshValMem  = lift . freshValMem

instance MonadExtSymbolics env m => MonadExtSymbolics env (StateT r m) where
  freshValWorld = lift . freshValWorld
  freshValFunc  = lift . freshValFunc
  freshValF32   = lift . freshValF32
  freshValF64   = lift . freshValF64

instance (Monoid w, MonadSymbolics env m) => MonadSymbolics env (RWST r w s m) where
  freshValBool = lift . freshValBool
  freshValI32  = lift . freshValI32
  freshValI33  = lift . freshValI33
  freshValI64  = lift . freshValI64
  freshValMem  = lift . freshValMem

instance (Monoid w, MonadExtSymbolics env m) => MonadExtSymbolics env (RWST r w s m) where
  freshValWorld = lift . freshValWorld
  freshValFunc  = lift . freshValFunc
  freshValF32   = lift . freshValF32
  freshValF64   = lift . freshValF64

freshVal :: Monad m => AnyComposedVal env -> SymbolicsStateT env m Int
freshVal c =
  SymbolicsStateT $
    do
      Symbolics vals <- S.get
      let (i, vals') = IdList.append c vals
      S.put $ Symbolics vals'
      return i
    
extFreshVal :: Monad m => ExtAnyComposedVal env -> ExtSymbolicsStateT env m Int
extFreshVal c =
  ExtSymbolicsStateT $
    do
      ExtSymbolics vals <- S.get
      let (i, vals') = IdList.append c vals
      S.put $ ExtSymbolics vals'
      return i

instance Show (Symbolics env) where
  showsPrec _ (Symbolics s) =
    let valConstrStrs  = map showEntry (IdList.entries s)
    in
    ß "[" . intercalateShow (ß ", ") valConstrStrs . ß "]"
    where
    showEntry :: (Int, AnyComposedVal env) -> ShowS
    showEntry (i, c@(ACVBool _)) = ß "s" . shows i . ß ":b=" . shows c
    showEntry (i, c@(ACVI32 _)) = ß "s" . shows i . ß ":i32=" . shows c
    showEntry (i, c@(ACVI33 _)) = ß "s" . shows i . ß ":i33=" . shows c
    showEntry (i, c@(ACVI64 _)) = ß "s" . shows i . ß ":i64=" . shows c
    -- showEntry (i, c@(ACVF32 _)) = ß "s" . shows i . ß ":f32=" . shows c
    -- showEntry (i, c@(ACVF64 _)) = ß "s" . shows i . ß ":f64=" . shows c
    showEntry (i, c@(ACVMem _)) = ß "s" . shows i . ß ":mem=" . shows c

instance Show (ExtSymbolics env) where
  showsPrec _ (ExtSymbolics s) =
    let valConstrStrs  = map showEntry (IdList.entries s)
    in
    ß "[" . intercalateShow (ß ", ") valConstrStrs . ß "]"
    where
    showEntry :: (Int, ExtAnyComposedVal env) -> ShowS
    showEntry (i, ECVBase c@(ACVBool _)) = ß "s" . shows i . ß ":b=" . shows c
    showEntry (i, ECVBase c@(ACVI32 _)) = ß "s" . shows i . ß ":i32=" . shows c
    showEntry (i, ECVBase c@(ACVI33 _)) = ß "s" . shows i . ß ":i33=" . shows c
    showEntry (i, ECVBase c@(ACVI64 _)) = ß "s" . shows i . ß ":i64=" . shows c
    showEntry (i, ECVBase c@(ACVMem _)) = ß "s" . shows i . ß ":mem=" . shows c
    showEntry (i, ECVF32 c)   = ß "s" . shows i . ß ":f32=" . shows c
    showEntry (i, ECVF64 c)   = ß "s" . shows i . ß ":f64=" . shows c
    showEntry (i, ECVWorld w) = ß "s" . shows i . ß ":w=" . shows w
    showEntry (i, ECVFunc (VFuncUnknown params results)) =
      ß "s" . shows i . ß ":f(" . intercalateShow (ß ",") (map shows params) . ß " -> " . intercalateShow (ß ",") (map shows results) . ß ")"

instance Show (AnyComposedVal env) where
  showsPrec d (ACVBool v) = showsPrec d v
  showsPrec d (ACVI32 v) = showsPrec d v
  showsPrec d (ACVI33 v) = showsPrec d v
  showsPrec d (ACVI64 v) = showsPrec d v
  showsPrec d (ACVMem v) = showsPrec d v

instance Show (Symbolic env a) => Show (ComposedVal env a) where
  -- showsPrec _ (VI32Const (WI32 a)) = justifyZeroHex 8  a . ß "i32"
  -- showsPrec _ (VI64Const (WI64 a)) = justifyZeroHex 16 a . ß "i64"
  -- showsPrec _ (VF32Const (WF32 a)) = justifyZeroHex 8  a . ß "f32"
  -- showsPrec _ (VF64Const (WF64 a)) = justifyZeroHex 16 a . ß "f64"
  
  showsPrec _ VI32Unknown  = ß "?"
  showsPrec _ VI64Unknown  = ß "?"
  showsPrec _ VF32Unknown  = ß "?"
  showsPrec _ VF64Unknown  = ß "?"
  showsPrec _ VBoolUnknown = ß "?"
  showsPrec _ VMemUnknown  = ß "?"
  
  -- I32
  showsPrec d (VI32Unop op a) =
    showParen (d > 10) (shows op . ß " " . showSymbolic a)
  showsPrec d (VI32Binop op a b) =
    showParen (d > 10) (shows op . ß " " . showSymbolic a . ß " " . showSymbolic b)
  showsPrec d (VI32Relop op a b) =
    showParen (d > 10) (shows op . ß " " . showSymbolic a . ß " " . showSymbolic b)
    
  -- I64
  showsPrec d (VI64Unop op a) =
    showParen (d > 10) (shows op . ß " " . showSymbolic a)
  showsPrec d (VI64Binop op a b) =
    showParen (d > 10) (shows op . ß " " . showSymbolic a . ß " " . showSymbolic b)
  showsPrec d (VI64Relop op a b) =
    showParen (d > 10) (shows op . ß " " . showSymbolic a . ß " " . showSymbolic b)
    
  -- F32
  showsPrec d (VF32Unop op a) =
    showParen (d > 10) (shows op . ß " " . showSymbolic a)
  showsPrec d (VF32Binop op a b) =
    showParen (d > 10) (shows op . ß " " . showSymbolic a . ß " " . showSymbolic b)
  showsPrec d (VF32Relop op a b) =
    showParen (d > 10) (shows op . ß " " . showSymbolic a . ß " " . showSymbolic b)
    
  -- F64
  showsPrec d (VF64Unop op a) =
    showParen (d > 10) (shows op . ß " " . showSymbolic a)
  showsPrec d (VF64Binop op a b) =
    showParen (d > 10) (shows op . ß " " . showSymbolic a . ß " " . showSymbolic b)
  showsPrec d (VF64Relop op a b) =
    showParen (d > 10) (shows op . ß " " . showSymbolic a . ß " " . showSymbolic b)
    
  -- Parametric Instructions
  showsPrec d (VSelect cond a b) =
    showParen (d > 10) (ß "select " . showSymbolic cond . ß " " . shows a . ß " " . shows b)

  -- Conversion
  showsPrec d (VI32Extend8S a)       = showParenCvt d CI32Extend8S a
  showsPrec d (VI32Extend16S a)      = showParenCvt d CI32Extend16S a
  showsPrec d (VI64Extend8S a)       = showParenCvt d CI64Extend8S a
  showsPrec d (VI64Extend16S a)      = showParenCvt d CI64Extend16S a
  showsPrec d (VI64Extend32S a)      = showParenCvt d CI64Extend32S a
  showsPrec d (VI32WrapI64 a)        = showParenCvt d CI32WrapI64 a
  showsPrec d (VI64ExtendI32 sx a)   = showParenCvt d (CI64ExtendI32 sx) a
  showsPrec d (VI32TruncF32 sat sx a) = showParenCvt d (CI32TruncF32 sat sx) a
  showsPrec d (VI32TruncF64 sat sx a) = showParenCvt d (CI32TruncF64 sat sx) a
  showsPrec d (VI64TruncF32 sat sx a) = showParenCvt d (CI64TruncF32 sat sx) a
  showsPrec d (VI64TruncF64 sat sx a) = showParenCvt d (CI64TruncF64 sat sx) a
  showsPrec d (VF32DemoteF64 a)      = showParenCvt d CF32DemoteF64 a
  showsPrec d (VF64PromoteF32 a)     = showParenCvt d CF64PromoteF32 a
  showsPrec d (VF32ConvertI32 sx a)  = showParenCvt d (CF32ConvertI32 sx) a
  showsPrec d (VF32ConvertI64 sx a)  = showParenCvt d (CF32ConvertI64 sx) a
  showsPrec d (VF64ConvertI32 sx a)  = showParenCvt d (CF64ConvertI32 sx) a
  showsPrec d (VF64ConvertI64 sx a)  = showParenCvt d (CF64ConvertI64 sx) a
  showsPrec d (VI32ReinterpretF32 a) = showParenCvt d CI32ReinterpretF32 a
  showsPrec d (VI64ReinterpretF64 a) = showParenCvt d CI64ReinterpretF64 a
  showsPrec d (VF32ReinterpretI32 a) = showParenCvt d CF32ReinterpretI32 a
  showsPrec d (VF64ReinterpretI64 a) = showParenCvt d CF64ReinterpretI64 a

  -- Memory Operators
  showsPrec d (VI32Load mem a) = showParen (d > 10) (ß "i32.load " . shows mem . ß " " . shows a)
  showsPrec d (VI64Load mem a) = showParen (d > 10) (ß "i64.load " . shows mem . ß " " . shows a)
  showsPrec d (VF32Load mem a) = showParen (d > 10) (ß "f32.load " . shows mem . ß " " . shows a)
  showsPrec d (VF64Load mem a) = showParen (d > 10) (ß "f64.load " . shows mem . ß " " . shows a)
  showsPrec d (VI32Load8 sx mem a) = showParen (d > 10) (ß "i32.load8_" . shows sx . ß " " . shows mem . ß " " . shows a)
  showsPrec d (VI64Load8 sx mem a) = showParen (d > 10) (ß "i64.load8_" . shows sx . ß " " . shows mem . ß " " . shows a)
  showsPrec d (VI32Load16 sx mem a) = showParen (d > 10) (ß "i32.load16_" . shows sx . ß " " . shows mem . ß " " . shows a)
  showsPrec d (VI64Load16 sx mem a) = showParen (d > 10) (ß "i64.load16_" . shows sx . ß " " . shows mem . ß " " . shows a)
  showsPrec d (VI64Load32 sx mem a) = showParen (d > 10) (ß "i64.load32_" . shows sx . ß " " . shows mem . ß " " . shows a)

  showsPrec d (VI32Store mem a v)   = showParen (d > 10) (ß "i32.store " . shows mem . ß " " . shows a . ß " " . shows v)
  showsPrec d (VI64Store mem a v)   = showParen (d > 10) (ß "i64.store " . shows mem . ß " " . shows a . ß " " . shows v)
  showsPrec d (VF32Store mem a v)   = showParen (d > 10) (ß "f32.store " . shows mem . ß " " . shows a . ß " " . shows v)
  showsPrec d (VF64Store mem a v)   = showParen (d > 10) (ß "f64.store " . shows mem . ß " " . shows a . ß " " . shows v)
  showsPrec d (VI32Store8 mem a v)  = showParen (d > 10) (ß "i32.store8_" . ß " " . shows mem . ß " " . shows a . ß " " . shows v)
  showsPrec d (VI64Store8 mem a v)  = showParen (d > 10) (ß "i64.store8_" . ß " " . shows mem . ß " " . shows a . ß " " . shows v)
  showsPrec d (VI32Store16 mem a v) = showParen (d > 10) (ß "i32.store16_" . ß " " . shows mem . ß " " . shows a . ß " " . shows v)
  showsPrec d (VI64Store16 mem a v) = showParen (d > 10) (ß "i64.store16_" . ß " " . shows mem . ß " " . shows a . ß " " . shows v)
  showsPrec d (VI64Store32 mem a v) = showParen (d > 10) (ß "i64.store32_" . ß " " . shows mem . ß " " . shows a . ß " " . shows v)

  -- Extra
  showsPrec _ VI33Unknown = ß "?"
  -- showsPrec _ (VI33Const x) = shows x . ß "i33"
  showsPrec d (VI33Address x 0)   = showParen (d > 10) (ß "addr " . showSymbolic x)
  showsPrec d (VI33Address x off) = showParen (d > 10) (ß "addr " . showSymbolic x . ß " " . shows off)
  showsPrec d (VI33Lt a b) = showParen (d > 10) (ß "i32.lt " . showSymbolic a . ß " " . showSymbolic b)
  showsPrec d (VI33MulPage v) = showParen (d > 10) (ß "i33.mulpage " . showSymbolic v)
  -- showsPrec _ (VBoolConst b) = shows b
  -- showsPrec d (VBEqMem a b) = showParen (d > 10) (ß "mem.eq " . showSymbolic a . ß " " . showSymbolic b)
  -- showsPrec d (VBEqWorld a b) = showParen (d > 10) (ß "world.eq " . showSymbolic a . ß " " . showSymbolic b)
  showsPrec d (VBAnd a b) = showParen (d > 10) (ß "b.and " . showSymbolic a . ß " " . showSymbolic b)
  showsPrec d (VBOr a b) = showParen (d > 10) (ß "b.or " . showSymbolic a . ß " " . showSymbolic b)
  showsPrec d (VBImplies a b) = showParen (d > 10) (ß "b.implies " . showSymbolic a . ß " " . showSymbolic b)
  showsPrec d (VBNot a) = showParen (d > 10) (ß "b.not " . showSymbolic a)
  showsPrec d (VBI32 a) = showParen (d > 10) (ß "b.fromI32 " . showSymbolic a)
  showsPrec d (VI32Bool a) = showParen (d > 10) (ß "i32.fromBool " . showSymbolic a)
  showsPrec _ VWorldUnknown = ß "?"
  showsPrec d (VMemGrowWorld w size) = showParen (d > 10) (ß "w.memgrow " . shows w . ß " " . shows size)
  showsPrec d (VMemGrowBool w size) = showParen (d > 10) (ß "b.memgrow " . shows w . ß " " . shows size)
  showsPrec d (VFuncUnknown params results) =
    showParen (d > 10) (ß "func " . intercalateShow (ß ",") (map shows params) . ß " -> " . intercalateShow (ß ",") (map shows results))
  showsPrec d (VFuncCallWorld f w m s xs) =
    showParen (d > 10) (ß "w.call " . shows f . ß " " . shows w . ß " " . shows m . ß " " . shows s . ß " (" . intercalateShow (ß ",") (map shows xs) . ß ")")
  showsPrec d (VFuncCallMem f w m s xs) =
    showParen (d > 10) (ß "mem.call " . shows f . ß " " . shows w . ß " " . shows m . ß " " . shows s . ß " (" . intercalateShow (ß ",") (map shows xs) . ß ")")
  showsPrec d (VFuncCallMemSize f w m s xs) =
    showParen (d > 10) (ß "size.call " . shows f . ß " " . shows w . ß " " . shows m . ß " " . shows s . ß " (" . intercalateShow (ß ",") (map shows xs) . ß ")")
  showsPrec d (VFuncCallResultI32 f w m s xs i) =
    showParen (d > 10) (ß "i32.call " . shows f . ß " " . shows w . ß " " . shows m . ß " " . shows i . ß " " . shows s . ß " (" . intercalateShow (ß ",") (map shows xs) . ß ")")
  showsPrec d (VFuncCallResultI64 f w m s xs i) =
    showParen (d > 10) (ß "i64.call " . shows f . ß " " . shows w . ß " " . shows m . ß " " . shows i . ß " " . shows s . ß " (" . intercalateShow (ß ",") (map shows xs) . ß ")")
  showsPrec d (VFuncCallResultF32 f w m s xs i) =
    showParen (d > 10) (ß "f32.call " . shows f . ß " " . shows w . ß " " . shows m . ß " " . shows i . ß " " . shows s . ß " (" . intercalateShow (ß ",") (map shows xs) . ß ")")
  showsPrec d (VFuncCallResultF64 f w m s xs i) =
    showParen (d > 10) (ß "f64.call " . shows f . ß " " . shows w . ß " " . shows m . ß " " . shows i . ß " " . shows s . ß " (" . intercalateShow (ß ",") (map shows xs) . ß ")")

showParenCvt :: Int -> Cvtop -> Symbolic env a -> ShowS
showParenCvt d op v =
  showParen (d > 10) (shows (InstrSimple $ SCvtop op) . ß " " . showSymbolic v)

data Prec
  = PrecAnd
  | PrecOr
  | PrecHigh -- Always put parenthesis for non-atoms
  | PrecLow -- Don't put parenthesis
  deriving Eq

showSymbolic :: Symbolic env a -> ShowS
showSymbolic (Symbolic a) = ß "s" . shows a
showSymbolic (SymConstBool b) = shows b
showSymbolic (SymConstI32 (WI32 i)) = shows i
showSymbolic (SymConstI33 i) = shows i
showSymbolic (SymConstI64 (WI64 i)) = shows i
showSymbolic (SymConstF32 (WF32 i)) = shows "f32#" . shows i
showSymbolic (SymConstF64 (WF64 i)) = shows "f64#" . shows i

instance Show (Symbolic env Bool) where
  showsPrec _ (Symbolic a)  = ß "s" . shows a . ß ":Bool"
  showsPrec _ (SymConstBool b) = shows b

instance Show (Symbolic env TI32) where
  showsPrec _ (Symbolic a) = ß "s" . shows a . ß ":i32"
  showsPrec _ (SymConstI32 (WI32 i)) = shows i
  
instance Show (Symbolic env TI33) where
  showsPrec _ (Symbolic a) = ß "s" . shows a . ß ":i33"
  showsPrec _ (SymConstI33 i) = shows i
  
instance Show (Symbolic env TI64) where
  showsPrec _ (Symbolic a) = ß "s" . shows a . ß ":i64"
  showsPrec _ (SymConstI64 (WI64 i)) = shows i
  
instance Show (Symbolic env TF32) where
  showsPrec _ (Symbolic a) = ß "s" . shows a . ß ":f32"
  showsPrec _ (SymConstF32 (WF32 i)) = shows "f32#" . shows i
  
instance Show (Symbolic env TF64) where
  showsPrec _ (Symbolic a) = ß "s" . shows a . ß ":f64"
  showsPrec _ (SymConstF64 (WF64 i)) = shows "f64#" . shows i

instance Show (Symbolic env TMem) where
  showsPrec _ (Symbolic a) = ß "s" . shows a . ß ":mem"
  
instance Show (Symbolic env TWorld) where
  showsPrec _ (Symbolic a) = ß "s" . shows a . ß ":world"

instance Show (Symbolic env TFunc) where
  showsPrec _ (Symbolic a) = ß "s" . shows a . ß ":func"
