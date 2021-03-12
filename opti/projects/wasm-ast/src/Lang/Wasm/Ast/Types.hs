{-# LANGUAGE DeriveGeneric, UnicodeSyntax, StrictData #-}

-- | Implementation of:
-- <https://webassembly.github.io/spec/core/syntax/types.html>
module Lang.Wasm.Ast.Types where

import Melude
-- Stdlib imports
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.DeepSeq ( NFData )
-- Extra stdlib imports
import qualified Data.Vector as Vector
import           Data.Vector ( Vector )
import Data.Vector.Instances
-- Local imports
import Lang.Wasm.Ast.Values ( WI32, WI64, WF32, WF64 )


-- | Value types classify the individual values that WebAssembly code can
-- compute with and the values that a variable accepts.
data ValType
  = TI32
  | TI64
  | TF32
  | TF64
  deriving (Eq, Show, Generic)


-- * Type markers
-- Might just as well re-use the value types for the type markers

-- | Type marker for i32
type TI32 = WI32

-- | Type marker for i64
type TI64 = WI64

-- | Type marker for f32
type TF32 = WF32

-- | Type marker for f64
type TF64 = WF64

-- | Result types classify the result of executing instructions or functions,
-- which is a sequence of values written with brackets.
type ResultType = Vector ValType

-- | Function types classify the signature of functions, mapping a vector of
-- parameters to a vector of results. They are also used to classify the inputs
-- and outputs of instructions.
--
-- WARNING: In the specification the stack grows to the /right/; So do these
--   types. In the rest of the implementation, most stacks grow to the /left/
--   (as those are lists, whose head is left).
data FuncType =
  FuncType {
    ftParams   :: ResultType
  , ftResults  :: ResultType
  }
  deriving (Eq, Generic)

-- | Limits classify the size range of resizeable storage associated with memory
-- types and table types.
--
-- While memory can be declared unbounded, no WASM instruction can currently
-- address memory beyond 8 GiB (32-bit address + 32-bit offset). This
-- effectively limits the number of 64KiB pages to 131,072.
data Limits =
  Limits {
    lMin  :: Integer
  , lMax  :: Maybe Integer
  }
  deriving (Eq, Show, Generic)

-- | Memory types classify linear memories and their size range.
-- newtype MemType = MemType Limits
--   deriving (Eq, Show, Generic)

-- | Table types classify tables over elements of element types within
-- a size range.
data TableType = TableType Limits ElemType
  deriving (Eq, Show, Generic)

-- | Note: In future versions of WebAssembly, additional element types may be
-- introduced.
data ElemType = FuncRef
  deriving (Eq, Show, Generic)

-- | Global types classify global variables, which hold a value and can either
-- be mutable or immutable.
data GlobalType =
  GlobalType {
    gtMut   :: !Mut
  , gtType  :: !ValType
  }
  deriving (Eq, Show, Generic)

data Mut
  = MConst
  | MVar
  deriving (Eq, Show, Generic)

instance Hashable ValType
instance Hashable FuncType
instance Hashable Limits
instance Hashable TableType
instance Hashable ElemType
instance Hashable GlobalType
instance Hashable Mut

instance NFData ValType
instance NFData FuncType
instance NFData Limits
instance NFData TableType
instance NFData ElemType
instance NFData GlobalType
instance NFData Mut

instance Show FuncType where
  showsPrec d ft =
    showParen (d > 10)
      (showTuple (Vector.toList $ ftParams ft) . ß "->" . showTuple (Vector.toList $ ftResults ft))
    where
    showTuple :: [ValType] -> ShowS
    showTuple [x] = shows x
    showTuple xs  = ß "(" . intercalateShow (ß ",") (map shows xs) . ß ")"
