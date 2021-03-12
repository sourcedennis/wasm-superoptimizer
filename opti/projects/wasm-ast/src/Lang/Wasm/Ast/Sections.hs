{-# LANGUAGE DeriveGeneric, StrictData #-}

-- | Implementation of:
-- <https://webassembly.github.io/spec/core/syntax/modules.html>
module Lang.Wasm.Ast.Sections where

import Melude
-- Stdlib imports
import           GHC.Generics ( Generic )
import           Control.DeepSeq ( NFData )
import           Data.Word ( Word8 )
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty ((:|)) )
-- Extra stdlib imports
import           Data.ByteString (ByteString)
import           Data.Hashable (Hashable)
import qualified Data.Vector as Vector
import           Data.Vector ( Vector )
import           Data.Vector.Instances ()
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
-- Local imports
import           Lang.Wasm.Ast.Indices
  (FuncIdx, TypeIdx, TableIdx, LabelIdx, MemIdx, GlobalIdx)
import           Lang.Wasm.Ast.Types
  (FuncType, TableType, GlobalType, Limits, ValType)
import           Lang.Wasm.Ast.Instrs
  (Expr, Instr (..), SimpleInstr (..))
import           Lang.Wasm.Ast.Values
  (WName, WI32 (..))

-- Note that data regions are collapsed in to the `DefinedMem` structures.
-- Similarly for element sections in tables. Type indices are dropped altogether.
-- They are embedded in the relevant structures.
data Module =
  Module {
    mFuncsImported    :: !(Vector ImportedFunc)
  , mFuncsDefined     :: !(Vector DefinedFunc)
  , mMemsImported     :: !(Vector ImportedMem)
  , mMemsDefined      :: !(Vector DefinedMem)
  , mTablesImported   :: !(Vector ImportedTable)
  , mTablesDefined    :: !(Vector DefinedTable)
  , mGlobalsImported  :: !(Vector ImportedGlobal)
  , mGlobalsDefined   :: !(Vector DefinedGlobal)
  , mExports          :: !(Vector Export)
  , mStart            :: !(Maybe FuncIdx)
  }
  deriving (Eq, Show, Generic)

instance NFData Module where
instance NFData ImportedMem where
instance NFData DefinedMem where
instance NFData ImportedTable where
instance NFData DefinedTable where


-- # Functions #

data Func
  = FuncImported !ImportedFunc
  | FuncDefined !DefinedFunc
  deriving (Eq, Show, Generic)

data ImportedFunc = ImportedFunc !ImportInfo !FuncType
  deriving (Eq, Show, Generic)

-- The function type is split up. While the type is: params -> results, the
-- params are also the first locals. So, when operating on the function body,
-- convenient access to locals is crucial.
data DefinedFunc =
  DefinedFunc {
    -- | The parameters and locals of the function are referenced through
    -- 0-based local indices in the function’s body; they are mutable.
    fType     :: !FuncType
    -- Note that the spec places _no bound_ on the number of locals. Occasional
    -- functions exist containing _hundreds_ of local variables.
    -- Does *not* include the parameters
  , fLocals   :: !(Vector ValType)
    -- | The body is an instruction sequence that upon termination must produce
    -- a stack matching the function type’s result type.
  , fBody     :: !Expr
  }
  deriving (Eq, Show, Generic)
  

-- # Memories #

data Mem
  = MemImported !ImportedMem
  | MemDefined !DefinedMem
  deriving (Eq, Show, Generic)

data ImportedMem = ImportedMem !ImportInfo !DefinedMem
  deriving (Eq, Show, Generic)

-- Note that memory is often sparsely filled. That is, it has large regions
-- containing only zeroes. So, store its content as an 'IntMap'.
-- However, occasionally, the content of memory is only known at instantiation
-- time. E.g., when a data region starts a variable offset 'x', that is provided
-- upon instantiation. (The spec allows this, but few WASM generators currently
-- do this)
data DefinedMem =
  DefinedMem {
    dmLimits  :: !Limits
  , dmDatas   :: ![(Expr,ByteString)]
  }
  deriving (Eq, Show, Generic)


-- # Tables #

data Table
  = TableImported !ImportedTable
  | TableDefined !DefinedTable
  deriving (Eq, Show, Generic)

data ImportedTable = ImportedTable !ImportInfo !DefinedTable
  deriving (Eq, Show, Generic)

data DefinedTable =
  DefinedTable {
    tType   :: !Limits
  , tElems  :: ![(Expr,[FuncIdx])]
  }
  deriving (Eq, Show, Generic)


-- # Globals #

data Global
  = GlobalImported !ImportedGlobal
  | GlobalDefined !DefinedGlobal
  deriving (Eq, Show, Generic)

data ImportedGlobal = ImportedGlobal !ImportInfo !GlobalType
  deriving (Eq, Show, Generic)

data DefinedGlobal =
  DefinedGlobal {
    gType  :: !GlobalType
  , gInit  :: !Expr
  }
  deriving (Eq, Show, Generic)


-- # Imports #

data ImportInfo =
  ImportInfo {
    imModule  :: !WName
  , imName    :: !WName
  }
  deriving (Eq, Show, Generic)

data Import
  = ImportDescFunc !ImportedFunc
  | ImportDescTable !ImportedTable
  | ImportDescMem !ImportedMem
  | ImportDescGlobal !ImportedGlobal


-- # Exports #

data Export =
  Export {
    exName  :: !WName
  , exDesc  :: !ExportDesc
  }
  deriving (Eq, Show, Generic)

data ExportDesc
  = ExportDescFunc !FuncIdx
  | ExportDescTable !TableIdx
  | ExportDescMem !MemIdx
  | ExportDescGlobal !GlobalIdx
  deriving (Eq, Show, Generic)


instance Hashable ImportInfo
instance Hashable ImportedFunc
instance Hashable DefinedFunc
instance Hashable Func
instance Hashable ImportedGlobal
instance Hashable DefinedGlobal
instance Hashable Global
instance Hashable Export
instance Hashable ExportDesc

instance NFData ImportInfo
instance NFData ImportedFunc
instance NFData DefinedFunc
instance NFData Func
instance NFData ImportedGlobal
instance NFData DefinedGlobal
instance NFData Global
instance NFData Export
instance NFData ExportDesc
