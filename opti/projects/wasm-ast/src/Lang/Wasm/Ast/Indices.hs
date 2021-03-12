{-# LANGUAGE DeriveGeneric, StrictData #-}

module Lang.Wasm.Ast.Indices where

import Data.Hashable (Hashable)
import GHC.Generics ( Generic )
import Control.DeepSeq ( NFData )


-- | References a function type in the list-of-types (e.g. @(f32,f32) -> (f32)@)
newtype TypeIdx   = TypeIdx { unTypeIdx :: Int }
  deriving (Eq, Generic)

-- | References a function in the function table.
newtype FuncIdx   = FuncIdx { unFuncIdx :: Int }
  deriving (Eq, Generic)

-- | References a table element
newtype TableIdx  = TableIdx { unTableIdx :: Int }
  deriving (Eq, Generic)

-- | References a memory block
newtype MemIdx    = MemIdx { unMemIdx :: Int }
  deriving (Eq, Generic)

-- | References a global variable
newtype GlobalIdx = GlobalIdx { unGlobalIdx :: Int }
  deriving (Eq, Generic)

-- | References a local variable
newtype LocalIdx  = LocalIdx { unLocalIdx :: Int }
  deriving (Eq, Generic)

-- | References an outward label in control flow (akin to DeBruijn index)
newtype LabelIdx  = LabelIdx { unLabelIdx :: Int }
  deriving (Eq, Generic)

instance Hashable TypeIdx
instance Hashable FuncIdx
instance Hashable TableIdx
instance Hashable MemIdx
instance Hashable GlobalIdx
instance Hashable LocalIdx
instance Hashable LabelIdx

instance NFData TypeIdx
instance NFData FuncIdx
instance NFData TableIdx
instance NFData MemIdx
instance NFData GlobalIdx
instance NFData LocalIdx
instance NFData LabelIdx

instance Show TypeIdx where
  showsPrec _ (TypeIdx idx) = showString "type" . shows idx

instance Show FuncIdx where
  showsPrec _ (FuncIdx idx) = showString "func" . shows idx
  
instance Show TableIdx where
  showsPrec _ (TableIdx idx) = showString "table" . shows idx
  
instance Show MemIdx where
  showsPrec _ (MemIdx idx) = showString "mem" . shows idx
  
instance Show GlobalIdx where
  showsPrec _ (GlobalIdx idx) = showString "global" . shows idx
  
instance Show LocalIdx where
  showsPrec _ (LocalIdx idx) = showString "local" . shows idx
  
instance Show LabelIdx where
  showsPrec _ (LabelIdx idx) = showString "label" . shows idx
