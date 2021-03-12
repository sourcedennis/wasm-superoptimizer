{-# LANGUAGE UnicodeSyntax, DeriveGeneric, TemplateHaskell #-}

-- | Data structures for a concrete representation of a WebAssembly state.
--
-- This representation is explicitly /not/ conformant with the specification on
-- two points:
-- * The stack does not contain stack frames. Instead, the stack is defined
--   within the context of a single function. This is mainly a performance
--   consideration (for the symbolic counterpart), as proving equality over
--   large stacks with multiple activations is too expensive.
--
-- It is advisable to import this module /qualified/, as the record names
-- mostly overlap with its symbolic counterpart, `Lang.Wasm.Symbolic.ProgState`.
module Lang.Wasm.Concrete.ProgState
  ( -- * Data structures
    ProgState (..)
  , LocalState (..)
  , Activation (..)
  , Stack (..)
  , Mem (..)
  , Global (..)
    -- * Lenses
  , cLocalState, cGlobals, cMem
  , cActivation, cStack
  , cmDefault, cmData, cmPageCount, cmLimits
  ) where

import Melude
-- Stdlib imports
import           GHC.Generics ( Generic )
import           Data.Word ( Word8 )
-- Extra stdlib imports
import           Data.Hashable ( Hashable (..) )
import qualified Data.IntMap.Strict as IntMap
import           Data.IntMap.Strict ( IntMap )
-- External library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
-- Local library imports
import qualified Lang.Wasm.Data as WD
import           Lang.Wasm.Data ( PStack, PActivation, PGlobal )
import           Lang.Wasm.Ast ( Val, Mut, WI32, WI64, WF32, WF64, Limits )


-- | WebAssembly program state
data ProgState =
  ProgState {
    _cLocalState  :: Maybe LocalState
  , _cGlobals     :: IdList Global
  , _cMem         :: Maybe Mem
  }
  deriving ( Show, Generic, Eq )

-- | The fragment of a program state that is local to an (untrapped) execution.
--
-- Unconditionally trapped executions do not have a local state, as the stack is
-- not further transformed.
data LocalState =
  LocalState {
    _cActivation  :: Activation
  , _cStack       :: Stack
  }
  deriving ( Show, Generic, Eq )

-- | A bounded WebAssembly memory block. Its size is always a multiple of 64KiB,
-- which is the page size.
data Mem =
  Mem {
    -- | Default value in memory, which is assigned to any value not in the
    -- `HashMap`.
    _cmDefault    :: Word8
  , -- | The contents of the memory block, which maps memory addresses (33-bit)
    -- to values (8-bit).
    _cmData       :: IntMap Word8
  , -- | The number of memory pages (of size 64KiB)
    _cmPageCount  :: WI32
  , -- | The /static/ bound on the number of memory pages.
    _cmLimits     :: Limits
  }
  deriving ( Show, Generic, Eq )

-- | A stack frame, which contains the variables of the active function.
type Activation = PActivation WI32 WI64 WF32 WF64

-- | The program stack within a function
type Stack = PStack WI32 WI64 WF32 WF64

-- | The program stack within a function
type Global = PGlobal WI32 WI64 WF32 WF64

$(makeLenses ''ProgState)
$(makeLenses ''LocalState)
$(makeLenses ''Mem)

instance Hashable ProgState

instance Hashable Mem where
  i `hashWithSalt` m =
    i `hashWithSalt` (m^.cmDefault) `hashWithSalt` IntMap.assocs (m^.cmData)
      `hashWithSalt` (m^.cmPageCount) `hashWithSalt` (m^.cmLimits)

instance Hashable LocalState
