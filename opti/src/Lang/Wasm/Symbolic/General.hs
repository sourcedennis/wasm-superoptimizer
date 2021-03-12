{-# LANGUAGE UnicodeSyntax, TemplateHaskell, RankNTypes, MultiParamTypeClasses,
             FlexibleContexts
#-}

-- | Structures shared between the configuration and symbolic state.
--
-- In `Lang.Wasm.Symbolic.ExecSimple`, symbolic execution of simple instructions
-- is defined over this structure (as this is the same for states and
-- configurations).
module Lang.Wasm.Symbolic.General
  ( -- * Data Structures
    -- ** Symbolic Structures
    SymbolicAddress
    -- ** Static Structures
  , StaticStructure (..)
  , LocalStaticStructure (..)
  , SymbolicMem (..)
    -- *** Lenses
  , smData, smPageCount, smLimits
    -- * Type Classes
  , MonadSymbolicWasm (..)
    -- * Functions
  , topMem
  , boundAddr
  ) where

import Melude
-- -- Local library imports
import Lang.Wasm.Ast ( Limits, TF64, TF32, TI64, TI32 )
import           Lang.Wasm.Data ( PActivation, PStack, PGlobals )
-- Local imports
import qualified Lang.Wasm.Symbolics as Y
import           Lang.Wasm.Symbolics ( Symbolic, MonadSymbolics )
import           Lang.Wasm.Markers ( TI33, TMem )
import           Lang.Wasm.Algebra ( Address (..), MonadWasmState (..) )

-- # Data Structures #

-- ## Symbolic Structures ##

type SymbolicAddress env = Address (Symbolic env TI32)


-- ## Static Structure ##

data StaticStructure =
  StaticStructure {
    stLocalState  :: Maybe LocalStaticStructure
  , stGlobals     :: PGlobals () () () ()
  , stHasMem      :: Bool
  }

data LocalStaticStructure =
  LocalStaticStructure {
    lstActivation  :: PActivation () () () ()
  , lstStack       :: PStack () () () ()
  }

-- | A symbolic representation of bounded memory.
--
-- Note that `Symbolic env TMem` is /unbounded/, as it it an uninterpreted
-- function with type @I33 -> I8@. The page count is used to constrain the
-- memory addresses with which memory is accessed. The page size is 64KiB,
-- by the WASM spec.
data SymbolicMem env =
  SymbolicMem {
    _smData       :: Symbolic env TMem
  , _smPageCount  :: Symbolic env TI32
  , _smLimits     :: Limits
  }
  deriving Show

$(makeLenses ''SymbolicMem)

-- |
--
-- The top of the memory lattice. The static memory structure (i.e., Limits)
-- are preserved.
topMem :: MonadSymbolics env m => SymbolicMem env -> m (SymbolicMem env)
topMem m =
  do
    d <- Y.vMemUnknown
    -- Technically, it is known that the memory size will never shrink.
    -- Ignore for now.
    pc <- Y.vI32Unknown
    return $ SymbolicMem d pc (m^.smLimits)


-- # Type Classes #

class MonadWasmState
        (Symbolic env TI32)
        (Symbolic env TI64)
        f32 f64 mem m
      => MonadSymbolicWasm env f32 f64 mem m where
  trapIf :: Symbolic env Bool -> m ()


-- # Functions #

boundAddr :: MonadSymbolics env m
          => SymbolicAddress env
          -> Symbolic env TI32
          -> m (Symbolic env Bool, Symbolic env TI33)
boundAddr (Address addrVar off) numPages =
  do
    addr <- Y.vI33Address addrVar off
    numBytes <- Y.vI33MulPage numPages
    isOk <- Y.vI33Lt addr numBytes
    return (isOk, addr)


-- # Symbolic operations

-- -- | Symbolically selects the `Activation` based on the (symbolic) condition.
-- --
-- -- Intuitively, @select True a b = a@, @select False a b = b@
-- --
-- -- This fails over the `MaybeT` if the activations have mismatching static
-- -- structures.
-- selectActivation :: Selection env (Activation env)
-- selectActivation c a b =
--   do
--     params <- IdList.zipWithExactM (selectVal c) (a^.saParams) (b^.saParams)
--     locals <- IdList.zipWithExactM (selectVal c) (a^.saLocals) (b^.saLocals)
--     return $ Activation params locals

-- -- | Symbolically selects the `Stack` based on the (symbolic) condition.
-- --
-- -- Intuitively, @select True a b = a@, @select False a b = b@
-- --
-- -- This fails over the `MaybeT` if the stacks have mismatching static
-- -- structures.
-- selectStack :: Selection env (Stack env)
-- selectStack c = zipWithExactNEM (zipWithExactM (selectVal c))

-- -- | Symbolically selects the `Mem` based on the (symbolic) condition.
-- --
-- -- Intuitively, @select True a b = a@, @select False a b = b@
-- --
-- -- This fails over the `MaybeT` if the memories have mismatching static
-- -- structures.
-- selectMem :: Selection env (Mem env)
-- selectMem c a b =
--   do
--     failIf ((a^.smLimits) /= (b^.smLimits)) "Mismatching static structures"
--     data' <- lift (Y.vMemSelect c (a^.smData) (b^.smData))
--     pageCount' <- lift (Y.vI32Select c (a^.smPageCount) (b^.smPageCount))
--     return $ Mem data' pageCount' (a^.smLimits)

-- -- | Symbolically selects the `Global` based on the (symbolic) condition.
-- --
-- -- Intuitively, @select True a b = a@, @select False a b = b@
-- --
-- -- This fails over the `MaybeT` if the globals have mismatching static
-- -- structures (value types or mutability marker).
-- selectGlobal :: Selection env (Global env)
-- selectGlobal c a b =
--   do
--     failIf ((a^.sgMut) /= (b^.sgMut)) "Mismatching static structures"
--     val' <- selectVal c (a^.sgVal) (b^.sgVal)
--     return $ Global (a^.sgMut) val'

-- -- | Symbolically selects the `Activation` based on the (symbolic) condition.
-- --
-- -- Intuitively, @select True a b = a@, @select False a b = b@
-- --
-- -- This fails over the `MaybeT` if the values have mismatching types.
-- selectVal :: Selection env (SymbolicVal env)
-- selectVal c =
--   zipValM (Y.vI32Select c) (Y.vI64Select c) (Y.vF32Select c) (Y.vF64Select c)


-- -- # Static Structure

-- matchStaticStructures :: StaticStructure -> StaticStructure -> Bool
-- matchStaticStructures a b =
--   matchLocal (stLocalState a) (stLocalState b) &&
--   stGlobals a == stGlobals b &&
--   stHasMem a == stHasMem b
--   where
--   matchLocal :: Maybe LocalStaticStructure -> Maybe LocalStaticStructure -> Bool
--   matchLocal (Just a) (Just b) =
--     lstActivation a == lstActivation b &&
--     lstStack a == lstStack b
--   -- | If a program has unconditionally trapped, it matches any structure
--   matchLocal _ _ = True
