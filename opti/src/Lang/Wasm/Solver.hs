{-# LANGUAGE RankNTypes #-}

-- | General solver monad for the superoptimizer.
--
-- (You are probably looking for `Binding.Z3.Z3Solver`)
module Lang.Wasm.Solver where

import Melude
-- Local library imports
import Lang.Wasm.Ast
  ( TI32, TI64, TF32, TF64
  , WI32, WI64, WF32, WF64
  , Val
  )
-- Local imports
import Lang.Wasm.Symbolics
  ( Symbolics, Symbolic, SymbolicVal, ExtSymbolics, Environment, ExtEnvironment
  , SymbolicsStateT, ExtSymbolicsStateT
  )
import Lang.Wasm.Symbolic.Configuration ( Configuration )
import qualified Lang.Wasm.Symbolic.ProgramState as SymP
import           Lang.Wasm.Symbolic.ProgramState ( SymbolicProgramState )
import qualified Lang.Wasm.Concrete.ProgState as ConP
import           Lang.Wasm.Dataflow.Liveness ( LivenessState )


data EvalResult a
  = EvUnsat
  | EvUnknown
  | EvSat a
  deriving (Show, Eq)


-- | A general description of a solver for WASM Symbolics. All its functions
-- *must*  be sound, but need not be complete. That is, a positive instance is
-- always correct, but false negatives may be returned.
class Monad m => MonadSolver m where
  -- # Functions for Configurations

  -- | Returns `True` if the first configuration is a (non-strict) subset of
  -- the second. Returns `False` if not or unknown.
  --
  -- The first configuration is a subset of the second if every state contained
  -- in the first is also contained in the second.
  isSubset :: Configuration a -> Configuration b -> m Bool
  -- isSubset _ _ = return False

  -- | Generates a concrete program state that is (possibly) contained in the
  -- configuration.
  pickElement :: Configuration a -> m (Maybe ConP.ProgState)
  -- pickElement _ = return Nothing

  -- | Returns `True` if the configuration is empty (i.e., contains no states).
  -- Returns `False` if /not/ empty or this is unknown.
  isEmpty :: Configuration a -> m Bool
  -- isEmpty _ = return False

  -- | Returns `True` iff the I32 is /always/ non-zero. So, if the value is
  -- /possibly/ zero, `False` is returned.
  isNonZeroI32  :: Environment env -> Symbolic env TI32 -> m Bool
  -- isNonZeroI32 _ _ = return False

  -- | Evaluates the symbolic value to its /unique/ concrete value. This only
  -- produces a satisfying result if it has exactly one concrete value.
  evalI32  :: Environment env -> Symbolic env TI32 -> m (EvalResult WI32)
  -- evalI32 _ _ = return EvUnknown

  -- | Evaluates the symbolic value to its /unique/ concrete value. This only
  -- produces a satisfying result if it has exactly one concrete value.
  evalI64  :: Environment env -> Symbolic env TI64 -> m (EvalResult WI64)
  -- evalI64 _ _ = return EvUnknown

  evalBool  :: Environment env -> Symbolic env TI32 -> m (EvalResult Bool)
  

  -- # Functions for symbolic states

  -- | Returns a counter-example (in `EvSat`) if the programs are unequal;
  -- though, it may be missing if a counter example cannot be represented.
  -- (e.g., by floating points)
  areUnequal :: ExtEnvironment env
             -> SymbolicProgramState env -- ^ Input state
             -> SymbolicProgramState env -- ^ Output state 1
             -> SymbolicProgramState env -- ^ Output state 2
             -> LivenessState      -- ^ Output liveness state
             -> m (EvalResult (Maybe ConP.ProgState))
  -- areUnequal _ _ _ _ = return EvUnknown

  -- | Returns `True` if the state is unsatisfiable, which indicates it
  -- represents no concrete executions.
  --
  -- Note that some unsatisfiable states may return `False`, which is sound.
  -- (This may happen with floats - and causes some unreachable branches to be
  -- traversed)
  isUnsat :: ExtEnvironment env -> m Bool
  -- isUnsat _ _ = return False

instance MonadSolver m => MonadSolver (MaybeT m) where
  isSubset     = lift .:   isSubset
  pickElement  = lift .    pickElement
  isEmpty      = lift .    isEmpty
  isNonZeroI32 = lift .:   isNonZeroI32
  evalI32      = lift .:   evalI32
  evalI64      = lift .:   evalI64
  evalBool     = lift .:   evalBool
  areUnequal   = lift .::. areUnequal
  isUnsat      = lift .    isUnsat

instance MonadSolver m => MonadSolver (StateT s m) where
  isSubset     = lift .:   isSubset
  pickElement  = lift .    pickElement
  isEmpty      = lift .    isEmpty
  isNonZeroI32 = lift .:   isNonZeroI32
  evalI32      = lift .:   evalI32
  evalI64      = lift .:   evalI64
  evalBool     = lift .:   evalBool
  areUnequal   = lift .::. areUnequal
  isUnsat      = lift .    isUnsat
  
instance MonadSolver m => MonadSolver (SymbolicsStateT s m) where
  isSubset     = lift .:   isSubset
  pickElement  = lift .    pickElement
  isEmpty      = lift .    isEmpty
  isNonZeroI32 = lift .:   isNonZeroI32
  evalI32      = lift .:   evalI32
  evalI64      = lift .:   evalI64
  evalBool     = lift .:   evalBool
  areUnequal   = lift .::. areUnequal
  isUnsat      = lift .    isUnsat
  
instance MonadSolver m => MonadSolver (ExtSymbolicsStateT s m) where
  isSubset     = lift .:   isSubset
  pickElement  = lift .    pickElement
  isEmpty      = lift .    isEmpty
  isNonZeroI32 = lift .:   isNonZeroI32
  evalI32      = lift .:   evalI32
  evalI64      = lift .:   evalI64
  evalBool     = lift .:   evalBool
  areUnequal   = lift .::. areUnequal
  isUnsat      = lift .    isUnsat

instance (Monoid w, MonadSolver m) => MonadSolver (RWST r w s m) where
  isSubset     = lift .:   isSubset
  pickElement  = lift .    pickElement
  isEmpty      = lift .    isEmpty
  isNonZeroI32 = lift .:   isNonZeroI32
  evalI32      = lift .:   evalI32
  evalI64      = lift .:   evalI64
  evalBool     = lift .:   evalBool
  areUnequal   = lift .::. areUnequal
  isUnsat      = lift .    isUnsat

instance MonadSolver m => MonadSolver (ReaderT r m) where
  isSubset     = lift .:   isSubset
  pickElement  = lift .    pickElement
  isEmpty      = lift .    isEmpty
  isNonZeroI32 = lift .:   isNonZeroI32
  evalI32      = lift .:   evalI32
  evalI64      = lift .:   evalI64
  evalBool     = lift .:   evalBool
  areUnequal   = lift .::. areUnequal
  isUnsat      = lift .    isUnsat
