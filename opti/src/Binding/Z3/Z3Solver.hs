{-# LANGUAGE GeneralizedNewtypeDeriving, UnicodeSyntax, GADTs, TupleSections,
             FlexibleContexts, RankNTypes, ScopedTypeVariables, TemplateHaskell
#-}

-- | Provides an instance for `Lang.Wasm.Solver` for the Z3 theorem prover.
--
-- Note that Floating-point operations are not fully defined, and are instead
-- defined over the empty theory (functional congruence) over uninterpreted
-- sorts. This limits the ability to generate appropriate counter-examples for
-- instances containing floating-point numbers.
module Binding.Z3.Z3Solver
  ( Z3Solver
  , runZ3SolverUntil
  , runZ3SolverUntilAndCount
  , runZ3SolverTimeout
  , runZ3SolverTimeoutAndCount
  ) where

import Melude
-- Stdlib imports
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty )
import           Control.Exception ( catch )
import           Control.Monad.Writer ( MonadWriter, tell )
-- Extra stdlib imports
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
import           Data.IntMap ( IntMap )
import qualified Data.Map as Map
import           Data.Map ( Map )
import qualified Data.IntMap.Strict as IntMap
import           Data.IntMap.Strict ( IntMap )
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict ( HashMap )
import           Data.Word ( Word8 )
-- External library imports
import qualified Z3.Monad as Z3
import           Z3.Monad ( MonadZ3 )
import qualified Data.IdList as IdList
-- Local library imports
import qualified Lang.Wasm.Data as WD
import           Lang.Wasm.Ast
  ( TI32, TI64, TF32, TF64, WI32 (..), WI64 (..), WF32 (..), WF64 (..)
  , FUnop, FBinop, FRelop, Sx (..), Sat (..), ValType (..), Val, PVal (..)
  )
-- Local imports
import qualified Binding.Z3.WasmZ3 as W3
import           Binding.Z3.Helpers ( mkNe, evalArr )
import           Lang.Wasm.Markers ( TI33, TMem )
import           Lang.Wasm.Solver ( MonadSolver (..), EvalResult (..) )
-- import qualified Lang.Wasm.Symbolic.State as SymState
import qualified Lang.Wasm.Dataflow.Liveness as L
import           Lang.Wasm.Dataflow.Liveness ( LivenessState )
import qualified Lang.Wasm.Concrete.ProgState as Con
import qualified Lang.Wasm.Symbolics as Y
import           Lang.Wasm.Symbolics
  ( TFunc, TWorld, TMemSize, Symbolic (..), SymbolicVal (..), ExtSymbolics )
import           Lang.Wasm.Symbolic.General as SG
import           Lang.Wasm.Symbolic.ProgramState
import           Lang.Wasm.Symbolic.Configuration as C
import           Lang.Wasm.Symbolic.Configuration
  ( Configuration (..), cfgSymbolics, cfgConstraint, cfgStack, cfgGlobals
  , cfgMem, cfgActivation
  )


import Data.Maybe ( maybeToList )

-- # Internal Data Structures #
-- (Must be defined on top because of Template Haskell splices)

-- | Functions are defined by `Maybe`s, this avoids them from being created in
-- Z3 when they are not used. They are created upon first use.
data SharedSolverState =
  SharedSolverState {
    _sSortF32    :: Maybe Z3.Sort -- uninterpreted
  , _sSortF64    :: Maybe Z3.Sort -- uninterpreted
  , _sSortWorld  :: Maybe Z3.Sort -- uninterpreted

  , _sF32FromBv  :: Maybe Z3.FuncDecl -- f32bv -> f32
  , _sF32ToBv    :: Maybe Z3.FuncDecl -- f32 -> f32bv
  , _sF32Unops   :: HashMap (FUnop TF32) Z3.FuncDecl  -- f32 -> f32
  , _sF32Binops  :: HashMap (FBinop TF32) Z3.FuncDecl -- f32 -> f32 -> f32
  , _sF32Relops  :: HashMap (FRelop TF32) Z3.FuncDecl -- f32 -> f32 -> Bool
  
  , _sF64FromBv  :: Maybe Z3.FuncDecl -- f64bv -> f64
  , _sF64ToBv    :: Maybe Z3.FuncDecl -- f64 -> f64bv
  , _sF64Unops   :: HashMap (FUnop TF64) Z3.FuncDecl  -- f64 -> f64
  , _sF64Binops  :: HashMap (FBinop TF64) Z3.FuncDecl -- f64 -> f64 -> f64
  , _sF64Relops  :: HashMap (FRelop TF64) Z3.FuncDecl -- f64 -> f64 -> Bool
  
  , _sF64PromoteF32  :: Maybe Z3.FuncDecl -- f32 -> f64
  , _sF32DemoteF64   :: Maybe Z3.FuncDecl -- f64 -> f32
  , _sF32ConvertI32  :: Maybe Z3.FuncDecl -- i32 -> f32
  , _sF64ConvertI32  :: Maybe Z3.FuncDecl -- i32 -> f64
  , _sF32ConvertI64  :: Maybe Z3.FuncDecl -- i64 -> f32
  , _sF64ConvertI64  :: Maybe Z3.FuncDecl -- i64 -> f64
  
  , _sI32TruncF32     :: Maybe Z3.FuncDecl -- f32 -> i32
  , _sI32TruncF64     :: Maybe Z3.FuncDecl -- f64 -> i32
  , _sI64TruncF32     :: Maybe Z3.FuncDecl -- f32 -> i64
  , _sI64TruncF64     :: Maybe Z3.FuncDecl -- f64 -> i64
  , _sI32TruncSatF32  :: Maybe Z3.FuncDecl -- f32 -> i32
  , _sI32TruncSatF64  :: Maybe Z3.FuncDecl -- f64 -> i32
  , _sI64TruncSatF32  :: Maybe Z3.FuncDecl -- f32 -> i64
  , _sI64TruncSatF64  :: Maybe Z3.FuncDecl -- f64 -> i64

  , _sMemGrowWorld  :: Maybe Z3.FuncDecl -- world -> i32 -> world
  , _sMemGrowBool   :: Maybe Z3.FuncDecl -- world -> i32 -> bool

  , _sNumSymbols  :: Int -- ^ Avoids symbols being reused
  }

newtype SingleSolverState =
    SingleSolverState {
    -- | Every WebAssembly function is associated with multiple uninterpreted
    -- functions within the logic. As every function in the logic can only have
    -- a single output, every output is associated with its own sub-function.
    -- E.g., `A -> (B x C)` is decomposed into `A -> B` and `A -> C`.
    -- These functions have inputs:
    -- World, Mem, MemSize, ...Params
    -- And these results:
    -- So, every function has at least 3 outputs
    -- World, Mem, MemSize, ...Results
    _sAstCache  :: IntMap (Either Z3.AST [Z3.FuncDecl])
  }

$(makeLenses ''SharedSolverState)
$(makeLenses ''SingleSolverState)


-- # Z3 Solver

type NumInvocations = Int

-- | The datastructure for the `Solver` implementation for Z3.
--
-- Contains the /end-time/, beyond which Z3 may /not/ run. As every computation
-- contained in a Z3 monad may contain multiple calls to the Z3 library, their
-- combined time should be constrained. Hence, individual calls are constrained
-- by a timeout of @endTime - currTime@.
newtype Z3Solver a = Z3Solver (RWST (Maybe TimeMs) Sum () IO a)
  deriving ( Functor, Applicative, Monad )

-- | Runs the Z3 solver, with the given /end time/. Beyond this time, no Z3
-- calls will be made.
runZ3SolverUntil :: Z3Solver a -> Maybe TimeMs -> IO a
runZ3SolverUntil (Z3Solver a) t =
  do
    (a, _, _) <- runRWST a t ()
    return a

-- | Runs the Z3 solver, with the given /end time/. Beyond this time, no Z3
-- calls will be made.
--
-- Also returns the number of times Z3 was invoked within this monad.
--
-- TODO: Revise these diagnostics
runZ3SolverUntilAndCount :: Z3Solver a -> Maybe TimeMs -> IO (a, NumInvocations)
runZ3SolverUntilAndCount (Z3Solver a) t =
  do
    (a, _, Sum c) <- runRWST a t ()
    return (a, c)

runZ3SolverTimeout :: Z3Solver a -> TimeMs -> IO a
runZ3SolverTimeout s t =
  do
    currTime <- getTimeMs
    let endTime = currTime + t
    runZ3SolverUntil s (Just endTime)
    
runZ3SolverTimeoutAndCount :: Z3Solver a -> TimeMs -> IO (a, NumInvocations)
runZ3SolverTimeoutAndCount s t =
  do
    currTime <- getTimeMs
    let endTime = currTime + t
    runZ3SolverUntilAndCount s (Just endTime)

newtype Sum = Sum Int

instance Semigroup Sum where
  (<>) (Sum a) (Sum b) = Sum ( a + b )
  
instance Monoid Sum where
  mempty = Sum 0

instance MonadSolver Z3Solver where
  -- isSubset :: Configuration a -> Configuration b -> m Bool
  isSubset a b =
    Z3Solver $ evalZ3DSolver
                 (isSubsetM a b)
                 (Y.liftExt (a^.cfgSymbolics))
                 (Y.liftExt (b^.cfgSymbolics))
                 False -- Errs on the side of caution

  -- pickElement :: Configuration a -> m (Maybe ConP.ProgState)
  -- This fails when the configuration contains floating-point values
  pickElement a =
    Z3Solver $ evalZ3Solver (pickElement' a) (Y.liftExt (a^.cfgSymbolics)) Nothing
    where
    pickElement' :: MonadZ3 z3 => Configuration env -> SolverRWS z3 env (Maybe Con.ProgState)
    pickElement' a =
      do
        Z3.assert =<< u (z3Bool (a^.cfgConstraint))
        enforceTimeout
        res <- Z3.check
        case res of
          Z3.Undef -> return Nothing
          Z3.Unsat -> return Nothing
          Z3.Sat   ->
            do
              enforceTimeout
              markZ3Invoked -- diagnostics
              -- Find a counter-example. If it cannot be represented (e.g. floats), that's fine.
              res' <- catchMaybeT $ Z3.withModel $ \m -> concreteStateC m a
              case res' of
                Just (_, Just resElem) -> return $ Just resElem
                _                      -> return Nothing

  -- isEmpty :: Configuration a -> m Bool
  isEmpty a =
    Z3Solver $ evalZ3Solver
                 (isEmpty' a)
                 (Y.liftExt (a^.cfgSymbolics))
                 False -- Errs on the side of caution
    where
    isEmpty' :: MonadZ3 z3 => Configuration env -> SolverRWS z3 env Bool
    isEmpty' a =
      case a^.cfgConstraint of
        -- `v` is the path constraint. If it's unsatisfiable (`False`), the
        -- configuration is empty.
        (Y.SymConstBool v) -> return (not v)
        _ ->
          do
            Z3.assert =<< u (z3Bool (a^.cfgConstraint))
            enforceTimeout
            markZ3Invoked -- diagnostics
            Z3.check <&> (== Z3.Unsat)

  -- isNonZeroI32  :: Environment env -> Symbolic env TI32 -> m Bool
  isNonZeroI32 _ (SymConstI32 (WI32 v)) = return (v /= 0)
  isNonZeroI32 (sym, constraint) v =
    -- The value is non-zero when the environment where the value is 0, is unsatisfiable
    let (constraint', sym') = Y.runSymbolicsState (andConstraint constraint v) sym
    in isUnsat (Y.liftExt sym', constraint')
    where
    andConstraint :: Y.MonadSymbolics env1 m
                  => Symbolic env1 Bool
                  -> Symbolic env1 TI32
                  -> m (Symbolic env1 Bool)
    andConstraint constraint v = Y.vBAnd constraint =<< Y.vI32Eqz v

  -- evalI32  :: Symbolic env TI32 -> Environment env -> m (EvalResult WI32)
  evalI32 _ (SymConstI32 v) = return $ EvSat v
  evalI32 (sym, constraint) v =
    Z3Solver $ evalZ3Solver
                 (evalI32' constraint v)
                 (Y.liftExt sym)
                 EvUnknown -- Errs on the side of caution
    where
    evalI32' :: MonadZ3 z3
             => Symbolic env Bool
             -> Symbolic env TI32
             -> SolverRWS z3 env (EvalResult WI32)
    evalI32' constraint v =
      do
        Z3.assert =<< u (z3Bool constraint)
        var <- W3.unAST <$> z3I32 v
        enforceTimeout
        markZ3Invoked -- diagnostics
        res <- Z3.withModel $ \m -> (fmap (fromMaybe 0) . Z3.evalInt m) var
        case res of
          (Z3.Sat, Just val) ->
            do
              -- Only if there is /no other/ satisfying assignment, the constant
              -- is a valid replacement.
              val' <- Z3.mkBitvector 32 val
              Z3.assert =<< Z3.mkNot =<< Z3.mkEq var val' -- Assert: var /= val
              enforceTimeout
              res <- Z3.check
              case res of
                Z3.Unsat -> return $ EvSat $ WI32 $ toNum val
                Z3.Undef -> return EvUnknown
                Z3.Sat   -> return EvUnsat -- The /unique/ assignment is unsatisfiable
          (Z3.Unsat, _) -> return EvUnsat
          _ -> return EvUnknown

  -- evalI64  :: Symbolic env TI64 -> Environment env -> m (EvalResult WI64)
  evalI64 _ (SymConstI64 v) = return $ EvSat v
  evalI64 (sym, constraint) v =
    Z3Solver $ evalZ3Solver
                 (evalI64' v constraint)
                 (Y.liftExt sym)
                 EvUnknown -- Errs on the side of caution
    where
    evalI64' :: MonadZ3 z3
             => Symbolic env TI64
             -> Symbolic env Bool
             -> SolverRWS z3 env (EvalResult WI64)
    evalI64' v constraint =
      do
        Z3.assert =<< u (z3Bool constraint)
        var <- W3.unAST <$> z3I64 v
        enforceTimeout
        markZ3Invoked -- diagnostics
        res <- Z3.withModel $ \m -> (fmap (fromMaybe 0) . Z3.evalInt m) var
        case res of
          (Z3.Sat, Just val) ->
            do
              -- Only if there is /no other/ satisfying assignment, the constant
              -- is a valid replacement.
              val' <- Z3.mkBitvector 64 val
              Z3.assert =<< Z3.mkNot =<< Z3.mkEq var val' -- Assert: var /= val
              enforceTimeout
              res <- Z3.check
              case res of
                Z3.Unsat -> return $ EvSat $ WI64 $ toNum val
                Z3.Undef -> return EvUnknown
                Z3.Sat   -> return EvUnsat -- The /unique/ assignment is unsatisfiable
          (Z3.Unsat, _) -> return EvUnsat
          _ -> return EvUnknown
  
  -- evalI32  :: Symbolic env TI32 -> Environment env -> m (EvalResult WI32)
  evalBool _ (SymConstI32 (WI32 v)) = return $ EvSat (v /= 0)
  evalBool (sym, constraint) v =
    Z3Solver $ evalZ3Solver
                 (evalBool' constraint v)
                 (Y.liftExt sym)
                 EvUnknown -- Errs on the side of caution
    where
    evalBool' :: MonadZ3 z3
              => Symbolic env Bool
              -> Symbolic env TI32
              -> SolverRWS z3 env (EvalResult Bool)
    evalBool' constraint v =
      do
        Z3.assert =<< u (z3Bool constraint)
        var <- W3.unAST <$> (W3.mkI32ToBool =<< z3I32 v)
        enforceTimeout
        markZ3Invoked -- diagnostics
        res <- Z3.withModel $ \m -> (fmap (fromMaybe False) . Z3.evalBool m) var
        case res of
          (Z3.Sat, Just val) ->
            do
              -- Only if there is /no other/ satisfying assignment, the constant
              -- is a valid replacement.
              val' <- Z3.mkBool val
              Z3.assert =<< Z3.mkNot =<< Z3.mkEq var val' -- Assert: var /= val
              enforceTimeout
              res <- Z3.check
              case res of
                Z3.Unsat -> return $ EvSat val
                Z3.Undef -> return EvUnknown
                Z3.Sat   -> return EvUnsat -- The /unique/ assignment is unsatisfiable
          (Z3.Unsat, _) -> return EvUnsat
          _ -> return EvUnknown

  -- areUnequal :: ExtSymbolics env
  --            -> SymbolicState env
  --            -> SymbolicState env
  --            -> SymbolicState env
  --            -> m (EvalResult (Maybe ConcreteState))
  areUnequal (sym, constraint) input out1 out2 outLiveness =
    Z3Solver $ evalZ3Solver
                 (areUnequalM constraint input out1 out2 outLiveness)
                 sym
                 EvUnknown -- Errs on the side of caution

  -- isUnsat :: ExtSymbolics env
  --         -> SymbolicState env
  --         -> m Bool
  isUnsat (_, SymConstBool b) = return (not b)
  isUnsat (sym,c) =
    Z3Solver $ evalZ3Solver
                 (isUnsatM c)
                 sym
                 False -- Errs on the side of caution
    where
    isUnsatM :: MonadZ3 z3
             => Symbolic env Bool
             -> SolverRWS z3 env Bool
    isUnsatM constraint =
      do
        Z3.assert =<< u (z3Bool constraint)
        enforceTimeout
        markZ3Invoked -- diagnostics
        (== Z3.Unsat) <$> Z3.check

markZ3Invoked :: MonadWriter Sum m => m ()
markZ3Invoked = tell (Sum 1)


instance MonadIO Z3Solver where
  liftIO = Z3Solver . liftIO

-- | A Z3 solver over a symbolic environment. It tracks internal state which
-- caches generated ASTs. This environment also ensures uninterpreted functions
-- for unused (float) operators are not passed to Z3.
type SolverRWS z3 env a =
  RWST (Maybe TimeMs, ExtSymbolics env)
       Sum
       (SharedSolverState, SingleSolverState)
       (MaybeT z3)
       a

-- | A Z3 solver over /two/ symbolic environments. It tracks internal state
-- which caches generated ASTs. This environment also ensures uninterpreted
-- functions for unused (float) operators are not passed to Z3.
--
-- Note that two environments share the set of uninterpreted functions (e.g.,
-- for float operators).
type DSolverRWS z3 env1 env2 a =
  RWST (Maybe TimeMs, ExtSymbolics env1, ExtSymbolics env2)
       Sum
       (SharedSolverState, SingleSolverState, SingleSolverState)
       (MaybeT z3)
       a

evalZ3Safe :: a -> Z3.Z3 a -> IO a
evalZ3Safe def a =
  -- QF_AUFBV supports quantifier-free formulas with bitvectors, bitvector arrays, free sorts, and function symbols
  Z3.evalZ3With (Just Z3.QF_AUFBV) Z3.stdOpts a `catch` \ (_ :: Z3.Z3Error) -> return def

-- | Runs a solver over one symbolic environment. If this fails (e.g., on
-- malformed formulas or timeout), the default value is returned.
evalZ3Solver :: SolverRWS Z3.Z3 env a
             -> ExtSymbolics env
             -> a
             -> RWST (Maybe TimeMs) Sum () IO a
evalZ3Solver solver sym defaultVal =
  do
    endTime <- R.ask

    mRes <- lift
              $ evalZ3Safe Nothing
              $ runMaybeT
              $ evalRWST
                  solver
                  (endTime, sym)
                  (emptySharedSolverState, emptySingleSolverState)
    case mRes of
      Nothing  -> return defaultVal
      Just (res, numInvoked) -> tell numInvoked >> return res

-- | Runs a solver over two symbolic environment. If this fails (e.g., on
-- malformed formulas or timeout), the default value is returned.
evalZ3DSolver :: DSolverRWS Z3.Z3 env1 env2 a
              -> ExtSymbolics env1
              -> ExtSymbolics env2
              -> a
              -> RWST (Maybe TimeMs) Sum () IO a
evalZ3DSolver solver sym1 sym2 defaultVal =
  do
    endTime <- R.ask

    mRes <- lift
              $ evalZ3Safe Nothing
              $ runMaybeT
              $ evalRWST
                  solver
                  (endTime, sym1, sym2)
                  (emptySharedSolverState, emptySingleSolverState, emptySingleSolverState)

    case mRes of
      Nothing  -> return defaultVal
      Just (res, numInvoked) -> tell numInvoked >> return res

-- | Tries to return a counter-example, if possible; `Nothing` signifies
-- equality. For programs where a counter-example is impossible, a `Bool` is
-- returned instead.
areUnequalM :: (MonadIO z3, MonadZ3 z3)
            => Symbolic env Bool -- ^ Constraint
            -> SymbolicProgramState env     -- ^ Initial state
            -> SymbolicProgramState env     -- ^ Output state 1
            -> SymbolicProgramState env     -- ^ Output state 2
            -> LivenessState     -- ^ Output state 2
            -> SolverRWS z3 env (EvalResult (Maybe Con.ProgState))
areUnequalM constraint input out1 out2 liveness =
  do
    failRWSIf $ IdList.size (out1^.sGlobals) /= IdList.size (out2^.sGlobals)
    failRWSIf $ IdList.size (out1^.sGlobals) /= IdList.size (liveness ^. L.lsGlobals)

    Z3.assert =<< u (z3Bool constraint)

    isStackNe   <- mkNeLocalState liveness (out1^.sLocalState) (out2^.sLocalState)
    isGlobalsNe <- W3.mkOr =<< zipWith3M mkNeGlobal (IdList.toList (liveness ^. L.lsGlobals)) (IdList.toList $ out1^.sGlobals) (IdList.toList $ out2^.sGlobals)
    isMemNe     <- mkNeMaybe mkNeMem (out1^.sMem) (out2^.sMem)
    isWorldNe   <- join (W3.mkNe <$> z3World (out1^.sWorld) <*> z3World (out2^.sWorld))

    Z3.assert =<< u (W3.mkOr [ isStackNe, isGlobalsNe, isMemNe, isWorldNe ])

    enforceTimeout
    markZ3Invoked -- diagnostics
    res <- Z3.check

    case res of
      Z3.Undef -> return EvUnknown
      Z3.Unsat -> return EvUnsat
      Z3.Sat   ->
        do
          enforceTimeout
          -- Find a counter-example. If it cannot be represented (e.g. floats), that's fine.
          res' <- catchMaybeT $ Z3.withModel $ \m -> concreteState m input
          case res' of
            Just (_, Just resIn) -> return $ EvSat (Just resIn)
            _                    -> return $ EvSat Nothing
  where
  mkNeMem :: MonadZ3 z3
          => SymbolicMem env
          -> SymbolicMem env
          -> SolverRWS z3 env (W3.AST Bool)
  mkNeMem a b =
    do
      failIf ((a ^. SG.smLimits) /= (b ^. SG.smLimits)) "Mismatching static structures"
      isPageCountNe <- join (W3.mkNe <$> z3I32 (a^.smPageCount) <*> z3I32 (b^.smPageCount))
      isDataNe <- join (W3.mkNe <$> z3Mem (a^.smData) <*> z3Mem (b^.smData))
      W3.mkOr [isPageCountNe, isDataNe]

  mkNeGlobal :: MonadZ3 z3
             => L.LiveGlobal
             -> SymbolicGlobal env
             -> SymbolicGlobal env
             -> SolverRWS z3 env (W3.AST Bool)
  mkNeGlobal l a b =
    do
      failIf (WD.gMut a /= WD.gMut b) "Mismatching static structures"
      failIf (WD.gMut l /= WD.gMut a) "Mismatching static structures"
      mkNeVal (WD.gVal l) (WD.gVal a) (WD.gVal b)
  
  mkNeMaybe :: MonadZ3 z3
            => ( a -> b -> SolverRWS z3 env (W3.AST Bool) )
            -> Maybe a
            -> Maybe b
            -> SolverRWS z3 env (W3.AST Bool)
  mkNeMaybe _ Nothing  Nothing  = W3.mkBool False -- they are surely equal
  mkNeMaybe _ (Just _) Nothing  = failRWS -- Unequal static structures
  mkNeMaybe _ Nothing  (Just _) = failRWS -- Unequal static structures
  mkNeMaybe f (Just a) (Just b) = f a b

  -- | Returns an AST representing the /inequality/ between the two local states
  mkNeLocalState :: MonadZ3 z3
                 => LivenessState
                 -> Maybe (SymbolicLocalState env)
                 -> Maybe (SymbolicLocalState env)
                 -> SolverRWS z3 env (W3.AST Bool)
  mkNeLocalState _ Nothing  Nothing  = W3.mkBool False -- False, they are surely equal
  -- they are unequal if either is not trapped
  mkNeLocalState _ (Just a) Nothing  = W3.mkNot =<< z3Bool (a^.sIsTrapped)
  mkNeLocalState _ Nothing  (Just b) = W3.mkNot =<< z3Bool (b^.sIsTrapped)
  mkNeLocalState ls (Just a) (Just b) =
    do
      -- The local states are /equal/ when:
      -- A.isTrapped = B.istrapped && !A.isTrapped -> A.stack = B.stack
      -- After all, the stacks need not be equal for trapped executions.
      -- /Inequality/ is then its inverse, as:
      -- A.isTrapped /= B.isTrapped || (!A.isTrapped && A.stack != B.stack)
      aIsTrapped <- z3Bool (a^.sIsTrapped)
      bIsTrapped <- z3Bool (b^.sIsTrapped)
      isTrappedNe <- W3.mkNe aIsTrapped bIsTrapped
      notIsTrapped <- W3.mkNot aIsTrapped
      -- Stacks need not be equal when both executions trapped
      isStackNe <- W3.mkAnd2 notIsTrapped =<< mkNeStack (ls ^. L.lsStack) (a^.sStack) (b^.sStack)
      isActivationNe <- mkNeActivation (ls ^. L.lsActivation) (a^.sActivation) (b^.sActivation)
      W3.mkOr [isTrappedNe, isStackNe, isActivationNe]

  -- | Returns an AST representing the /inequality/ between the two stacks
  mkNeStack :: MonadZ3 z3
            => NonEmpty [L.LiveVal]
            -> SymbolicStack env
            -> SymbolicStack env
            -> SolverRWS z3 env (W3.AST Bool)
  mkNeStack ls xs ys =
    do
      failIf (not $ eqWithLenNE eqLength xs ys) "Mismatching static structures"
      W3.mkOr . concat =<< zipWith3M (zipWith3M mkNeVal) (NE.toList ls) (NE.toList xs) (NE.toList ys)

  mkNeActivation :: MonadZ3 z3
                 => L.LiveActivation
                 -> SymbolicActivation env
                 -> SymbolicActivation env
                 -> SolverRWS z3 env (W3.AST Bool)
  mkNeActivation ls a b =
    do
      failIf (IdList.size (WD.aParams a) /= IdList.size (WD.aParams b)) "Mismatching static structures"
      failIf (IdList.size (WD.aLocals a) /= IdList.size (WD.aLocals b)) "Mismatching static structures"

      isParamsNe <- zipWith3M mkNeVal (IdList.toList $ WD.aParams ls) (IdList.toList $ WD.aParams a) (IdList.toList $ WD.aParams b)
      isLocalsNe <- zipWith3M mkNeVal (IdList.toList $ WD.aLocals ls) (IdList.toList $ WD.aLocals a) (IdList.toList $ WD.aLocals b)
      W3.mkOr (isParamsNe ++ isLocalsNe)

  -- | Returns an AST representing the /inequality/ between the two values.
  mkNeVal :: MonadZ3 z3 => L.LiveVal -> Y.ExtSymbolicVal env -> Y.ExtSymbolicVal env -> SolverRWS z3 env (W3.AST Bool)
  mkNeVal (VI32 L.Dead) (VI32 a) (VI32 b) = W3.mkBool False -- Assume they're equal (as dead values don't matter)
  mkNeVal (VI64 L.Dead) (VI64 a) (VI64 b) = W3.mkBool False
  mkNeVal (VF32 L.Dead) (VF32 a) (VF32 b) = W3.mkBool False
  mkNeVal (VF64 L.Dead) (VF64 a) (VF64 b) = W3.mkBool False
  mkNeVal (VI32 L.Live) (VI32 a) (VI32 b)
    | Y.isEqSymbolic a b  = W3.mkBool False
    | otherwise           = join (W3.mkNe <$> z3I32 a <*> z3I32 b)
  mkNeVal (VI64 L.Live) (VI64 a) (VI64 b)
    | Y.isEqSymbolic a b  = W3.mkBool False
    | otherwise           = join (W3.mkNe <$> z3I64 a <*> z3I64 b)
  mkNeVal (VF32 L.Live) (VF32 a) (VF32 b)
    | Y.isEqSymbolic a b  = W3.mkBool False
    | otherwise           = join (W3.mkNe <$> z3F32 a <*> z3F32 b)
  mkNeVal (VF64 L.Live) (VF64 a) (VF64 b)
    | Y.isEqSymbolic a b  = W3.mkBool False
    | otherwise           = join (W3.mkNe <$> z3F64 a <*> z3F64 b)
  mkNeVal _ _ _ = failRWS

-- | Evaluates a symbolic WebAssembly state under a Z3 model.
--
-- Note that this may fail, as floating-point numbers cannot really be represented.
concreteState :: MonadZ3 z3
              => Z3.Model
              -> SymbolicProgramState env
              -> SolverRWS z3 env Con.ProgState
concreteState m s =
  Con.ProgState
    <$> concreteLocalState m (s^.sLocalState)
    <*> mapM (concreteGlobal m) (s^.sGlobals)
    <*> concreteMem m (s^.sMem)
  where
  concreteLocalState :: MonadZ3 z3
                     => Z3.Model
                     -> Maybe (SymbolicLocalState env)
                     -> SolverRWS z3 env (Maybe Con.LocalState)
  concreteLocalState _ Nothing  = return Nothing
  concreteLocalState m (Just s) =
    do
      isTrapped <- fromMaybe True <$> (Z3.evalBool m =<< u (z3Bool (s^.sIsTrapped)))
      if isTrapped then
        return Nothing
      else
        (Just .: Con.LocalState)
          <$> concreteActivation m (s^.sActivation)
          <*> WD.mapStackVal (concreteValExt m) (s^.sStack)

-- | Returns a concrete program state that is a member of the configuration
-- under the given Z3 model.
concreteStateC :: MonadZ3 z3
               => Z3.Model
               -> Configuration env
               -> SolverRWS z3 env Con.ProgState
concreteStateC m c =
  Con.ProgState
    <$> concreteLocalStateC m (c^.cfgActivation) (c^.cfgStack)
    <*> mapM (WD.mapGlobalValM $ concreteVal m) (c^.cfgGlobals)
    <*> concreteMem m (c^.cfgMem)
  where
  concreteLocalStateC :: MonadZ3 z3
                      => Z3.Model
                      -> C.Activation env
                      -> C.Stack env
                      -> SolverRWS z3 env (Maybe Con.LocalState)
  concreteLocalStateC m act s =
    (Just .: Con.LocalState)
      <$> WD.mapActivationVal (concreteVal m) act
      <*> mapM (mapM (concreteVal m)) s
  concreteMemC :: MonadZ3 z3
               => Z3.Model
               -> Maybe (SymbolicMem env)
               -> SolverRWS z3 env (Maybe Con.Mem)
  concreteMemC _ Nothing = return Nothing
  concreteMemC m (Just mem) =
    do
      (memDefault, memData) <- evalMem m (mem ^. smData)
      memPageCount <- concreteI32 m (mem ^. SG.smPageCount)
      return $ Just $ Con.Mem memDefault memData memPageCount (mem ^. SG.smLimits)

concreteMem :: MonadZ3 z3
            => Z3.Model
            -> Maybe (SymbolicMem env)
            -> SolverRWS z3 env (Maybe Con.Mem)
concreteMem _ Nothing = return Nothing
concreteMem m (Just mem) =
  do
    (memDefault, memData) <- evalMem m (mem^.smData)
    memPageCount <- concreteI32 m (mem^.smPageCount)
    return $ Just $ Con.Mem memDefault memData memPageCount (mem^.smLimits)

concreteActivation :: MonadZ3 z3
                   => Z3.Model
                   -> SymbolicActivation env
                   -> SolverRWS z3 env Con.Activation
concreteActivation m a =
  WD.PActivation
    <$> mapM (concreteValExt m) (WD.aParams a)
    <*> mapM (concreteValExt m) (WD.aLocals a)

concreteGlobal :: MonadZ3 z3
               => Z3.Model
               -> SymbolicGlobal env
               -> SolverRWS z3 env Con.Global
concreteGlobal m g =
  WD.PGlobal (WD.gMut g) <$> concreteValExt m (WD.gVal g)

concreteVal :: MonadZ3 z3
            => Z3.Model
            -> Y.SymbolicVal env
            -> SolverRWS z3 env Val
concreteVal m (VI32 i)  = VI32 <$> concreteI32 m i
concreteVal m (VI64 i)  = VI64 <$> concreteI64 m i
concreteVal _ (VF32 ()) = return $ VF32 $ WF32 0
concreteVal _ (VF64 ()) = return $ VF64 $ WF64 0

concreteValExt :: MonadZ3 z3
               => Z3.Model
               -> Y.ExtSymbolicVal env
               -> SolverRWS z3 env Val
concreteValExt m (VI32 i) = VI32 <$> concreteI32 m i
concreteValExt m (VI64 i) = VI64 <$> concreteI64 m i
concreteValExt _ (VF32 _) = failRWS
concreteValExt _ (VF64 _) = failRWS

concreteI32 :: MonadZ3 z3 => Z3.Model -> Symbolic env TI32 -> SolverRWS z3 env TI32
concreteI32 m i = WI32 . maybe 0 toNum <$> (Z3.evalInt m =<< u (z3I32 i))

concreteI64 :: MonadZ3 z3 => Z3.Model -> Symbolic env TI64 -> SolverRWS z3 env TI64
concreteI64 m i = WI64 . maybe 0 toNum <$> (Z3.evalInt m =<< u (z3I64 i))

-- | Evaluates the memory data block
evalMem :: MonadZ3 z3
        => Z3.Model
        -> Symbolic env TMem
        -> SolverRWS z3 env (Word8, IntMap Word8)
evalMem m mem =
  do
    mem' <- z3Mem mem
    Just (def, arr) <- evalArr (fmap (fmap $ fmap toNum) . Z3.evalInt) Z3.evalInt m (W3.unAST mem')
    return (toNum def, toNum <$> toIntMap arr)


-- | Checks whether the states represented by the first `Configuration` is a
-- (non-strict) subset of the second.
--
-- Consider two sets: A = { a | a < 5 }, B = { b | b < 10 }.
-- A is a subset of B. This is because there is /no/ valid assignment for:
-- a < 5 AND b >= 10 AND a = b
--
-- Consider two sets: A = { a | a < 5 }, B = { b | b < 3 }.
-- A is a /not/ subset of B, because there /is/ a valid assignment for:
-- a < 5 AND b >= 3 AND a = b; namely: a=b=3
isSubsetM :: MonadZ3 z3 => Configuration a -> Configuration b -> DSolverRWS z3 a b Bool
isSubsetM a b =
  do
    failRWSIf $ not $ eqWithLenNE eqLength (a^.cfgStack) (b^.cfgStack)
    failRWSIf $ IdList.size (a^.cfgGlobals) /= IdList.size (b^.cfgGlobals)

    eqMem     <- mkEqMaybe mkEqMem (a^.cfgMem) (b^.cfgMem)
    eqStack   <- mkEqStack (a^.cfgStack) (b^.cfgStack)
    eqGlobals <- Z3.mkAnd =<< zipWithM (fmap u . mkEqGlobal) (IdList.toList (a^.cfgGlobals)) (IdList.toList (b^.cfgGlobals))

    Z3.assert =<< Z3.mkAnd (map W3.unAST [ eqStack, W3.AST eqGlobals, eqMem ])

    Z3.assert =<< u (liftSLeft $ z3Bool (a^.cfgConstraint))
    Z3.assert =<< Z3.mkNot =<< u (liftSRight $ z3Bool (b^.cfgConstraint))

    -- If the instance is surely unsatisfiable, then `a` is a subset of `b`
    enforceDTimeout
    Z3.check <&> (== Z3.Unsat)
  where
  mkEqStack :: MonadZ3 z3
            => C.Stack env1
            -> C.Stack env2
            -> DSolverRWS z3 env1 env2 (W3.AST Bool)
  mkEqStack xs ys =
    W3.AST <$> (Z3.mkAnd =<< zipWithM (fmap u . mkEqVal) (concat $ NE.toList xs) (concat $ NE.toList ys))

  mkEqMem :: MonadZ3 z3
          => SymbolicMem env1
          -> SymbolicMem env2
          -> DSolverRWS z3 env1 env2 (W3.AST Bool)
  mkEqMem a b =
    do
      failRWSIf ((a^.smLimits) /= (b^.smLimits))

      isEqCount <- mkEqAny z3I32 (a^.smPageCount) (b^.smPageCount)
      isEqData <- mkEqAny z3Mem (a^.smData) (b^.smData)

      W3.AST <$> Z3.mkAnd [ W3.unAST isEqCount, W3.unAST isEqData ]

  mkEqGlobal :: MonadZ3 z3
             => C.Global env1
             -> C.Global env2
             -> DSolverRWS z3 env1 env2 (W3.AST Bool)
  mkEqGlobal a b =
    do
      failRWSIf (WD.gMut a /= WD.gMut b)
      mkEqVal (WD.gVal a) (WD.gVal b)

  mkEqVal :: MonadZ3 z3
          => SymbolicVal env1
          -> SymbolicVal env2
          -> DSolverRWS z3 env1 env2 (W3.AST Bool)
  mkEqVal (VI32 a) (VI32 b) = mkEqAny z3I32 a b
  mkEqVal (VI64 a) (VI64 b) = mkEqAny z3I64 a b
  mkEqVal (VF32 a) (VF32 b) = W3.mkBool True
  mkEqVal (VF64 a) (VF64 b) = W3.mkBool True
  mkEqVal _        _        = failRWS

  mkEqAny :: MonadZ3 z3
          => ( forall env . Symbolic env v -> SolverRWS z3 env (W3.AST v) )
          -> Symbolic env1 v
          -> Symbolic env2 v
          -> DSolverRWS z3 env1 env2 (W3.AST Bool)
  mkEqAny f a b =
    join (W3.mkEq <$> liftSLeft (f a) <*> liftSRight (f b))
  
  mkEqMaybe :: MonadZ3 z3
            => ( a -> b -> DSolverRWS z3 env1 env2 (W3.AST Bool) )
            -> Maybe a
            -> Maybe b
            -> DSolverRWS z3 env1 env2 (W3.AST Bool)
  mkEqMaybe _ Nothing  Nothing  = W3.AST <$> Z3.mkBool True
  mkEqMaybe f (Just a) (Just b) = f a b
  mkEqMaybe _ _        _        = failRWS

-- | Fails over the `MaybeT` monad in the `RWST`. The final computation will
-- return `Nothing`.
failRWS :: (Monoid w, Monad m) => RWST r w s (MaybeT m) a
failRWS = lift $ MaybeT (return Nothing)

-- | Fails over the `MaybeT` monad in the `RWST` iff the condition is `True`.
-- The final computation will then return `Nothing`.
failRWSIf :: (Monoid w, Monad m) => Bool -> RWST r w s (MaybeT m) ()
failRWSIf True  = failRWS
failRWSIf False = return ()

-- | Lift a solver of a single environment to a solver over two environments.
liftSLeft :: Monad z3 => SolverRWS z3 env1 a -> DSolverRWS z3 env1 env2 a
liftSLeft rwst = RWST $ \(t, r1, _) (s,s1,s2) ->
  do
    (a, (s',s1'), w) <- runRWST rwst (t, r1) (s,s1)
    return (a, (s',s1',s2), w)

-- | Lift a solver of a single environment to a solver over two environments.
liftSRight :: Monad z3 => SolverRWS z3 env2 a -> DSolverRWS z3 env1 env2 a
liftSRight rwst = RWST $ \(t, _, r2) (s,s1,s2) ->
  do
    (a, (s',s2'), w) <- runRWST rwst (t, r2) (s,s2)
    return (a, (s',s1,s2'), w)

-- | Applies a RWS to a contained RWS state. The containment is described by a
-- `Lens'`.
rwsSLens :: Monad m => Lens' s1 s2 -> RWST r w s2 m a -> RWST r w s1 m a
rwsSLens lens rws = RWST $ \r s1 ->
  do
    let s2 = view lens s1
    (a, s2', w') <- runRWST rws r s2
    let s1' = over lens (const s2') s1
    return (a, s1', w')

-- | Catches a `Nothing` over the `RWST` monad, and returns it as the value.
catchMaybeT :: (Monad m, Monoid w) => RWST r w s (MaybeT m) a -> RWST r w s (MaybeT m) (Maybe a)
catchMaybeT rws = RWST $ \r s ->
  MaybeT $
    do
      res <- runMaybeT $ runRWST rws r s
      case res of
        Just (a,s',w) -> return $ Just (Just a, s', w)
        Nothing       -> return $ Just (Nothing, s, mempty)

emptySharedSolverState :: SharedSolverState
emptySharedSolverState =
  SharedSolverState
    Nothing Nothing Nothing
    Nothing Nothing mempty mempty mempty
    Nothing Nothing mempty mempty mempty
    Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing
    0

emptySingleSolverState :: SingleSolverState
emptySingleSolverState = SingleSolverState mempty

-- If any of the AST nodes is malformed, though a non-existant or type-incorrect
-- sub-expression, the conversion step will return `Nothing`, indicating
-- failure. This reflects into the `Z3Solver` class functions returning their
-- sound (e.g., `False`/`EvUnknown`) result.

z3Val :: MonadZ3 z3
      => Y.ExtSymbolicVal env
      -> SolverRWS z3 env Z3.AST
z3Val (VI32 i) = u $ z3I32 i
z3Val (VI64 i) = u $ z3I64 i
z3Val (VF32 i) = u $ z3F32 i
z3Val (VF64 i) = u $ z3F64 i


z3Bool :: MonadZ3 z3
       => Symbolic env Bool
       -> SolverRWS z3 env (W3.AST Bool)
z3Bool (SymConstBool i) = W3.AST <$> Z3.mkBool i
z3Bool (Symbolic i) = cacheAST z3Bool' (preview $ Y._ECVBase . Y._ACVBool) i
  where
  z3Bool' ::  MonadZ3 z3
          => Y.ComposedVal env Bool
          -> SolverRWS z3 env (W3.AST Bool)
  z3Bool' (Y.VI32Relop op a b)  = join (W3.mkRelopI32 op <$> z3I32 a <*> z3I32 b)
  z3Bool' (Y.VI64Relop op a b)  = join (W3.mkRelopI64 op <$> z3I64 a <*> z3I64 b)
  z3Bool' (Y.VF32Relop op a b)  = callFunc (rwsSLens _1 $ f32Relop op) [ u (z3F32 a), u (z3F32 b) ]
  z3Bool' (Y.VF64Relop op a b)  = callFunc (rwsSLens _1 $ f64Relop op) [ u (z3F64 a), u (z3F64 b) ]
  z3Bool' (Y.VSelect c t f)     = join (W3.mkSelect <$> z3Bool c <*> z3Bool t <*> z3Bool f)
  z3Bool' (Y.VI33Lt a b)        = join (W3.mkLtU33 <$> z3I33 a <*> z3I33 b)
  z3Bool' Y.VBoolUnknown        = W3.AST <$> (Z3.mkBoolVar =<< rwsSLens _1 freshSymbol)
  -- z3Bool' (Y.VBEqMem a b)       = W3.AST <$> join (Z3.mkEq <$> u (z3Mem a) <*> u (z3Mem b))
  -- z3Bool' (Y.VBEqWorld a b)     = W3.AST <$> join (Z3.mkEq <$> u (z3World a) <*> u (z3World b))
  -- z3Bool' (Y.VBEqF32 a b)       = W3.AST <$> join (Z3.mkEq <$> u (z3F32 a) <*> u (z3F32 b))
  -- z3Bool' (Y.VBEqF64 a b)       = W3.AST <$> join (Z3.mkEq <$> u (z3F64 a) <*> u (z3F64 b))
  z3Bool' (Y.VBAnd a b)         = W3.AST <$> (Z3.mkAnd =<< sequenceA [u (z3Bool a), u (z3Bool b)])
  z3Bool' (Y.VBOr a b)          = W3.AST <$> (Z3.mkOr =<< (fmap W3.unAST <$> sequenceA [z3Bool a, z3Bool b]))
  z3Bool' (Y.VBImplies a b)     = W3.AST <$> join (Z3.mkImplies <$> (W3.unAST <$> z3Bool a) <*> (W3.unAST <$> z3Bool b))
  z3Bool' (Y.VBNot a)           = W3.AST <$> (Z3.mkNot =<< u (z3Bool a))
  z3Bool' (Y.VBI32 a)           = W3.mkI32ToBool =<< z3I32 a
  z3Bool' (Y.VMemGrowBool w s)  = callFunc (rwsSLens _1 memGrowBool) [ u (z3World w), u (z3I32 s) ]


z3I32 :: MonadZ3 z3
      => Symbolic env TI32
      -> SolverRWS z3 env (W3.AST TI32)
z3I32 (SymConstI32 (WI32 i)) = W3.mkConstI32 i
z3I32 (Symbolic i) = cacheAST z3I32' (preview $ Y._ECVBase . Y._ACVI32) i
  where
  z3I32' ::  MonadZ3 z3
         => Y.ComposedVal env TI32
         -> SolverRWS z3 env (W3.AST TI32)
  -- z3I32' (Y.VI32Const (WI32 i))   = W3.mkConstI32 i
  z3I32' Y.VI32Unknown                = W3.mkVarI32 =<< rwsSLens _1 freshSymbol
  z3I32' (Y.VI32Unop op a)            = W3.mkUnopI32 op =<< z3I32 a
  z3I32' (Y.VI32Binop op a b)         = join (W3.mkBinopI32 op <$> z3I32 a <*> z3I32 b)
  z3I32' (Y.VSelect c t f)            = join (W3.mkSelect <$> z3Bool c <*> z3I32 t <*> z3I32 f)
  z3I32' (Y.VI32Extend8S a)           = W3.mkExtend8sI32 =<< z3I32 a
  z3I32' (Y.VI32Extend16S a)          = W3.mkExtend16sI32 =<< z3I32 a
  z3I32' (Y.VI32WrapI64 a)            = W3.mkWrapI64toI32 =<< z3I64 a
  z3I32' (Y.VI32TruncF32 NoSat sx a)  = callFunc (rwsSLens _1 i32TruncF32) [Z3.mkBool (sx == U), u (z3F32 a)]
  z3I32' (Y.VI32TruncF64 NoSat sx a)  = callFunc (rwsSLens _1 i32TruncF64) [Z3.mkBool (sx == U), u (z3F64 a)]
  z3I32' (Y.VI32TruncF32 Sat sx a)    = callFunc (rwsSLens _1 i32TruncSatF32) [Z3.mkBool (sx == U), u (z3F32 a)]
  z3I32' (Y.VI32TruncF64 Sat sx a)    = callFunc (rwsSLens _1 i32TruncSatF64) [Z3.mkBool (sx == U), u (z3F64 a)]
  z3I32' (Y.VI32ReinterpretF32 a)     = callFunc (rwsSLens _1 f32ToBv) [u (z3F32 a)]
  z3I32' (Y.VI32Load mem a)           = join (W3.mkLoadI32 <$> z3Mem mem <*> z3I33 a)
  z3I32' (Y.VI32Load8 sx mem a)       = join (W3.mkLoad8sxI32 sx <$> z3Mem mem <*> z3I33 a)
  z3I32' (Y.VI32Load16 sx mem a)      = join (W3.mkLoad16sxI32 sx <$> z3Mem mem <*> z3I33 a)
  z3I32' (Y.VI32Bool a)               = W3.mkBoolToI32 =<< z3Bool a
  z3I32' (Y.VFuncCallMemSize f w mem memSize params) =
    callProgFunc f w mem memSize params 2
  z3I32' (Y.VFuncCallResultI32 f w mem memSize params i) =
    callProgFunc f w mem memSize params (i+3)


z3I33 :: MonadZ3 z3
      => Symbolic env TI33
      -> SolverRWS z3 env (W3.AST TI33)
z3I33 (SymConstI33 i) = W3.mkConstI 33 i
z3I33 (Symbolic i) = cacheAST z3I33' (preview $ Y._ECVBase . Y._ACVI33) i
  where
  z3I33' :: MonadZ3 z3
         => Y.ComposedVal env TI33
         -> SolverRWS z3 env (W3.AST TI33)
  z3I33' Y.VI33Unknown             = W3.mkVarI 33 =<< rwsSLens _1 freshSymbol
  z3I33' (Y.VSelect c t f)        = join (W3.mkSelect <$> z3Bool c <*> z3I33 t <*> z3I33 f)
  -- z3I33' (Y.VI33Const v)           = W3.mkConstI 33 v
  z3I33' (Y.VI33Address addr off)  = join (W3.mkAddress <$> z3I32 addr <*> pure off)
  z3I33' (Y.VI33MulPage a)  =
    do
      a33 <- Z3.mkZeroExt 1 =<< u (z3I32 a)
      pageSize <- Z3.mkBitvector 33 (64*1024)
      W3.AST <$> Z3.mkBvmul a33 pageSize


z3I64 :: MonadZ3 z3
      => Symbolic env TI64
      -> SolverRWS z3 env (W3.AST TI64)
z3I64 (SymConstI64 (WI64 i)) = W3.mkConstI64 i
z3I64 (Symbolic i) = cacheAST z3I64' (preview $ Y._ECVBase . Y._ACVI64) i
  where
  z3I64' ::  MonadZ3 z3
         => Y.ComposedVal env TI64
         -> SolverRWS z3 env (W3.AST TI64)
  -- z3I64' (Y.VI64Const (WI64 i))   = W3.mkConstI64 i
  z3I64' Y.VI64Unknown               = W3.mkVarI64 =<< rwsSLens _1 freshSymbol
  z3I64' (Y.VI64Unop op a)           = W3.mkUnopI64 op =<< z3I64 a
  z3I64' (Y.VI64Binop op a b)        = join (W3.mkBinopI64 op <$> z3I64 a <*> z3I64 b)
  z3I64' (Y.VSelect c t f)           = join (W3.mkSelect <$> z3Bool c <*> z3I64 t <*> z3I64 f)
  z3I64' (Y.VI64Extend8S i)          = W3.mkExtend8sI64 =<< z3I64 i
  z3I64' (Y.VI64Extend16S i)         = W3.mkExtend16sI64 =<< z3I64 i
  z3I64' (Y.VI64Extend32S i)         = W3.mkExtend32sI64 =<< z3I64 i
  z3I64' (Y.VI64ExtendI32 sx i)      = W3.mkExtendI32toI64 sx =<< z3I32 i
  z3I64' (Y.VI64TruncF32 NoSat sx a) = callFunc (rwsSLens _1 i64TruncF32) [Z3.mkBool (sx == U), W3.unAST <$> z3F32 a]
  z3I64' (Y.VI64TruncF64 NoSat sx a) = callFunc (rwsSLens _1 i64TruncF64) [Z3.mkBool (sx == U), W3.unAST <$> z3F64 a]
  z3I64' (Y.VI64TruncF32 Sat sx a)   = callFunc (rwsSLens _1 i64TruncSatF32) [Z3.mkBool (sx == U), W3.unAST <$> z3F32 a]
  z3I64' (Y.VI64TruncF64 Sat sx a)   = callFunc (rwsSLens _1 i64TruncSatF64) [Z3.mkBool (sx == U), W3.unAST <$> z3F64 a]
  z3I64' (Y.VI64ReinterpretF64 a)    = callFunc (rwsSLens _1 f64ToBv) [W3.unAST <$> z3F64 a]
  z3I64' (Y.VI64Load mem a)          = join (W3.mkLoadI64 <$> z3Mem mem <*> z3I33 a)
  z3I64' (Y.VI64Load8 sx mem a)      = join (W3.mkLoad8sxI64 sx <$> z3Mem mem <*> z3I33 a)
  z3I64' (Y.VI64Load16 sx mem a)     = join (W3.mkLoad16sxI64 sx <$> z3Mem mem <*> z3I33 a)
  z3I64' (Y.VI64Load32 sx mem a)     = join (W3.mkLoad32sxI64 sx <$> z3Mem mem <*> z3I33 a)
  z3I64' (Y.VFuncCallResultI64 f w mem memSize params i) =
    callProgFunc f w mem memSize params (i+3)


z3F32 :: MonadZ3 z3
      => Symbolic env TF32
      -> SolverRWS z3 env (W3.AST TF32)
z3F32 (SymConstF32 (WF32 i)) = callFunc (rwsSLens _1 f32FromBv) [ Z3.mkBitvector 32 (toNum i) ]
z3F32 (Symbolic i) = cacheAST z3F32' (preview $ Y._ECVF32) i
  where
  z3F32' ::  MonadZ3 z3
         => Y.ComposedVal env TF32
         -> SolverRWS z3 env (W3.AST TF32)
  -- z3F32' (Y.VF32Const (WF32 i))   = callFunc (rwsSLens _1 f32FromBv) [ Z3.mkBitvector 32 (toNum i) ]
  z3F32' Y.VF32Unknown            = W3.AST <$> join (Z3.mkVar <$> rwsSLens _1 freshSymbol <*> rwsSLens _1 sortF32)
  z3F32' (Y.VF32Unop op a)        = callFunc (rwsSLens _1 $ f32Unop op) [ u (z3F32 a) ]
  z3F32' (Y.VF32Binop op a b)     = callFunc (rwsSLens _1 $ f32Binop op) [ u (z3F32 a), u (z3F32 b) ]
  z3F32' (Y.VSelect c t f)        = join (W3.mkSelect <$> z3Bool c <*> z3F32 t <*> z3F32 f)
  z3F32' (Y.VF32DemoteF64 a)      = callFunc (rwsSLens _1 f32DemoteF64) [ u (z3F64 a) ]
  z3F32' (Y.VF32ConvertI32 sx a)  = callFunc (rwsSLens _1 f32ConvertI32) [ Z3.mkBool (sx == U), u (z3I32 a) ]
  z3F32' (Y.VF32ConvertI64 sx a)  = callFunc (rwsSLens _1 f32ConvertI64) [ Z3.mkBool (sx == U), u (z3I64 a) ]
  z3F32' (Y.VF32ReinterpretI32 a) = callFunc (rwsSLens _1 f32FromBv) [ u (z3I32 a) ]
  z3F32' (Y.VF32Load mem addr)    = callFunc (rwsSLens _1 f32FromBv) [ u $ join (W3.mkLoadI32 <$> z3Mem mem <*> z3I33 addr) ]
  z3F32' (Y.VFuncCallResultF32 f w mem memSize params i) =
    callProgFunc f w mem memSize params (i+3)


z3F64 :: MonadZ3 z3
      => Symbolic env TF64
      -> SolverRWS z3 env (W3.AST TF64)
z3F64 (SymConstF64 (WF64 i)) = callFunc (rwsSLens _1 f64FromBv) [ Z3.mkBitvector 64 (toNum i) ]
z3F64 (Symbolic i) = cacheAST z3F64' (preview $ Y._ECVF64) i
  where
  z3F64' ::  MonadZ3 z3
         => Y.ComposedVal env TF64
         -> SolverRWS z3 env (W3.AST TF64)
  -- z3F64' (Y.VF64Const (WF64 i))   = callFunc (rwsSLens _1 f64FromBv) [ Z3.mkBitvector 64 (toNum i) ]
  z3F64' Y.VF64Unknown            = W3.AST <$> join (Z3.mkVar <$> rwsSLens _1 freshSymbol <*> rwsSLens _1 sortF64)
  z3F64' (Y.VF64Unop op a)        = callFunc (rwsSLens _1 $ f64Unop op) [ u (z3F64 a) ]
  z3F64' (Y.VF64Binop op a b)     = callFunc (rwsSLens _1 $ f64Binop op) [ u (z3F64 a), u (z3F64 b) ]
  z3F64' (Y.VSelect c t f)        = join (W3.mkSelect <$> z3Bool c <*> z3F64 t <*> z3F64 f)
  z3F64' (Y.VF64PromoteF32 a)     = callFunc (rwsSLens _1 f64PromoteF32) [ u (z3F32 a) ]
  z3F64' (Y.VF64ConvertI32 sx a)  = callFunc (rwsSLens _1 f64ConvertI32) [ Z3.mkBool (sx == U), u (z3I32 a) ]
  z3F64' (Y.VF64ConvertI64 sx a)  = callFunc (rwsSLens _1 f64ConvertI64) [ Z3.mkBool (sx == U), u (z3I64 a) ]
  z3F64' (Y.VF64ReinterpretI64 a) = callFunc (rwsSLens _1 f64FromBv) [ u (z3I64 a) ]
  z3F64' (Y.VF64Load mem addr)    = callFunc (rwsSLens _1 f64FromBv) [ u $ join (W3.mkLoadI64 <$> z3Mem mem <*> z3I33 addr) ]
  z3F64' (Y.VFuncCallResultF64 f w mem memSize params i) =
    callProgFunc f w mem memSize params (i+3)


z3Mem :: MonadZ3 z3
      => Symbolic env TMem
      -> SolverRWS z3 env (W3.AST TMem)
z3Mem (Symbolic i) = cacheAST z3Mem' (preview $ Y._ECVBase . Y._ACVMem) i
  where
  z3Mem' ::  MonadZ3 z3
         => Y.ComposedVal env TMem
         -> SolverRWS z3 env (W3.AST TMem)
  z3Mem' Y.VMemUnknown              = W3.mkMemory =<< rwsSLens _1 freshSymbol
  z3Mem' (Y.VSelect c t f)          = join (W3.mkSelect <$> z3Bool c <*> z3Mem t <*> z3Mem f)
  z3Mem' (Y.VI32Store mem addr v)   = join (W3.mkStoreI32 <$> z3Mem mem <*> z3I33 addr <*> z3I32 v)
  z3Mem' (Y.VI64Store mem addr v)   = join (W3.mkStoreI64 <$> z3Mem mem <*> z3I33 addr <*> z3I64 v)
  z3Mem' (Y.VF32Store mem addr v)   = join (W3.mkStoreI32 <$> z3Mem mem <*> z3I33 addr <*> callFunc (rwsSLens _1 f32ToBv) [ u (z3F32 v) ])
  z3Mem' (Y.VF64Store mem addr v)   = join (W3.mkStoreI64 <$> z3Mem mem <*> z3I33 addr <*> callFunc (rwsSLens _1 f64ToBv) [ u (z3F64 v) ])
  z3Mem' (Y.VI32Store8 mem addr v)  = join (W3.mkStore8I32 <$> z3Mem mem <*> z3I33 addr <*> z3I32 v)
  z3Mem' (Y.VI64Store8 mem addr v)  = join (W3.mkStore8I64 <$> z3Mem mem <*> z3I33 addr <*> z3I64 v)
  z3Mem' (Y.VI32Store16 mem addr v) = join (W3.mkStore16I32 <$> z3Mem mem <*> z3I33 addr <*> z3I32 v)
  z3Mem' (Y.VI64Store16 mem addr v) = join (W3.mkStore16I64 <$> z3Mem mem <*> z3I33 addr <*> z3I64 v)
  z3Mem' (Y.VI64Store32 mem addr v) = join (W3.mkStore32I64 <$> z3Mem mem <*> z3I33 addr <*> z3I64 v)
  z3Mem' (Y.VFuncCallMem f w mem memSize params) =
    callProgFunc f w mem memSize params 1


z3Func :: MonadZ3 z3
       => Symbolic env TFunc
       -> SolverRWS z3 env [Z3.FuncDecl]
z3Func (Symbolic i) =
  do
    Right res <-
      cacheOver (_2 . sAstCache . lIntMapIdx i) $
        do
          (_, Y.ExtSymbolics syms) <- R.ask
          Y.ECVFunc x <- liftRWSTMaybe $ IdList.lookup i syms
          Right <$> z3Func' x
    return res
  where
  z3Func' ::  MonadZ3 z3
          => Y.ComposedVal env TFunc
          -> SolverRWS z3 env [Z3.FuncDecl]
  z3Func' (Y.VFuncUnknown params results) =
    do
      sortMem     <- Z3.mkArraySort <$> Z3.mkBvSort 33 <*> Z3.mkBvSort 8
      sortMemSize <- Z3.mkBvSort 32
      wParamSorts <- sequenceA [ rwsSLens _1 sortWorld, sortMem, pure sortMemSize ]
      paramSorts  <- mapM valTypeSort params
      resultSorts <- mapM valTypeSort results

      forM resultSorts $ \r -> join $ Z3.mkFuncDecl <$> rwsSLens _1 freshSymbol <*> pure (wParamSorts ++ paramSorts) <*> pure r
  valTypeSort :: MonadZ3 z3 => ValType -> SolverRWS z3 env Z3.Sort
  valTypeSort TI32 = Z3.mkBvSort 32
  valTypeSort TI64 = Z3.mkBvSort 64
  valTypeSort TF32 = rwsSLens _1 sortF32
  valTypeSort TF64 = rwsSLens _1 sortF64


z3World :: MonadZ3 z3
        => Symbolic env TWorld
        -> SolverRWS z3 env (W3.AST TWorld)
z3World (Symbolic i) = cacheAST z3World' (preview Y._ECVWorld) i
  where
  z3World' ::  MonadZ3 z3
           => Y.ComposedVal env TWorld
           -> SolverRWS z3 env (W3.AST TWorld)
  z3World' Y.VWorldUnknown              = W3.AST <$> join (Z3.mkVar <$> rwsSLens _1 freshSymbol <*> rwsSLens _1 sortWorld)
  z3World' (Y.VMemGrowWorld w numPages) = callFunc (rwsSLens _1 memGrowWorld) [ u (z3World w), u (z3I32 numPages) ]
  z3World' (Y.VFuncCallWorld f w mem memSize params) =
    callProgFunc f w mem memSize params 0


-- | Internal.
--
-- Warning: AST has not type-marker
callProgFunc :: MonadZ3 z3
             => Symbolic env TFunc
             -> Symbolic env TWorld
             -> Symbolic env TMem
             -> Symbolic env TMemSize
             -> [Y.ExtSymbolicVal env]
             -> Natural -- ^ The index of the return value
             -> SolverRWS z3 env (W3.AST a)
callProgFunc f w mem memSize params i =
  do
    Just f' <- atIndex i <$> z3Func f
    xs <- sequenceA
            [ u (z3World w)
            , u (z3Mem mem)
            , u (z3I32 memSize)
            ]
    ys <- mapM z3Val params
    W3.AST <$> Z3.mkApp f' (xs ++ ys)

-- | Caches an AST in the solver state. If it already exists, return the
-- existing. Otherwise, build the new one.
--
-- This will fail (over the `MaybeT`) if the stored value is of a different type
-- than the symbolic identifier.
cacheAST :: MonadZ3 z3
         => ( Y.ComposedVal env a -> SolverRWS z3 env (W3.AST a) )
         -> ( Y.ExtAnyComposedVal env -> Maybe (Y.ComposedVal env a) )
         -> Int -- Symbolic env a
         -> SolverRWS z3 env (W3.AST a)
cacheAST fLookup fConstructor i =
  do
    fmap W3.AST $ do
      Left res <-
        cacheOver (_2 . sAstCache . lIntMapIdx i) $
          do
            (_, Y.ExtSymbolics syms) <- R.ask
            res <- liftRWSTMaybe $ IdList.lookup i syms
            Just x <- return $ fConstructor res
            fmap (Left . W3.unAST) (fLookup x)
      return res

u :: MonadZ3 z3 => z3 (W3.AST a) -> z3 Z3.AST
u = fmap W3.unAST

callFunc :: MonadZ3 z3 => z3 Z3.FuncDecl -> [z3 Z3.AST] -> z3 (W3.AST b)
callFunc f xs = W3.AST <$> join (Z3.mkApp <$> f <*> sequenceA xs)

freshSymbol :: (MonadZ3 m, MonadState SharedSolverState m) => m Z3.Symbol
freshSymbol =
  do
    val <- S.gets $ view sNumSymbols
    S.modify $ over sNumSymbols $ const (val + 1)
    Z3.mkIntSymbol val

liftRWSTMaybe :: (Monad m, Monoid w) => Maybe a -> RWST r w s (MaybeT m) a
liftRWSTMaybe a = RWST $ \_ s -> MaybeT $ return (fmap (,s,mempty) a)


-- # Helpers

-- ## Cache methods

-- | Returns the F32 sort from the state, or creates it
sortF32 :: (MonadZ3 m, MonadState SharedSolverState m) => m Z3.Sort
-- Strangely, Z3 crashes when sorts get an int-symbol. So, use a string.
sortF32 = cacheOver sSortF32 (Z3.mkUninterpretedSort =<< Z3.mkStringSymbol "EmptyFloat32")

sortF64 :: (MonadZ3 m, MonadState SharedSolverState m) => m Z3.Sort
-- Strangely, Z3 crashes when sorts get an int-symbol. So, use a string.
sortF64 = cacheOver sSortF64 (Z3.mkUninterpretedSort =<< Z3.mkStringSymbol "EmptyFloat64")

sortWorld :: (MonadZ3 m, MonadState SharedSolverState m) => m Z3.Sort
-- Strangely, Z3 crashes when sorts get an int-symbol. So, use a string.
sortWorld = cacheOver sSortWorld (Z3.mkUninterpretedSort =<< Z3.mkStringSymbol "world")

 -- f32bv -> f32
f32FromBv :: (MonadZ3 m, MonadState SharedSolverState m) => m Z3.FuncDecl
f32FromBv = cacheOver sF32FromBv $
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBvSort 32] <*> sortF32
    
-- f32 -> f32bv
f32ToBv :: (MonadZ3 m, MonadState SharedSolverState m) => m Z3.FuncDecl
f32ToBv = cacheOver sF32ToBv $
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [sortF32] <*> Z3.mkBvSort 32

-- f32 -> f32
f32Unop :: (MonadZ3 m, MonadState SharedSolverState m) => FUnop TF32 -> m Z3.FuncDecl
f32Unop i = cacheOver (sF32Unops . lHashMapIdx i) $
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [sortF32] <*> sortF32

-- f32 -> f32 -> f32
f32Binop :: (MonadZ3 m, MonadState SharedSolverState m) => FBinop TF32 -> m Z3.FuncDecl
f32Binop i = cacheOver (sF32Binops . lHashMapIdx i) $
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [sortF32, sortF32] <*> sortF32

-- f32 -> f32 -> bool
f32Relop :: (MonadZ3 m, MonadState SharedSolverState m) => FRelop TF32 -> m Z3.FuncDecl
f32Relop i = cacheOver (sF32Relops . lHashMapIdx i) $
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [sortF32, sortF32] <*> Z3.mkBoolSort

 -- f64bv -> f64
f64FromBv :: (MonadZ3 m, MonadState SharedSolverState m) => m Z3.FuncDecl
f64FromBv = cacheOver sF64FromBv $
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBvSort 64] <*> sortF64
    
-- f64 -> f64bv
f64ToBv :: (MonadZ3 m, MonadState SharedSolverState m) => m Z3.FuncDecl
f64ToBv = cacheOver sF64ToBv $
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [sortF64] <*> Z3.mkBvSort 64

-- f64 -> f64
f64Unop :: (MonadZ3 m, MonadState SharedSolverState m) => FUnop TF64 -> m Z3.FuncDecl
f64Unop i = cacheOver (sF64Unops . lHashMapIdx i) $
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [sortF64] <*> sortF64

-- f64 -> f64 -> f64
f64Binop :: (MonadZ3 m, MonadState SharedSolverState m) => FBinop TF64 -> m Z3.FuncDecl
f64Binop i = cacheOver (sF64Binops . lHashMapIdx i) $
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [sortF64, sortF64] <*> sortF64

-- f64 -> f64 -> bool
f64Relop :: (MonadZ3 m, MonadState SharedSolverState m) => FRelop TF64 -> m Z3.FuncDecl
f64Relop i = cacheOver (sF64Relops . lHashMapIdx i) $
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [sortF64, sortF64] <*> Z3.mkBoolSort

f64PromoteF32, f32DemoteF64, f32ConvertI32, f64ConvertI32, f32ConvertI64,
  f64ConvertI64
  :: (MonadZ3 m, MonadState SharedSolverState m) => m Z3.FuncDecl
f64PromoteF32 = cacheOver sF64PromoteF32 $ -- f32 -> f64
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [sortF32] <*> sortF64
f32DemoteF64 = cacheOver sF32DemoteF64 $   -- f64 -> f32
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [sortF64] <*> sortF32
f32ConvertI32 = cacheOver sF32ConvertI32 $ -- i32 -> f32
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBvSort 32] <*> sortF32
f64ConvertI32 = cacheOver sF64ConvertI32 $ -- i32 -> f64
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBvSort 32] <*> sortF64
f32ConvertI64 = cacheOver sF32ConvertI64 $ -- i64 -> f32
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBvSort 64] <*> sortF32
f64ConvertI64 = cacheOver sF64ConvertI64 $ -- i64 -> f64
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBvSort 64] <*> sortF64
  
i32TruncF32, i32TruncF64, i64TruncF32, i64TruncF64,
  i32TruncSatF32, i32TruncSatF64, i64TruncSatF32, i64TruncSatF64
  :: (MonadZ3 m, MonadState SharedSolverState m) => m Z3.FuncDecl
i32TruncF32    = cacheOver sI32TruncF32 $ -- bool/sx -> f32 -> i32
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBoolSort, sortF32] <*> Z3.mkBvSort 32
i32TruncF64    = cacheOver sI32TruncF64 $ -- bool/sx -> f64 -> i32
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBoolSort, sortF64] <*> Z3.mkBvSort 32
i64TruncF32    = cacheOver sI64TruncF32 $ -- bool/sx -> f32 -> i64
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBoolSort, sortF32] <*> Z3.mkBvSort 64
i64TruncF64    = cacheOver sI64TruncF64 $ -- bool/sx -> f64 -> f64
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBoolSort, sortF64] <*> Z3.mkBvSort 64
i32TruncSatF32 = cacheOver sI32TruncSatF32 $ -- bool/sx -> f32 -> i32
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBoolSort, sortF32] <*> Z3.mkBvSort 32
i32TruncSatF64 = cacheOver sI32TruncSatF64 $ -- bool/sx -> f64 -> i32
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBoolSort, sortF64] <*> Z3.mkBvSort 32
i64TruncSatF32 = cacheOver sI64TruncSatF32 $ -- bool/sx -> f32 -> i64
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBoolSort, sortF32] <*> Z3.mkBvSort 64
i64TruncSatF64 = cacheOver sI64TruncSatF64 $ -- bool/sx -> f64 -> f64
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [Z3.mkBoolSort, sortF64] <*> Z3.mkBvSort 64

-- Together, these capture the function: world -> i32 -> bool * world.
memGrowWorld, memGrowBool :: (MonadZ3 m, MonadState SharedSolverState m) => m Z3.FuncDecl
memGrowWorld = cacheOver sMemGrowWorld $ -- world -> i32 -> world
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [sortWorld, Z3.mkBvSort 32] <*> sortWorld
memGrowBool = cacheOver sMemGrowBool $ -- world -> i32 -> bool
  join $ Z3.mkFuncDecl <$> freshSymbol <*> sequenceA [sortWorld, Z3.mkBvSort 32] <*> Z3.mkBoolSort


-- # Helpers
  
toIntMap :: Integral i => Map i a -> IntMap a
toIntMap m =
  let vals = map (mapFst toNum) (Map.assocs m)
  in IntMap.fromList vals

-- | Makes the Z3 timeout consistent with the end time specified in the
-- `ReaderT` monad. If there is no time left at all, it fails over `MaybeT`.
enforceTimeout :: MonadZ3 z3 => SolverRWS z3 env ()
enforceTimeout =
  do
    currTime <- liftIO getTimeMs
    (mEndTime, _) <- R.ask

    case mEndTime of
      Nothing -> return ()
      Just endTime ->
        let remTime = endTime - currTime
        in
        if remTime <= 0 then
          -- No time left at all, so fail
          failRWS
        else
          updateZ3Timeout remTime

-- | Makes the Z3 timeout consistent with the end time specified in the
-- `ReaderT` monad. If there is no time left at all, it fails over `MaybeT`.
enforceDTimeout :: MonadZ3 z3 => DSolverRWS z3 env1 env2 ()
enforceDTimeout =
  do
    currTime <- liftIO getTimeMs
    (mEndTime, _, _) <- R.ask

    case mEndTime of
      Nothing -> return ()
      Just endTime ->
        let remTime = endTime - currTime
        in
        if remTime <= 0 then
          -- No time left at all, so fail
          failRWS
        else
          updateZ3Timeout remTime

updateZ3Timeout :: MonadZ3 z3 => TimeMs -> z3 ()
updateZ3Timeout t =
  do
    params <- Z3.mkParams
    paramName <- Z3.mkStringSymbol "timeout"
    Z3.paramsSetUInt params paramName (fromIntegral t)
    Z3.solverSetParams params

-- # Instances

instance MonadZ3 m => MonadZ3 (MaybeT m) where
  getSolver = lift Z3.getSolver -- MaybeT (fmap Just Z3.getSolver)

  getContext = lift Z3.getContext -- MaybeT (fmap Just Z3.getContext)

instance (MonadZ3 m, Monoid w) => MonadZ3 (RWST r w s m) where
  getSolver = lift Z3.getSolver -- RWST $ \_ s -> (,s,mempty) <$> Z3.getSolver

  getContext = lift Z3.getContext -- RWST $ \_ s -> (,s,mempty) <$> Z3.getContext
