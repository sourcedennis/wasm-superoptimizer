{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving, DeriveGeneric #-}

-- | Utilities and diagnostics monad for the superoptimizer
module Lang.Wasm.Superopt.MonadSuperopt
  ( MonadSuperopt (..)
  , SuperUtilStats (..)
  , runSuperUtil
  ) where

import Melude
-- Stdlib imports
import           Control.DeepSeq ( NFData, force )
import           GHC.Generics ( Generic )
import qualified System.Timeout as Timeout
import           Control.Exception ( evaluate )
import qualified Control.Monad.Writer as W
-- Extra stdlib imports
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import qualified System.Random as RNG
import           System.Random ( StdGen )
-- External library imports
import Control.Concurrent.ParallelIO.Local ( Pool, withPool, parallel )
-- Local library imports
import Lang.Wasm.Ast ( DefinedFunc, FuncIdx )
-- Local imports
import Lang.Wasm.Solver ( MonadSolver (..) )
import Binding.Z3.Z3Solver ( Z3Solver, runZ3SolverUntilAndCount )

import Control.Monad.IO.Class ( MonadIO ( liftIO ) )


-- | Program cost
type Cost = Int

type IsSuccess = Bool

-- | A monad for superoptimizer utilities.
class MonadSolver m => MonadSuperopt m where
  -- # Utils #

  -- | Evaluates the term to WHNF, if possible within the given time. Time is in
  -- milliseconds (1000ms = 1s). Also returns the time it took.
  timeout :: TimeMs -> m a -> m (Maybe (TimeMs, a))

  -- | Propagates a timeout to the provided computation. However, the
  -- computation itself is responsible for enforcing it (by obeying
  -- `timeRemaining` before performing an intensive action)
  softTimeout :: TimeMs -> m a -> m (TimeMs, a)

  -- | Returns the remaining time within the given timeout.
  timeRemaining :: m TimeMs

  -- | Uniformly selects a random number in the given range. The lower bound is
  -- /inclusive/, while the upper bound is /exclusive/.
  randomInRange :: (Int, Int) -> m Int

  -- | Uniformly selects a random float in the range [0,1)
  randomFloat :: m Float
  
  -- | Runs the computations in parallel, if possible.
  runParallel :: [m a] -> m [a]


  -- # Diagnostics #

  -- | Logs that an instruction sequence was generated, or this failed
  -- (if `Nothing`).
  logMicroGenDone :: TimeMs -> Maybe (Cost, Cost) -> m ()

  -- | Logs that an attempt was made at replacing a constant
  logConstantReplaced :: TimeMs -> IsSuccess -> m ()
  
  logZ3VerifyInvoked :: m ()

  -- | Logs that an attempt was made at eliminating a branch
  logBranchEliminated :: TimeMs -> IsSuccess -> m ()

  -- | This is called when one iteration from a loop is peeled off.
  logDriveDone :: m ()

  -- | An unreachable branch was eliminated.
  logEliminateDone :: m ()

  logReconstructionFailed :: m ()


-- | Internal. Reader state for `SuperUtils`
data SuperUtilsRS =
  SuperUtilsRS {
    -- | If this is `Just`, the computation lives within a `timeout`, whose end
    -- time is specified by this number. While the timeout is externally
    -- enforced, this value is necessary when passing it to external (FFI)
    -- computations.
    ssEndTime       :: TimeMs
    -- | A parallel pool. If `Nothing`, the program runs on a single thread.
  , ssParallelPool  :: Maybe Pool
  , ssIsVerbose     :: Bool
  }

data SuperUtilStats =
  SuperUtilStats {
    -- | (num success, num failed)
    ssConstants :: ( Int, Int )
  , -- | (num success, num failed)
    ssBranches  :: ( Int, Int )
  , -- | number of times Z3 was invoked to verify fragment equivalence
    ssZ3Verified  :: Int
  }
  deriving (Show, Generic)

instance NFData SuperUtilStats

instance Semigroup SuperUtilStats where
  (<>) a b =
    SuperUtilStats
      (sum2 (ssConstants a) (ssConstants b))
      (sum2 (ssBranches a) (ssBranches b))
      (ssZ3Verified a + ssZ3Verified b)
    where
    sum2 :: ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
    sum2 (a, b) (c, d) = (a+c, b+d)

instance Monoid SuperUtilStats where
  mempty = SuperUtilStats (0,0) (0,0) 0

-- | Utilities for the superoptimizer
newtype SuperUtils a = SuperUtils (RWST SuperUtilsRS SuperUtilStats StdGen IO a)
  deriving (Functor, Applicative, Monad)

runSuperUtil :: SuperUtils a
             -> TimeMs -- soft timeout
             -> Maybe Int -- Number of threads. Sequential if `Nothing`
             -> Bool
             -> IO (a, SuperUtilStats)
runSuperUtil (SuperUtils sus) t mNumThreads isVerbose = -- single thread
  do
    currTime <- getTimeMs
    let endTime = currTime + t
    
    case mNumThreads of
      Nothing -> 
        evalRWST sus (SuperUtilsRS endTime Nothing isVerbose) =<< RNG.newStdGen
      Just numThreads ->
        withPool numThreads $ \p ->
          evalRWST sus (SuperUtilsRS endTime (Just p) isVerbose) =<< RNG.newStdGen

instance MonadSolver SuperUtils where
  isSubset        = liftSolver .:   isSubset
  pickElement     = liftSolver .    pickElement
  isEmpty         = liftSolver .    isEmpty
  isNonZeroI32    = liftSolver .:   isNonZeroI32
  evalI32         = liftSolver .:   evalI32
  evalI64         = liftSolver .:   evalI64
  evalBool        = liftSolver .:   evalBool
  areUnequal      = liftSolver .::. areUnequal
  isUnsat         = liftSolver .    isUnsat

-- | Lifts a `Z3Solver` to a `SuperUtils`. The timeout from the `SuperUtils` is
-- taken into account.
liftSolver :: Z3Solver a -> SuperUtils a
liftSolver z =
  SuperUtils $ RWST $ \r s ->
    do
      (res, numCalls) <- runZ3SolverUntilAndCount z (Just $ ssEndTime r)
      return (res, s, mempty { ssZ3Verified = numCalls } )

instance MonadSuperopt SuperUtils where
  -- timeout :: TimeMs -> m a -> m (Maybe (TimeUs, a))
  timeout t (SuperUtils a) = SuperUtils $ RWST $ \r s ->
    do
      currTime <- getTimeMs
      -- The new end-time is the minimum of any other surrounding timeouts,
      -- and the new end-time. (In practise, timeouts are rarely nested)
      let newEndTime = min (ssEndTime r) (currTime + t)

      -- Run a contained computation with a timeout
      let r' = r { ssEndTime = newEndTime }
      res <- timeoutIO t $ runRWST a r' s

      case res of
        Nothing ->
          return (Nothing, s, mempty)
        Just (tTaken, (a, rng, v)) ->
          return (Just (tTaken, a), rng, v)
    where
    timeoutIO :: TimeMs -> IO a -> IO (Maybe (TimeMs, a))
    -- The native timeout is in microseconds (1 millisecond = 1000 microseconds)
    timeoutIO t a = Timeout.timeout (fromInteger t * 1000) (timedTask a)
    timedTask :: IO a -> IO (TimeMs, a)
    timedTask a =
      do
        startTime <- getTimeMs
        a' <- evaluate =<< a
        endTime <- getTimeMs
        return (endTime - startTime, a')

  softTimeout t (SuperUtils a) = SuperUtils $ RWST $ \r s ->
    do
      -- Soft timeouts are not enforced externally. Instead, the contained
      -- computation is responsible for checking violation with
      -- `timeRemaining`.
      currTime <- getTimeMs
      -- The new end-time is the minimum of any other surrounding timeouts,
      -- and the new end-time. (In practise, timeouts are rarely nested)
      let newEndTime = min (ssEndTime r) (currTime + t)
      let r' = r { ssEndTime = newEndTime }
      (a, rng, w) <- runRWST a r' s
      endTime <- getTimeMs
      let tTaken = endTime - currTime
      return ((tTaken, a), rng, w)
  
  timeRemaining =
    SuperUtils $ do
      currTime <- lift getTimeMs
      endTime <- R.asks ssEndTime
      return (endTime - currTime)

  -- randomInRange :: (Int, Int) -> m Int
  randomInRange (a,b) =
    SuperUtils $ S.state (RNG.randomR (a, b-1))

  -- randomFloat :: m Float
  randomFloat =
    SuperUtils $ S.state RNG.random
  
  -- runParallel :: [m a] -> m [a]
  runParallel xs =
    SuperUtils $ RWST $ \r sus ->
      case ssParallelPool r of
        Nothing -> -- Sequential
          let SuperUtils res = sequenceA xs
          in runRWST res r sus
        Just pool -> -- Parallel
          do
            let (g:gs) = splitStates sus
            let ys = zip xs gs
            res <- parallel pool $ map (runStep r) ys
            let vals = map (fst . fst) res
            return (vals, g, mconcat (map snd res))
    where
    splitStates :: StdGen -> [StdGen]
    splitStates g =
      let (g1, g2) = RNG.split g
      in g1 : splitStates g2
    runStep :: SuperUtilsRS -> (SuperUtils a, StdGen) -> IO ((a, StdGen), SuperUtilStats)
    runStep rs (SuperUtils su, sus) =
      do
        (a, s, w) <- runRWST su rs sus
        return ((a, s), w)

  -- logMicroGenDone :: TimeUs -> Maybe (Cost, Cost) -> m ()
  logMicroGenDone t Nothing =
    printShows (ß "Could not generate a fragment in time (" . shows t . ß "ms)")
  logMicroGenDone t (Just (a, b)) =
    printShows $
      ß "Small sequence replaced."
      . ß " Cost reduced from " . shows a . ß " to " . shows b
      . ß " (took " . shows t . ß "ms)"

  logConstantReplaced t True =
    SuperUtils (W.tell (mempty { ssConstants = (1, 0) }))
    >> printShows (ß "Constant replacement successful (" . shows t . ß "ms)")
  logConstantReplaced t False =
    SuperUtils (W.tell (mempty { ssConstants = (0, 1) }))
    >> printShows (ß "Constant replacement failed (" . shows t . ß "ms)")

  logZ3VerifyInvoked = 
    SuperUtils (W.tell (mempty { ssZ3Verified = 1 }))
    >> printShows (ß "Z3 called for verification")
    
  logBranchEliminated t True =
    SuperUtils (W.tell (mempty { ssBranches = (1, 0) }))
    >> printShows (ß "Branch elimination successful (" . shows t . ß "ms)")
  logBranchEliminated t False =
    SuperUtils (W.tell (mempty { ssBranches = (0, 1) }))
    >> printShows (ß "Branch elimination failed (" . shows t . ß "ms)")

  -- logDriveDone :: m ()
  logDriveDone =
    printShows (ß "Driving pass done")

  -- logEliminateDone :: m ()
  logEliminateDone =
    printShows (ß "Branch eliminated")
  
  logReconstructionFailed =
    printShows (ß "Program reconstruction failed (possibly by timeout)")

printShows :: ShowS -> SuperUtils ()
printShows f =
  SuperUtils $
    do
      isVerbose <- R.asks ssIsVerbose
      S.when isVerbose $ lift $ putStrLn (f "")


-- # Instances #

instance MonadSuperopt m => MonadSuperopt (MaybeT m) where
  -- timeout :: TimeUs -> m a -> m (Maybe (TimeUs, a))
  timeout t a = --lift .: timeout
    MaybeT $
      do
        res <- timeout t $ runMaybeT a
        case res of
          -- Timing failed
          Nothing           -> return $ Just Nothing
          -- Timing succeeded
          Just (t, Just a)  -> return $ Just $ Just (t, a)
          Just (_, Nothing) -> return Nothing -- Computation failed
  
  softTimeout t a =
    MaybeT $
      do
        (t, res) <- softTimeout t $ runMaybeT a
        case res of
          Just a  -> return $ Just (t, a)
          Nothing -> return Nothing -- Computation failed
  
  timeRemaining = lift timeRemaining

  -- randomInRange :: (Int, Int) -> m Int
  randomInRange = lift . randomInRange

  -- randomFloat :: m Float
  randomFloat = lift randomFloat
  
  -- runParallel :: [m a] -> m [a]
  runParallel = MaybeT . fmap sequenceA . runParallel . map runMaybeT

  -- logMicroGenDone :: TimeUs -> Maybe (Cost, Cost) -> m ()
  logMicroGenDone = lift .: logMicroGenDone
  
  logConstantReplaced = lift .: logConstantReplaced

  logZ3VerifyInvoked = lift logZ3VerifyInvoked

  logBranchEliminated = lift .: logBranchEliminated

  -- logDriveDone :: m ()
  logDriveDone = lift logDriveDone

  -- logEliminateDone :: m ()
  logEliminateDone = lift logEliminateDone

  logReconstructionFailed = lift logReconstructionFailed

instance MonadIO SuperUtils where
  liftIO = SuperUtils . liftIO
