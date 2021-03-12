{-# LANGUAGE FlexibleContexts, TupleSections, UnicodeSyntax, RankNTypes, TypeFamilies #-}

-- | My prelude =)
--
-- A collection of project-specific functions, which should be used together
-- with the default GHC prelude.
--
-- Warning: Most modules in the project depend on this module. When this module
-- changes, everything else must be recompiled.
module Melude
  ( TimeMs
  , trace
    -- * Lens rexports
  , Lens
  , Lens'
  , Prism
  , Prism'
  , lens
  , prism
  , prism'
  , over
  , view
  , (^.), (^?), (.~), (%~), (?~)
  , preview
  , _1, _2, _3, _4
  , _Just
  , makePrisms
  , makeLenses
    -- * My lenses
  , lHashMapIdx
  , lMapIdx
  , lIntMapIdx
  , cacheOver
    -- * Math function
  , clamp
    -- * Operators
  , (&), (<&>)
  , (.:)
  , (.:.)
  , (.::)
  , (.::.)
  , (.:::)
  , (<<=<)
  , (<<<=<)
    -- * Utils
  , mapFst
  , mapSnd
  , mapFstM
  , mapSndM
  , if'
  , applyIf
  , applyIfM
  , applyIfPred
  , applyIfPredM
  , applyMaybe
  , applyMaybeM
  , justM
  , applyAll
  , toNum
  , combine
  , combineL
  , combineR
  , mergeAsc
  , mergeAscWith
  , toMaybe
  , toMaybePred
  , isEqWith
  , isEqWithM
  , fmap2
  , zipWithExactM
  , zipWithExactNEM
  , failIf
  , failMaybe
  , failMaybeMsg
  , fail
    -- * Tuple utils
  , T2, T4, T8
  , apply1, apply2, apply4, apply8
  , zipWith2M_, zipWith4M_, zipWith8M_
  , foldr4, foldr8
    -- * Traversable utils
  , takeFirst
  , takeWhileJust
  , mapFirst
    -- * List utils
  , atIndex
  , lookupIndex
  , mapAtMaybe
  , mapAt
  , lengthLeq
  , mapFirstMatch
  , argMin
  , argMinM
  , takeExact
  , dropExact
  , splitAtExact
  , dropPrefix
  , splitPrefix
  , dropEqPrefix
  , splitEqPrefix
  , allEqVal
  , groupSliding
    -- * Comparison operators
  , cmpLength
  , eqLength
  , eqWithLen
  , eqWithLenNE
  , leqLength
  , cmpLexicographical
  , cmpTuple
  , cmpIndex
    -- * State monad operations
  , putFst
  , putSnd
  , modifyIf
  , withStateFst
  , withStateSnd
  , withState
  , withStateLens
  , modifyM
  , getsM
  , readerToState
  , whenS
    -- * String
  , intercalateShow
  , replicateShow
  , ß
    -- * Time
  , getTimeMs
    -- * Re-exports
  , fromMaybe, isJust, isNothing, mapMaybe, catMaybes
  , join, (>=>), (<=<)
  , foldM, forM
  , Natural
  , MonadFail
  , Hashable
  , Vector, (!?)
  , Identity ( Identity, runIdentity )
  , MonadState, State, StateT ( StateT, runStateT )
  , execState, runState, evalState, execStateT, evalStateT
  , MonadReader, Reader, ReaderT ( ReaderT, runReaderT ), runReader
  , RWST (..), RWS (..), rws, runRWS, evalRWS, execRWS, evalRWST, execRWST
  , zipWithM, zipWithM_
  , zipWith3M
  , MonadIO (..)
  , MaybeT (..)
  , NonEmpty ((:|))
  , liftA2
  , lift
  ) where

-- Stdlib imports
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty ((:|)), (<|) )
import           Numeric.Natural ( Natural )
import           Data.Maybe ( fromMaybe, isJust, isNothing, mapMaybe, catMaybes )
import qualified Debug.Trace as Trace
import           Data.Time.Clock.System ( getSystemTime, systemSeconds, systemNanoseconds )
-- Extra stdlib imports
import           Data.Hashable ( Hashable )
import qualified Data.HashMap.Lazy as HashMap
import           Data.Vector ( Vector, (!?) )
import           Data.HashMap.Lazy ( HashMap )
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.Map as Map
import           Data.Map ( Map )
import           Control.Monad.Trans.Maybe ( MaybeT (..) )
import           Control.Monad.Trans ( lift )
import           Control.Monad.IO.Class ( MonadIO (..) )
import           Control.Monad.Identity
  ( Identity (..), runIdentity )
import           Control.Monad
  ( join, (>=>), (<=<), foldM, forM, zipWithM, zipWithM_ )
import qualified Control.Monad.State as S
import           Control.Monad.State
  ( MonadState, State, StateT (..)
  , execState, runState, evalState, execStateT, runStateT, evalStateT
  )
import           Control.Monad.RWS
  ( RWST (..), RWS, rws, runRWS, evalRWS, execRWS, evalRWST, execRWST )
import           Control.Monad.Reader
  ( MonadReader, Reader, ReaderT (..), runReader, runReaderT )
import           Control.Applicative ( liftA2 )
import           Data.Functor ( ($>) )
import qualified Control.Monad.Fail as MF
import           Control.Monad.Fail ( MonadFail )
-- External imports
import qualified Control.Lens as Lens
import           Control.Lens
  ( Lens, Lens', Prism, Prism', lens, prism, prism', over, view, preview
  , _1, _2, _3, _4, _Just, (^.), (.~), (%~), (?~), (^?), (&), makeLenses
  )
import           Control.Lens.TH ( makePrisms )


-- | Time in /milliseconds/. (1s = 1000ms)
type TimeMs = Integer

trace :: String -> a -> a
trace = Trace.trace
-- trace = const id


-- # Operators

-- | Flipped `<$>`
(<&>) :: Functor f => f a -> ( a -> b ) -> f b
(<&>) a f = f <$> a

infixr 9 .:
infixr 9 .:.
infixr 9 .::
infixr 9 .::.
infixr 9 .:::

-- | Composes two functions, where the seconds one is fed /two/ arguments.
-- Intuitively, the number of dots after the first signifies the argument count.
(.:) :: ( c -> d ) -> ( a -> b -> c ) -> a -> b -> d
(.:) g f a = g . f a

-- | Composes two functions, where the seconds one is fed /three/ arguments.
-- Intuitively, the number of dots after the first signifies the argument count.
(.:.) :: ( d -> e ) -> ( a -> b -> c -> d ) -> a -> b -> c -> e
(.:.) g f a = g .: f a

-- | Composes two functions, where the seconds one is fed /four/ arguments.
-- Intuitively, the number of dots after the first signifies the argument count.
(.::) :: ( e -> f ) -> ( a -> b -> c -> d -> e ) -> a -> b -> c -> d -> f
(.::) g f a = g .:. f a

-- | Composes two functions, where the seconds one is fed /five/ arguments.
-- Intuitively, the number of dots after the first signifies the argument count.
(.::.) :: ( f -> g ) -> ( a -> b -> c -> d -> e -> f ) -> a -> b -> c -> d -> e -> g
(.::.) g f a = g .:: f a

-- | Composes two functions, where the seconds one is fed /six/ arguments.
-- Intuitively, the number of dots after the first signifies the argument count.
(.:::) :: ( g -> h ) -> ( a -> b -> c -> d -> e -> f -> g ) -> a -> b -> c -> d -> e -> f -> h
(.:::) g f a = g .::. f a

(<<=<) :: Monad m => (c -> m d) -> (a -> b -> m c) -> a -> b -> m d
(<<=<) f g a = f <=< g a

(<<<=<) :: Monad m => (d -> m e) -> (a -> b -> c -> m d) -> a -> b -> c -> m e
(<<<=<) f g a = f <<=< g a


-- # My lenses

-- | Constructs a lens for a key into a `HashMap`.
lHashMapIdx :: (Eq a, Hashable a) => a -> Lens' (HashMap a b) (Maybe b)
lHashMapIdx a = lens (HashMap.lookup a) (\m b -> HashMap.alter (const b) a m)

-- | Constructs a lens for a key into a `Map`.
lMapIdx :: Ord a => a -> Lens' (Map a b) (Maybe b)
lMapIdx a = lens (Map.lookup a) (\m b -> Map.alter (const b) a m)

-- | Constructs a lens for a key into a `IntMap`.
lIntMapIdx :: Int -> Lens' (IntMap b) (Maybe b)
lIntMapIdx i = lens (IntMap.lookup i) (\m b -> IntMap.alter (const b) i m)

-- | The state possibly contains a cache of elements, which are created upon
-- their first use. The lens is used to access this cache within the state.
--
-- Sometimes a computation keeps a cache around, like:
-- `Int -> State (IntMap a) a`.
-- Recomputation of the value is not necessary when it was previously stored.
--
-- Example:
-- >>> runState (cacheOver (lIntMapIdx 0) (pure 10)) IntMap.empty
-- (10,fromList [(0,10)])
--
-- This will compute @pure 10@ and store it at index 0. If it were previously
-- stored at index 0, that previous result would've been returned.
cacheOver :: MonadState a m => Lens' a (Maybe b) -> m b -> m b
cacheOver l mB =
  do
    a <- S.get
    case view l a of
      Just b  -> return b
      Nothing ->
        do
          b <- mB
          S.modify $ over l (const $ Just b)
          return b


-- # Math function

-- | Constraints the value within the given range.
--
-- Example:
-- >>> clamp 5 10 3
-- 5
-- >>> clamp 5 10 12
-- 10
clamp :: Ord a
      => a -- ^ Lower bound
      -> a -- ^ Upper bound
      -> a -- ^ Value
      -> a -- ^ Constrained value
clamp l h = min h . max l


-- # Utils

-- | Applies the function to the first tuple element
mapFst :: ( a -> c ) -> ( a, b ) -> ( c, b )
mapFst f (a, b) = (f a, b)

-- | Applies the function to the second tuple element
mapSnd :: ( b -> c ) -> ( a, b ) -> ( a, c )
mapSnd f (a, b) = (a, f b)

-- | Applies the function to the first tuple element
mapFstM :: Monad m => ( a -> m c ) -> ( a, b ) -> m ( c, b )
mapFstM f (a, b) = (,b) <$> f a

-- | Applies the function to the second tuple element
mapSndM :: Monad m => ( b -> m c ) -> ( a, b ) -> m ( a, c )
mapSndM f (a, b) = (a,) <$> f b

mapTuple :: ( a -> c ) -> ( b -> d ) -> (a, b) -> (c, d)
mapTuple f g (a, b) = (f a, g b)

-- | Alternative syntax for if-then-else. This composes better as a function.
--
-- See the discussion here: https://wiki.haskell.org/If-then-else
if' :: Bool -> a -> a -> a
if' True  a _ = a
if' False _ a = a

-- | Applies the function only if the condition is `True`
applyIf :: Bool -> ( a -> a ) -> a -> a
applyIf True  f a = f a
applyIf False _ a = a

-- | Applies the function only if the condition is `True`
applyIfM :: Monad m => Bool -> ( a -> m a ) -> a -> m a
applyIfM True  f = f
applyIfM False _ = return

-- | Applies the function only if the predicate is `True` for the element.
--
-- Example:
-- >>> applyIfPred (>= 5) (* 2) 10
-- 20
applyIfPred :: ( a -> Bool ) -> ( a -> a ) -> a -> a
applyIfPred f g a = applyIf (f a) g a

-- | Applies the function only if the predicate is `True` for the element.
applyIfPredM :: Monad m => ( a -> m Bool ) -> ( a -> m a ) -> a -> m a
applyIfPredM f g a =
  do
    b <- f a
    applyIfM b g a

-- | Applies the function with the value in the @Just@, or returns the original
-- argument if @Nothing@.
applyMaybe :: Maybe b -> ( b -> a -> a ) -> a -> a
applyMaybe Nothing  _ a = a
applyMaybe (Just b) f a = f b a

-- | Applies the function with the value in the @Just@, or returns the original
-- argument if @Nothing@.
applyMaybeM :: Monad m => Maybe b -> ( b -> a -> m a ) -> a -> m a
applyMaybeM Nothing  _ = return
applyMaybeM (Just b) f = f b

-- | Applies the monad if the value is available.
justM :: Monad m => ( a -> m () ) -> Maybe a -> m ()
justM = maybe (return ())

-- | Similar to `map`, but instead applies multiple functions to a single
-- element.
applyAll :: [ a -> b ] -> a -> [b]
applyAll xs x = map ($ x) xs

-- | Converts a value of `Integral` type to a value of `Num` type.
toNum :: (Integral a, Num b) => a -> b
toNum = fromInteger . toInteger

-- | Combines the elements within the `Maybe`s.
combine :: ( a -> a -> a ) -> Maybe a -> Maybe a -> Maybe a
combine f (Just a) (Just b)  = Just $ f a b
combine _ (Just a) Nothing   = Just a
combine _ Nothing  (Just b)  = Just b
combine _ Nothing  Nothing   = Nothing

-- | Combines the value with the other value inside the `Maybe`. Similar to
-- `combine`, but the /left/ element is surely present.
combineL :: ( a -> b -> a ) -> a -> Maybe b -> a
combineL f a (Just b) = f a b
combineL _ a Nothing  = a

-- | Combines the value with the other value inside the `Maybe`. Similar to
-- `combine`, but the /right/ element is surely present.
combineR :: ( a -> b -> b ) -> Maybe a -> b -> b
combineR f (Just a) b = f a b
combineR _ Nothing  b = b

-- | Merges many ascending lists into a ascending list.
--
-- If any input list is /not/ ascending, the output list likely won't be either.
-- This property is /not/ tested on the input.
mergeAsc :: Ord a => [[a]] -> [a]
mergeAsc = mergeAscWith (<=)

-- | Merges multiple ascending lists into a single ascending list.
--
-- If any input list is /not/ ascending, the output list likely won't be either.
-- This property is /not/ tested on the input.
mergeAscWith :: ( a -> a -> Bool ) -> [[a]] -> [a]
mergeAscWith fLeq = foldr (mergeAsc2 fLeq) []
  where
  mergeAsc2 :: ( a -> a -> Bool ) -> [a] -> [a] -> [a]
  mergeAsc2 fLeq (x:xs) (y:ys)
    | fLeq x y   = x : mergeAsc2 fLeq xs (y:ys)
    | otherwise  = y : mergeAsc2 fLeq (x:xs) ys
  mergeAsc2 _ xs [] = xs
  mergeAsc2 _ [] ys = ys

-- | Returns `Just` when the condition is `True`.
toMaybe :: Bool -> a -> Maybe a
toMaybe True  a = Just a
toMaybe False _ = Nothing

-- | Returns `Just` when the predicate is `True` for the element.
toMaybePred :: ( a -> Bool ) -> a -> Maybe a
toMaybePred f a = toMaybe (f a) a

-- | Returns `True` iff both lists have equal length and their values are equal
-- by the given predicate.
isEqWith :: ( a -> b -> Bool ) -> [a] -> [b] -> Bool
isEqWith f xs ys =
  cmpLength xs ys == EQ && and (zipWith f xs ys)

-- | Returns `True` iff both lists have equal length and their values are equal
-- by the given predicate, which is contained in a monad.
isEqWithM :: Monad m => ( a -> b -> m Bool ) -> [a] -> [b] -> m Bool
isEqWithM f xs ys =
  (&&) (cmpLength xs ys == EQ) <$> (and <$> zipWithM f xs ys)

-- | Maps over two nested `Functor`s.
fmap2 :: (Functor f1, Functor f2) => ( a -> b ) -> f1 (f2 a) -> f1 (f2 b)
fmap2 = fmap . fmap

-- | Like `zipWithM`, but fails over the `MonadFail` if the list lengths differ.
zipWithExactM :: MonadFail m => ( a -> b -> m c ) -> [a] -> [b] -> m [c]
zipWithExactM f xs ys
  | eqLength xs ys  = zipWithM f xs ys
  | otherwise       = fail "Unequal list lengths"
    
-- | Like `zipWithM`, but fails over the `MonadFail` if the list lengths differ.
zipWithExactNEM :: MonadFail m => ( a -> b -> m c ) -> NonEmpty a -> NonEmpty b -> m (NonEmpty c)
zipWithExactNEM f (x:|xs) (y:|ys)
  | eqLength xs ys  = (:|) <$> f x y <*> zipWithM f xs ys
  | otherwise       = fail "Unequal list lengths"

-- | Like `zipWithM` , but on 3 arguments
zipWith3M :: Applicative m => ( a -> b -> c -> m d ) -> [a] -> [b] -> [c] -> m [d]
zipWith3M f (x:xs) (y:ys) (z:zs) = (:) <$> f x y z <*> zipWith3M f xs ys zs
zipWith3M _ _ _ _ = pure []

-- | Fails over the `MonadFail` iff the condition is `True`.
failIf :: MonadFail m => Bool -> String -> m ()
failIf True  = fail
failIf False = const $ return ()

-- | Lifts the `Maybe` to a `MonadFail`.
failMaybe :: MonadFail m => Maybe a -> m a
failMaybe = failMaybeMsg "Nothing"

-- | Lifts the `Maybe` to a `MonadFail`, with a custom message.
failMaybeMsg :: MonadFail m => String -> Maybe a -> m a
failMaybeMsg msg Nothing  = fail msg
failMaybeMsg msg (Just a) = return a


-- # Tuple utils

type T2 a = (a,a)
type T4 a = (a,a,a,a)
type T8 a = (a,a,a,a,a,a,a,a)

apply1 :: Applicative m => ( a -> m b ) -> a -> m b
apply1 = ($)

apply2 :: Applicative m => ( a -> m b ) -> T2 a -> m (T2 b)
apply2 f (x,y) = (,) <$> f x <*> f y

apply4 :: Applicative m => ( a -> m b ) -> T4 a -> m (T4 b)
apply4 f (x,y,z,w) = (,,,) <$> f x <*> f y <*> f z <*> f w

apply8 :: Applicative m => ( a -> m b ) -> T8 a -> m (T8 b)
apply8 f (a,b,c,d,x,y,z,w)
  = (,,,,,,,) <$> f a <*> f b <*> f c <*> f d <*> f x <*> f y <*> f z <*> f w

zipWith2M_ :: Applicative m => ( a -> b -> m c ) -> T2 a -> T2 b -> m ()
zipWith2M_ f (a,b) (x,y) = f a x *> f b y $> ()

zipWith4M_ :: Applicative m => ( a -> b -> m c ) -> T4 a -> T4 b -> m ()
zipWith4M_ f (x0,x1,x2,x3) (y0,y1,y2,y3)
  = f x0 y0 *> f x1 y1 *> f x2 y2 *> f x3 y3 $> ()

zipWith8M_ :: Applicative m => ( a -> b -> m c ) -> T8 a -> T8 b -> m ()
zipWith8M_ f (x0,x1,x2,x3,x4,x5,x6,x7) (y0,y1,y2,y3,y4,y5,y6,y7)
  = f x0 y0 *> f x1 y1 *> f x2 y2 *> f x3 y3
    *> f x4 y4 *> f x5 y5 *> f x6 y6 *> f x7 y7 $> ()

foldr4 :: (a -> b -> b) -> b -> T4 a -> b
foldr4 f q (a,b,c,d) = f a $ f b $ f c $ f d q

foldr8 :: (a -> b -> b) -> b -> T8 a -> b
foldr8 f q (a,b,c,d,x,y,z,w) =
  f a $ f b $ f c $ f d $ f x $ f y $ f z $ f w q


-- # Traversable utils

-- | Returns the first value for which the provided function returns `Just`.
takeFirst :: Foldable t => ( a -> Maybe b ) -> t a -> Maybe b
takeFirst f = foldr (firstMaybe . f) Nothing
  where
  -- Lazy in the second argument
  firstMaybe :: Maybe b -> Maybe b -> Maybe b
  firstMaybe (Just a) _ = Just a
  firstMaybe Nothing  b = b

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust []          = []
takeWhileJust (Just x:xs) = x : takeWhileJust xs
takeWhileJust (Nothing:_) = []

-- | Applies the function to the first element to which it returns `Just`.
--
-- Example:
-- >>> take 10 $ mapFirst (fmap (*2) . toMaybePred (>= 5)) [1..]
-- [1,2,3,4,10,6,7,8,9,10]
mapFirst :: Traversable t => ( a -> Maybe a ) -> t a -> t a
mapFirst f xs = evalState (traverse (step f) xs) False
  where
  -- | Tracks a `Bool` across the elements. It starts at `False`, but becomes
  -- `True` after the first element is mapped.
  step :: ( a -> Maybe a ) -> a -> State Bool a
  step f a =
    ifStateBool
      (return a)
      (maybe (return a) ((>>) (S.put True) . return) (f a))
  -- | Returns the first state if the current state is `True`. Returns the
  -- second otherwise.
  ifStateBool :: State Bool a -> State Bool a -> State Bool a
  ifStateBool ifTrue ifFalse = S.get >>= \b -> if' b ifTrue ifFalse


-- # List utils

-- | /O(n)/. Returns the element that is at the given index within the array. If
-- no such element exists, `Nothing` is returned.
atIndex :: Integral i => i -> [a] -> Maybe a
atIndex _ []    = Nothing
atIndex 0 (x:_) = Just x
atIndex n (_:xs)
  | n < 0      = Nothing
  | otherwise  = atIndex (n-1) xs

-- | /O(log n)/ lookup. /O(n log n)/ construction. Constructs an efficient
-- lookup function.
--
-- >>> lookupIndex [5,6,7,32] 3
lookupIndex :: [a] -> ( Int -> Maybe a )
lookupIndex xs =
  let m = IntMap.fromList $ zip [0..] xs
  in (`IntMap.lookup` m)

-- -- | Returns a function which maps values to their /first/ index in the list.
-- --
-- -- >>> findIndex [42 :: Int,33,99,12,33] 33
-- -- Just 1
-- findIndex :: (Eq a, Hashable a) => [a] -> ( a -> Maybe Int )
-- findIndex xs =
--   let m = HashMap.fromList $ reverse $ zip xs [0..]
--   in (`HashMap.lookup` m)

-- | Maps the function over the element at the given location. The updated list
-- is returned. If no such element exists, `Nothing` is returned.
--
-- See also `mapAt`.
mapAtMaybe :: Integral i => ( a -> Maybe a ) -> i -> [a] -> Maybe [a]
mapAtMaybe _ _ [] = Nothing
mapAtMaybe f 0 (x:xs) = (:) <$> f x <*> pure xs
mapAtMaybe f n (x:xs)
  | n < 0      = Nothing
  | otherwise  = (x:) <$> mapAtMaybe f (n-1) xs

-- | Maps the function over the element at the given location. The updated list
-- is returned. If no such element exists, the original list is returned.
--
-- The following relation to `mapAtMaybe` holds:
-- `mapAt f i xs == fromMaybe xs (mapAtMaybe f i xs)`
mapAt :: Integral i => ( a -> a ) -> i -> [a] -> [a]
mapAt _ _ [] = []
mapAt f 0 (x:xs) = f x : xs
mapAt f n (x:xs)
  | n < 0      = []
  | otherwise  = x : mapAt f (n-1) xs

-- | Tests whether the length of the list is less-than-or-equal to the given
-- number. This terminates on infinite lists.
--
-- Using this method is likely faster than using @length xs < n@, as no more
-- than @n@ elements are traversed in the list.
--
-- >>> lengthLeq 3 "bob"
-- True
--
-- >>> lengthLeq 3 (repeat 42)
-- False
lengthLeq :: Natural -> [a] -> Bool
lengthLeq _ []     = True -- n >= 0; True for Natural
lengthLeq 0 (_:_)  = False
lengthLeq n (_:xs) = lengthLeq (n-1) xs

-- | Applies the function to the first element satisfying the predicate. If this
-- application fails (by returning `Nothing`), this function returns `Nothing`.
-- Also, if no element satisfied the predicate, `Nothing` is returned.
mapFirstMatch :: ( a -> Maybe b ) -> ( b -> Maybe a ) -> [a] -> Maybe [a]
mapFirstMatch _ _ [] = Nothing
mapFirstMatch fMatch fMap (x:xs) =
  case fMatch x of
    Nothing -> (x:) <$> mapFirstMatch fMatch fMap xs
    Just b  -> (:) <$> fMap b <*> pure xs

-- | Returns the element in the collection whose function result is minimal.
argMin :: (Foldable t, Ord b) => ( a -> b ) -> t a -> Maybe a
argMin f = foldr (step f) Nothing
  where
  step :: Ord b => ( a -> b ) -> a -> Maybe a -> Maybe a
  step _ x Nothing = Just x
  step f x (Just y)
    | f x < f y  = Just x
    | otherwise  = Just y

-- | Returns the element in the collection whose function result is minimal.
argMinM :: (Monad m, Foldable t, Ord b) => ( a -> m b ) -> t a -> m (Maybe a)
argMinM f = fmap (fmap fst) . foldM (step f) Nothing
  where
  step :: (Monad m, Ord b) => ( a -> m b ) -> Maybe (a,b) -> a -> m (Maybe (a,b))
  step f Nothing         xa = Just . (xa,) <$> f xa
  step f (Just (ya, yb)) xa =
    do
      xb <- f xa
      if xb < yb then
        return $ Just (xa, xb)
      else
        return $ Just (ya, yb)

-- | Returns the list prefix of the amount specified by the first argument. When
-- fewer elements are available, `Nothing` is returned.
--
-- Example:
-- >>> takeExact 3 [1,2,3,4]
-- Just [1,2,3]
--
-- >>> takeExact 5 [1,2,3,4]
-- Nothing
takeExact :: MonadFail m => Int -> [a] -> m [a]
takeExact 0 _  = return []
takeExact _ [] = fail "Insufficient list elements"
takeExact n (x:xs)
  | n < 0      = fail $ ß"Cannot take " . shows n . ß" elements" $ ""
  | otherwise  = (x:) <$> takeExact (n-1) xs

-- | Returns the list suffix, where the first argument specifies how many
-- elements are omitted. When fewer elements are available, `Nothing` is
-- returned.
--
-- Example:
-- >>> dropExact 3 [1,2,3,4]
-- Just [4]
-- >>> dropExact 5 [1,2,3,4]
-- Nothing
dropExact :: MonadFail m => Int -> [a] -> m [a]
dropExact 0 xs = return xs
dropExact _ [] = fail "Insufficient list elements"
dropExact n (_:xs)
  | n < 0      = fail $ ß"Cannot drop " . shows n . ß" elements" $ ""
  | otherwise  = dropExact (n-1) xs

-- |
--
-- >>> splitAtExact 3 [1,2,3,4]
-- Just ([1,2,3],[4])
splitAtExact :: MonadFail m => Int -> [a] -> m ([a],[a])
splitAtExact 0 xs = return ([],xs)
splitAtExact _ [] = fail "Insufficient list elements"
splitAtExact n (x:xs)
  | n < 0      = fail $ ß"Cannot drop " . shows n . ß" elements" $ ""
  | otherwise  = mapFst (x:) <$> splitAtExact (n-1) xs

-- | Drops the /entire/ prefix iff the prefix elements pairwise satisfy the
-- predicates.
--
-- >>> dropPrefix [(==1), (==2)] [1,2,3,4]
-- Just [3,4]
dropPrefix :: MonadFail m => [a -> Bool] -> [a] -> m [a]
dropPrefix = fmap snd .: splitPrefix

-- |
-- >>> splitPrefix [(==1), (==2)] [1,2,3,4]
-- Just ([1,2],[3,4])
splitPrefix :: MonadFail m => [a -> Bool] -> [a] -> m ([a], [a])
splitPrefix (f:fs) (x:xs)
  | f x        = mapFst (x:) <$> splitPrefix fs xs
  | otherwise  = fail "Mismatching prefix"
splitPrefix (_:_) [] = fail "Mismatching prefix"
splitPrefix []    xs = return ([], xs)

-- | Drops the prefix if it matches the given elements.
--
-- >>> dropEqPrefix [1,2] [1,2,3] :: Maybe [Int]
-- Just [3]
--
-- >>> dropEqPrefix [1,4] [1,2,3] :: Maybe [Int]
-- Nothing
dropEqPrefix :: (MonadFail m, Eq a) => [a] -> [a] -> m [a]
dropEqPrefix = dropPrefix . map (==)

-- |
--
-- >>> splitEqPrefix [1,2] [1,2,3] :: Maybe ([Int], [Int])
-- Just ([1,2],[3])
splitEqPrefix :: (MonadFail m, Eq a) => [a] -> [a] -> m ([a], [a])
splitEqPrefix = splitPrefix . map (==)

-- | If all elements in the list are equal, return that value.
allEqVal :: (MonadFail m, Eq a) => NonEmpty a -> m a
allEqVal (x :| []) = return x
allEqVal (x :| (y:ys))
  | x == y     = allEqVal (y :| ys)
  | otherwise  = fail "List elements unequal"

-- | Groups adjacent elements that satisfy the predicate.
--
-- Example:
-- >>> map NE.toList $ groupSliding (\a b -> b - a <= 2) [1,2,4,7,11,12,13,16]
-- [[1,2,4],[7],[11,12,13],[16]]
groupSliding :: ( a -> a -> Bool ) -> [a] -> [NonEmpty a]
groupSliding f []     = []
groupSliding f (x:xs) = NE.toList $ groupSliding' f (x :| xs)
  where
  groupSliding' :: ( a -> a -> Bool ) -> NonEmpty a -> NonEmpty (NonEmpty a)
  groupSliding' _ (x :| []) = (x :| []) :| []
  groupSliding' f (x :| (y:ys))
    | f x y      =
        let (zs :| zss) = groupSliding' f (y :| ys)
        in (x <| zs) :| zss
    | otherwise  =
        (x :| []) <| groupSliding' f (y :| ys)


-- # Comparison operators

-- | Compares the lengths of the lists. If either of the lists has finite
-- length, this function will terminate.
--
-- >>> [1] `cmpLength` repeat 3
-- LT
--
-- >>> repeat 3 `cmpLength` [1]
-- GT
--
-- >>> [1,2,3] `cmpLength` [6,6,6]
-- EQ
cmpLength :: [a] -> [b] -> Ordering
cmpLength (_:xs) (_:ys) = cmpLength xs ys
cmpLength (_:_)  []     = GT
cmpLength []     (_:_)  = LT
cmpLength []     []     = EQ

-- | Checks whether both lists have equal length.
--
-- This performs similar to @eqLength xs ys == (length xs == length ys)@,
-- except that it terminates if at least one of the input lists is finite.
eqLength :: [a] -> [b] -> Bool
eqLength xs ys = cmpLength xs ys == EQ

-- | Checks whether the lists have equal lengths and elements.
eqWithLen :: ( a -> b -> Bool ) -> [a] -> [b] -> Bool
eqWithLen f xs ys = cmpLength xs ys == EQ && and (zipWith f xs ys)

-- | Checks whether the lists have equal lengths and elements.
eqWithLenNE :: ( a -> b -> Bool ) -> NonEmpty a -> NonEmpty b -> Bool
eqWithLenNE f (x:|xs) (y:|ys) = eqWithLen f (x:xs) (y:ys)

-- | Returns `True` if the second list is at least as long as the first. If (at
-- least) either of the lists has a finite length, this function will terminate.
--
-- Similar to @length xs ≤ length ys@, except it terminates when (at least) one
-- of the input lists has finite length. It is also likely faster, as fewer
-- elements are traversed.
--
-- >>> [1] `leqLength` repeat 3
-- True
--
-- >>> repeat 3 `leqLength` [1]
-- False
leqLength :: [a] -> [b] -> Bool
leqLength xs ys = cmpLength xs ys /= GT

-- |
cmpLexicographical :: ( a -> b -> Ordering ) -> [a] -> [b] -> Ordering
cmpLexicographical _ [] [] = EQ
cmpLexicographical _ [] _  = LT
cmpLexicographical _ _  [] = GT
cmpLexicographical f (x:xs) (y:ys) =
  case f x y of
    EQ  -> cmpLexicographical f xs ys
    res -> res

cmpTuple :: ( a -> c -> Ordering ) -> ( b -> d -> Ordering ) -> (a,b) -> (c,d) -> Ordering
cmpTuple f g (a,b) (c,d)  =
  case f a c of
    EQ  -> g b d
    res -> res

-- | Define an ordering by their index in the given list. This is particularly
-- useful for data structures for which defining an `Ord` instance is
-- undesirable.
--
-- >>> cmpIndex [4..7] 7 4
-- GT
cmpIndex :: Eq a => [a] -> a -> a -> Ordering
cmpIndex []     _ _ = EQ -- no ordering defined
cmpIndex (x:xs) y z
  | y == z     = EQ
  | x == y     = LT
  | x == z     = GT
  | otherwise  = cmpIndex xs y z


-- # State monad operations

putFst :: MonadState (a, b) m => a -> m ()
putFst = S.modify . mapFst . const

putSnd :: MonadState (a, b) m => b -> m ()
putSnd = S.modify . mapSnd . const

-- | Like `Control.Monad.State.modify`, but only modifies if the condition is
-- `True`.
modifyIf :: MonadState s m => Bool -> ( s -> s ) -> m ()
modifyIf True  f = S.modify f
modifyIf False _ = return ()

-- | Run a computation on the first state element
withStateFst :: Monad m => StateT s1 m a -> StateT (s1,s2) m a
withStateFst = withState fst (mapFst . const)

-- | Run a computation on the second state element
withStateSnd :: Monad m => StateT s2 m a -> StateT (s1,s2) m a
withStateSnd = withState snd (mapSnd . const)

-- | Run a computation on a contained state
--
-- Example: @withState snd (mapSnd . const)@
withState :: Monad m => ( s2 -> s1 ) -> ( s1 -> s2 -> s2 ) -> StateT s1 m a -> StateT s2 m a
withState f g x = StateT $ \s2 -> fmap (mapSnd (`g` s2)) (runStateT x (f s2))

-- | Run a computation on a contained state, where a `Lens` describes the
-- containment.
withStateLens :: Monad m => Lens' s2 s1 -> StateT s1 m a -> StateT s2 m a
withStateLens l s =
  StateT $ \s2 ->
    do
      let s1 = view l s2
      (a, s1') <- runStateT s s1
      let s2' = Lens.set l s1' s2
      return (a, s2')

modifyM :: Monad m => ( s -> m s ) -> StateT s m ()
modifyM f = S.get >>= lift . f >>= S.put

getsM :: Monad m => ( s -> m a ) -> StateT s m a
getsM f = lift . f =<< S.get

-- | Convert a `ReaderT` into a `StateT`.
--
-- Defining operations that do not modify the state are best defined as a
-- `Reader`, as it further restricts the type. However, it must sometimes be
-- included into a `State` computation; hence this function exists.
readerToState :: Monad m => ReaderT s m a -> StateT s m a
readerToState r = StateT $ \s -> (,) <$> runReaderT r s <*> pure s

whenS :: MonadState s m => ( s -> Bool ) -> m () -> m ()
whenS f s =
  do
    isOk <- S.gets f
    S.when isOk s


-- # String

-- | Like `Data.List.intercalate`, but operates on `ShowS` elements.
--
-- Example:
-- >>> intercalateShow (showString ",") (map shows [1,2,3,4]) ""
-- "1,2,3,4"
intercalateShow :: ShowS -> [ShowS] -> ShowS
intercalateShow _ []     = id
-- Specifically use `foldl`, as `foldr` quickly causes a stack overflow
intercalateShow v (x:xs) = foldl (\b a -> b.v.a) x xs

replicateShow :: Int -> ShowS -> ShowS
replicateShow 0 _ = id
replicateShow i s = s . replicateShow (i-1) s

-- | Abbreviation alias for `showString`.
ß :: String -> ShowS
ß = showString


-- # Time

-- | Returns the current time in milliseconds.
getTimeMs :: IO TimeMs
getTimeMs =
  -- Maybe this does not belong in the melude? Though, it's important all
  -- modules use the same time reference. So, keep here for now.
  do
    t <- getSystemTime
    return $ fromIntegral (systemNanoseconds t `div` 10^6) + fromIntegral (systemSeconds t)  * 10^3


instance String ~ err => MonadFail (Either err) where
  -- fail :: String -> Either String a
  fail = Left
