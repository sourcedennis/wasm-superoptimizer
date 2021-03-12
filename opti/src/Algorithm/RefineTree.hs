{-# LANGUAGE RankNTypes, TupleSections, ScopedTypeVariables #-}

-- | Partitions a set of elements by counter examples, and constructs a
-- (non-binary) /decision tree/ to easily search for identical elements.
--
-- If any two /different/ elements can be distinguished by their behavior w.r.t.
-- some property, then a decision tree can be built that distinguishes elements
-- on these properties.
--
-- The important application of this - within this project - is distinguishing
-- programs by their behavior on input test cases. After all, if two programs
-- produce different outputs for a particular test case, the programs are surely
-- unequal.
module Algorithm.RefineTree
  ( RefineTree (..)
  , build
  , searchEq
  , height
  , size
  , groups
  ) where

import Melude
-- Stdlib imports
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty ((:|)) )
-- Extra stdlib imports
import           Data.Hashable ( Hashable )
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict ( HashMap )


-- | Partitions the set of @a@ on counter examples
--
-- This effectively builds a /decision tree/ on input-output pairs.
data RefineTree kIn kOut a
  = RefineNode kIn (HashMap kOut (RefineTree kIn kOut a))
  | RefineLeaf (NonEmpty a)
  deriving Show

-- | Builds the refine tree.
--
-- The number of prove calls is /linear/ in the number of input elements (often
-- even sublinear). However, execution time seems dominated by /O(n (log n)^2)/
-- time needed for construction of all `HashMap`s.
--
-- (It creates /O(n)/ hashmaps, whose summed size is also /O(n)/)
build :: forall m a kIn kOut
       . ( Monad m, Eq kOut, Hashable kOut )
      => ( forall b . [m b] -> m [b] )
      -> ( a -> a -> m (Maybe kIn) )
      -> ( kIn -> a -> kOut )
      -> NonEmpty a
      -> m (RefineTree kIn kOut a)
build fPar fIsEq fEval = refine' []
  where
  refine' :: [a] -> NonEmpty a -> m (RefineTree kIn kOut a)
  refine' eqAcc (x :| [])  = return $ RefineLeaf (x :| eqAcc)
  refine' eqAcc (x :| (y:zs)) =
    do
      res <- fIsEq x y
      case res of
        Nothing -> refine' (x:eqAcc) (y :| zs)
        Just kIn ->
          let xOut = fEval kIn x
              zsK  = partitionMap (fEval kIn) (y:zs)
          in
          case xOut `HashMap.lookup` zsK of
            Nothing ->
              do
                m <- mapPar fPar (refine' []) zsK
                return $ RefineNode kIn $ HashMap.insert xOut (RefineLeaf (x :| eqAcc)) m
            Just xClass ->
              do
                m <- mapPar fPar (refine' []) (HashMap.delete xOut zsK)
                xRes <- refine' eqAcc (x :| NE.toList xClass)
                return $ RefineNode kIn $ HashMap.insert xOut xRes m

-- | Searches the `RefineTree` for equal nodes.
--
-- Warning: The equality-check and evaluation must be identical to the ones used
--   during construction.
searchEq :: ( Monad m, Eq kOut, Hashable kOut )
         => ( a -> a -> m (Maybe kIn) )
            -- ^ When unequal, it shows this by a counter-example. Otherwise equal.
         -> ( kIn -> a -> kOut )
            -- ^ Evaluate the element for the property
         -> RefineTree kIn kOut a
            -- ^ The tree containing the elements
         -> a
            -- ^ The element whose equals should be found
         -> m [a]
            -- ^ The equal elements obtained from the tree
searchEq fIsEq _fEval (RefineLeaf (x :| xs)) a =
  do
    mCounterExample <- fIsEq a x
    if isJust mCounterExample then
      -- The elements that match on all properties, are - in fact - unequal
      return []
    else
      -- The elements /are/ equal
      return (x:xs)
searchEq fIsEq fEval (RefineNode kIn m) a =
  let kOut = fEval kIn a
  in
  case HashMap.lookup kOut m of
    Nothing   -> return []
    Just tree -> searchEq fIsEq fEval tree a

-- | Returns the height of the tree. This is the maximum number of /edges/ to
-- the leaves. (i.e., @RefineLeaf _@  has a height of 0)
height :: RefineTree kIn kOut a -> Int
height (RefineLeaf _)   = 0
height (RefineNode _ m) = 1 + foldr (max . height) 0 (HashMap.elems m)

-- | The number of elements stored within the tree.
size :: RefineTree kIn kOut a -> Int
size (RefineLeaf xs)  = length xs
size (RefineNode _ m) = sum $ map size $ HashMap.elems m

-- | The equivalence classes stored in the tree.
groups :: RefineTree kIn kOut a -> [NonEmpty a]
groups (RefineLeaf xs)  = [xs]
groups (RefineNode _ m) = concatMap groups $ HashMap.elems m


-- # Helpers #

-- | Map over the values in the hashmap in parallel
mapPar :: (Monad m, Eq k, Hashable k) => ( forall a . [m a] -> m [a] ) -> ( b -> m c ) -> HashMap k b -> m (HashMap k c)
mapPar fPar f = fmap HashMap.fromList . fPar . map (\(k,v) -> (k,) <$> f v) . HashMap.toList

-- | Partitions a list of elements based on a property
partitionMap :: (Eq k, Hashable k) => ( a -> k ) -> [a] -> HashMap k (NonEmpty a)
partitionMap f = foldr (\a m -> HashMap.alter (Just . maybe (a:|[]) (\ys -> a :| NE.toList ys)) (f a) m) HashMap.empty
