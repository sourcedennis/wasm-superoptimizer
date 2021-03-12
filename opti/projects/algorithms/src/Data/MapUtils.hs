
module Data.MapUtils
  ( intMapLookupFunc
  ) where

-- Stdlib imports
import           Data.Maybe ( fromMaybe )
-- Extra stdlib imports
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )


-- | Converts an `IntMap` into a vector. Empty intermediate gaps are filled with
-- the given default value.
--
-- This relies upon the fact that graph nodes are picked very densly in the
-- lower indices, meaning very few intermediate values will occur.
--
-- >>> intMapVec 0 (IntMap.fromList [(3,7),(5,42)])
-- [0,0,0,7,0,42]
intMapVec :: a -> IntMap a -> Vector a
intMapVec d = Vector.fromList . fillGaps d 0 . IntMap.toAscList
  where
  fillGaps :: a -> Int -> [(Int,a)] -> [a]
  fillGaps d x ((i,a):xs)
    | x < i      = d : fillGaps d (x+1) ((i,a):xs)
    | x == i     = a : fillGaps d (x+1) xs
    | otherwise  = fillGaps d x xs
  fillGaps _ _ [] = []

-- | Constructs a very fast lookup function for an `IntMap`. The construction
-- takes /O(n)/ but lookup is /O(1)/.
--
-- >>> map (intMapLookupFunc 99 (IntMap.fromList [(0,3),(2,4),(4,8)])) [0..5]
-- [3,99,4,99,8,99]
intMapLookupFunc :: a -> IntMap a -> ( Int -> a )
intMapLookupFunc d m =
  let v = intMapVec d m
  in \i -> fromMaybe d ( v !? i )
