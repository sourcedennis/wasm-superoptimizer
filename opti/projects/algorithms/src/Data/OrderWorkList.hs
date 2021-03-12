
-- | Internal.
--
-- Used by `Algorithm.Dataflow`, to ensure nodes are visited in an appropriate
-- (post-order) order.
module Data.OrderWorkList
  ( OrderWorkList
  , empty
  , pop
  , insert
  ) where

-- Stdlib imports
import           Data.Maybe ( fromMaybe )
import           Data.List ( sortOn )
-- Extra stdlib imports
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
import           Control.Monad ( (>=>) )
import qualified Data.Vector.Unboxed as UVector
import           Data.Vector.Unboxed ( Vector, (!?) )
-- External library imports
import qualified Data.PQueue.Prio.Min as PQ
import           Data.PQueue.Prio.Min ( MinPQueue )


type Index = Int
type NodeId = Int

data OrderWorkList =
  OrderWorkList {
    queue     :: MinPQueue Index NodeId
  , elements  :: IntSet
  , fIndex    :: NodeId -> Index
  }


-- # Construction

-- | Constructs an ordered work list. This is effectively a priority queue where
-- elements with a lower index in the given order are visited first.
empty :: [Int] -> OrderWorkList
empty = OrderWorkList PQ.empty IntSet.empty . nodeIndex


-- # Query/Update

-- | Removes and returns the next (highest priority) element from the work list.
pop :: OrderWorkList -> Maybe (NodeId, OrderWorkList)
pop wl =
  do
    (x, q) <- PQ.minView (queue wl)
    return
      ( x
      , wl {
          queue     = q
        , elements  = IntSet.delete x (elements wl)
        }
      )

-- | Inserts an element into the work list.
insert :: NodeId -> OrderWorkList -> OrderWorkList
insert x wl =
  if x `IntSet.member` elements wl then
    wl
  else
    wl {
      queue     = PQ.insert (fIndex wl x) x (queue wl)
    , elements  = IntSet.insert x (elements wl)
    }


-- # Helpers

-- | Constructs a /O(1)/ index lookup function.
nodeIndex :: [NodeId] -> ( NodeId -> Index )
nodeIndex xs =
  let n = length xs
      indexList = fillGaps n 0 $ sortOn snd $ zip [0..] xs
      indexVec = UVector.fromList indexList
  in fromMaybe n . (!?) indexVec
  where
  -- |
  -- >>> fillGaps 0 0 [(4,2)]
  -- [0,0,4]
  fillGaps :: a -> Int -> [(a,Int)] -> [a]
  fillGaps d x ((a,i):xs)
    | x < i      = d : fillGaps d (x+1) ((a,i):xs)
    | x == i     = a : fillGaps d (x+1) xs
    | otherwise  = fillGaps d x xs
  fillGaps _ _ [] = []
