
module Algorithm.Partition where

-- Stdlib imports
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty ((:|)) )
-- Extra stdlib imports
import           Data.Hashable ( Hashable )
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict ( HashMap )


-- | Partitions a list of elements on a property. Stores the result in a
-- `HashMap`. Requires the property to satisfy the `Hashable` class.
partitionHashMap :: ( Eq k, Hashable k ) => ( a -> k ) -> [a] -> HashMap k (NonEmpty a)
partitionHashMap f = foldr (\a m -> HashMap.alter (Just . maybe (a:|[]) (\ys -> a :| NE.toList ys)) (f a) m) HashMap.empty

-- | Partitions a list of elements on a property. Requires the property to
-- satisfy the `Hashable` class.
partitionHash :: ( Eq k, Hashable k ) => ( a -> k ) -> [a] -> [NonEmpty a]
partitionHash f = HashMap.elems . partitionHashMap f
