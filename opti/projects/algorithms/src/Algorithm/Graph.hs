{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

-- TODO: Cleanup
module Algorithm.Graph
  ( traverseGraph
  , reachables
  , loopEntries
  , multiParentNodes
  -- , invert
  , invertFlow
  , removeIndirection
  , groupFinite
  , dominators
  , quasiTopologicalOrder
  ) where

import Melude
-- Stdlib imports
import           Data.Bifunctor ( bimap )
-- Extra stdlib imports
import qualified Data.Map as Map
import           Data.Map ( Map )
import qualified Data.Set as Set
import           Data.Set ( Set )
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
import           Data.Foldable ( foldrM )
import           Control.Monad ( when, unless, void )
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import qualified Control.Monad.RWS as RWS
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
-- External library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
-- Local imports
import           Data.MapUtils ( intMapLookupFunc )


type PathVisited = IntSet
type Visited = IntSet
type MultiParentNodes = IntSet
type LoopEntries = IntSet

-- | Depth-first traversal of the graph
traverseGraph :: forall m
              .  Monad m
              => ( Int -> m IntSet )
              -> IntSet
              -> m IntSet
traverseGraph fVisit = foldrM visit IntSet.empty . IntSet.toList
  where
  visit :: Int -> IntSet -> m IntSet
  visit nodeI visited =
    if nodeI `IntSet.member` visited then
      return visited
    else
      do
        nextNodes <- fVisit nodeI
        let visited' = IntSet.insert nodeI visited
        foldrM visit visited' $ IntSet.toList nextNodes

-- | Returns the set of nodes reachable from the set of roots
reachables :: ( Int -> IntSet ) -> IntSet -> IntSet
reachables fNext = runIdentity . traverseGraph (return . fNext)

-- | /O(n)/.
--
-- WARNING: This output is /only correct/ iff the input graph is /reducible/
--   (which means it represents /structured control flow/).
loopEntries :: ( Int -> IntSet ) -> Int -> LoopEntries
loopEntries fNext rootI =
  snd $ execState (visit IntSet.empty rootI) (IntSet.empty, IntSet.empty)
  where
  visit :: PathVisited -> Int -> State (Visited, LoopEntries) ()
  visit path i =
    if i `IntSet.member` path then -- It's a back edge. Loop entry found
      S.modify (bimap (IntSet.insert i) (IntSet.insert i))
    else
      do
        isVisited <- S.gets (IntSet.member i . fst)
        unless isVisited
          $ do
              S.modify $ mapFst $ IntSet.insert i -- mark visited
              mapM_ (visit (IntSet.insert i path)) (IntSet.toList $ fNext i)

-- | Returns the nodes that have multiple direct parents (which are reachable
-- from the root)
multiParentNodes :: ( Int -> IntSet ) -> Int -> MultiParentNodes
multiParentNodes fNext rootI =
  snd $ execState (visit rootI) (IntSet.empty, IntSet.empty)
  where
  visit :: Int -> State (Visited, MultiParentNodes) ()
  visit i =
    do
      isVisited <- S.gets (IntSet.member i . fst)
      if isVisited then -- It's multi visited!
        S.modify $ mapSnd $ IntSet.insert i
      else
        do
          S.modify $ mapFst $ IntSet.insert i -- mark visited
          mapM_ visit (IntSet.toList $ fNext i)

type EncounteredDominators = IntSet

-- | For every node, find the nodes dominating it (out of `relevantDominatorsI`).
dominators :: ( node -> IntSet ) -> IntMap node -> IntSet -> Int -> IntMap IntSet
dominators f graph relevantDominatorsI rootI = fst $ RWS.execRWS (moveDown rootI) IntSet.empty IntMap.empty
  where
  moveDown :: Int -> RWS EncounteredDominators () (IntMap IntSet) ()
  moveDown nodeI =
    do
      let node = fromMaybe (error "Invalid graph") $ IntMap.lookup nodeI graph
      currNodeDominators <- S.gets $ IntMap.lookup nodeI
      encounteredDominators <- R.asks $ applyIf (nodeI `IntSet.member` relevantDominatorsI) (IntSet.insert nodeI)
      let newNodeDominators = maybe encounteredDominators (IntSet.intersection encounteredDominators) currNodeDominators
      S.modify $ IntMap.insert nodeI newNodeDominators
      unless
        (currNodeDominators == Just newNodeDominators)
        $ mapM_ (R.local (const encounteredDominators) . moveDown) (IntSet.toList $ f node)

-- | Inverts flow and returns a /O(1)/ lookup function.
invertFlow :: forall edge . ( Int -> [ ( edge, Int ) ] ) -> Int -> ( Int -> [ ( edge, Int ) ] )
invertFlow fEdges rootI = intMapLookupFunc [] $ execState (traverseGraph visit $ IntSet.singleton rootI) mempty
  where
  visit :: Int -> State (IntMap [(edge,Int)]) IntSet
  visit i =
    do
      let next = fEdges i
      S.modify $ \m -> foldr (\(e, j) -> IntMap.alter (Just . ((e,i):) . fromMaybe []) j) m next
      return $ IntSet.fromList $ map snd next

removeIndirection :: forall a b
                  .  ( forall m . Monad m => ( Int -> m Int ) -> a -> m b )
                  -> ( Int -> Either Int a )
                  -> b
                  -> Int
                  -> (Int, IdList b)
removeIndirection fUpdate fNode nodeInf rootI =
  mapSnd snd $ S.runState (rebuildNode IntSet.empty rootI) (IntMap.empty, IdList.empty)
  where
  rebuildNode :: IntSet -> Int -> State (IntMap Int, IdList b) Int
  rebuildNode cycleSet i =
    cacheOver (_1 . lIntMapIdx i)
      $ do
          mCachedVal <- S.gets (IntMap.lookup i . fst)
          case mCachedVal of
            Just i -> return i
            Nothing ->
              if i `IntSet.member` cycleSet then
                storeMapping i =<< newNode nodeInf
              else
                case fNode i of
                  Left j ->
                    -- Another indirection
                    storeMapping i =<< rebuildNode (IntSet.insert i cycleSet) j
                  Right n ->
                    do
                      j <- newNode nodeInf -- (error "Placeholder Node Not Eliminated")
                      storeMapping i j
                      nodeContent <- fUpdate (rebuildNode IntSet.empty) n
                      replaceNode j nodeContent
                      return j

  storeMapping :: Int -> Int -> State (IntMap Int, s) Int
  storeMapping oldI newI =
    S.modify (mapFst $ IntMap.insert oldI newI) >> return newI

  newNode :: b -> State (s, IdList b) Int
  newNode n = S.state $ mapState snd (mapSnd . const) (IdList.append n)
  
  replaceNode :: Int -> b -> State (s, IdList b) ()
  replaceNode i = S.modify . mapSnd . IdList.replace i

-- | Groups a list of elements with a classifier
groupFinite :: forall a b . Ord b => ( a -> b ) -> [a] -> [[a]]
groupFinite fClassify = Map.elems . foldr addToList Map.empty
  where
  addToList :: a -> Map b [a] -> Map b [a]
  addToList x = Map.alter (Just . (x:) . fromMaybe []) (fClassify x)


-- # Helpers

-- fastGraph :: a -> IntMap a -> ( Int -> a )
-- fastGraph =

-- | Inserts a single value into a map where each key maps to a set of values.
insertSetMap :: Int -> Int -> IntMap IntSet -> IntMap IntSet
insertSetMap k v = IntMap.alter (Just . IntSet.insert v . fromMaybe IntSet.empty) k

stages :: Monad m => [m (Maybe a)] -> m a -> m a
stages (x:xs) s =
  -- Note: This is not the same as `fromMaybe <$> stages xs s <*> x`
  do
    x' <- x
    case x' of
      Just res -> return res
      Nothing  -> stages xs s
stages [] s = s

-- Maps a state to another state: (s1 -> (a,s1)) -> (s2 -> (a,s2))
mapState :: (s2 -> s1) -> (s1 -> s2 -> s2) -> (s1 -> (a,s1)) -> (s2 -> (a,s2))
mapState f g h s2 = mapSnd (`g` s2) $ h $ f s2

-- |
--
-- Reverse post-order (depth-first) traversal of the graph.
quasiTopologicalOrder :: ( Int -> IntSet ) -> IntSet -> [Int]
quasiTopologicalOrder fNext roots =
  evalState (foldM visit [] (IntSet.toList roots)) IntSet.empty
  where
  visit :: [Int] -> Int -> State Visited [Int]
  visit acc i =
    do
      isVisited <- S.gets $ IntSet.member i
      if isVisited then
        return acc
      else
        do
          S.modify $ IntSet.insert i -- mark visited
          (i:) <$> foldM visit acc (IntSet.toList $ fNext i)
