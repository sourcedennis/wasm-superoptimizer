{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.Dataflow
  ( mapFlow
  , fix
  , fixOrder
  , fixTopological
  ) where

import Melude ( mapFst )
-- Stdlib imports
import           Data.Maybe ( fromMaybe, isNothing )
import           Control.Monad ( when, filterM, forM_ )
-- Extra stdlib imports
import qualified Control.Monad.State.Strict as S
import           Control.Monad.State.Strict ( State, execState )
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
-- Local imports
import qualified Data.OrderWorkList as OWL
import           Data.OrderWorkList ( OrderWorkList )



type NodeId = Int

mapFlow :: ( a -> b ) -> ( NodeId -> [ ( a, NodeId ) ] ) -> ( NodeId -> [ ( b, NodeId ) ] )
mapFlow f g i = map (mapFst f) $ g i

-- | Find the /least fixed-point/ for a dataflow equation.
-- 
-- Nodes are initialised to their initial value, where their value ascends
-- in the (dataflow-specific) lattice.
--
-- Traversal of nodes is /not/ ordered, which makes this algorithm
-- /significantly/ slower than `fixOrder`. However, when the number of
-- iterations for a single node is expected to be low (e.g. with type-checking),
-- use this. (As it has no ordering overhead).
fix :: forall a
    .  Eq a
    => ( a -> a -> a )                      -- ^ Confluence operator
    -> ( NodeId -> a )                      -- ^ Initial node values
    -> ( NodeId -> [ ( a -> a, NodeId ) ] ) -- ^ Outgoing node edges
    -> IntSet                               -- ^ Iteration roots
    -> IntMap a                             -- ^ Least fixed point per node
fix fConfluence fInit fNext roots =
  execState (mapM execute $ IntSet.toList roots) (intMap fInit roots)
  where
  insert :: a -> NodeId -> State (IntMap a) Bool
  insert = insertIfUpdate fInit fConfluence
  execute :: NodeId -> State (IntMap a) ()
  execute i =
    do
      val <- S.gets (fromMaybe (fInit i) . IntMap.lookup i)
      updatedNodes <- map snd <$> filterM (\(f, j) -> insert (f val) j) (fNext i)
      forM_ updatedNodes execute

-- | Find the /least fixed-point/ for a dataflow equation.
-- 
-- Nodes are initialised to their initial value, where their value ascends
-- in the (dataflow-specific) lattice.
--
-- Traversal of nodes is /ordered/, which means: Out of all nodes that are
-- queued for visiting, those with higher priority are visited first. The
-- priority is determined by element index in the input list.
--
-- This algorithm is /significantly/ faster than `fix` for dataflow equations
-- where repeated node visits are expected. (If not, `fix` is preferred, as it
-- has less overhead).
fixOrder :: forall a
         .  Eq a
         => ( a -> a -> a )                      -- ^ Confluence operator
         -> ( NodeId -> a )                      -- ^ Initial node values
         -> ( NodeId -> [ ( a -> a, NodeId ) ] ) -- ^ Outgoing node edges
         -> IntSet                               -- ^ Iteration roots
         -> [NodeId]                             -- ^ Node priority order
         -> IntMap a                             -- ^ Least fixed point per node
fixOrder fConfluence fInit fNext roots order =
  let wl = foldr OWL.insert (OWL.empty order) $ IntSet.toList roots
  in execState (execute wl) (intMap fInit roots)
  where
  insert :: a -> NodeId -> State (IntMap a) Bool
  insert = insertIfUpdate fInit fConfluence
  execute :: OrderWorkList -> State (IntMap a) ()
  execute wl =
    case OWL.pop wl of
      Nothing -> return ()
      Just (i, wl') ->
        do
          val <- S.gets (fromMaybe (fInit i) . IntMap.lookup i)
          updatedNodes <- map snd <$> filterM (\(f, j) -> insert (f val) j) (fNext i)
          execute $ foldr OWL.insert wl' updatedNodes

-- | /O(n)/. Single forward pass of dataflow analysis.
fixTopological :: forall a
               .  ( NodeId -> a -> a -> a )            -- ^ Confluence operator (node specific)
               -> ( NodeId -> a )                      -- ^ Initial node values
               -> ( NodeId -> [ ( a -> a, NodeId ) ] ) -- ^ Outgoing node edges
               -> [NodeId]                             -- ^ Node priority order
               -> IntMap a                             -- ^
fixTopological fConfluence fInit fNext order =
  visit order IntMap.empty
  where
  visit :: [NodeId] -> IntMap a -> IntMap a
  visit [] m = m
  visit (x:xs) m =
    let currVal = fromMaybe (fInit x) $ IntMap.lookup x m
    in visit xs $ foldr (applyTransfer currVal) m (fNext x)
  applyTransfer :: a -> ( a -> a, NodeId ) -> IntMap a -> IntMap a
  applyTransfer a (f, i) m =
    let currVal = fromMaybe (fInit i) $ IntMap.lookup i m
        newVal  = fConfluence i currVal (f a)
    in IntMap.insert i newVal m

-- | Confluences the value with the node's current value, and saves it.
-- Returns `True` if this updates the value.
insertIfUpdate :: Eq a => ( NodeId -> a ) -> ( a -> a -> a ) -> a -> NodeId -> State (IntMap a) Bool
insertIfUpdate fInit fConfluence a i =
  do
    mCurrVal <- S.gets $ IntMap.lookup i
    let currVal   = fromMaybe (fInit i) mCurrVal
        newVal    = fConfluence currVal a
        isUpdated = isNothing mCurrVal || currVal /= newVal
    when isUpdated (S.modify $ IntMap.insert i newVal)
    return isUpdated

intMap :: ( Int -> a ) -> IntSet -> IntMap a
intMap f = IntMap.fromList . map (\i -> (i, f i)) . IntSet.toList
