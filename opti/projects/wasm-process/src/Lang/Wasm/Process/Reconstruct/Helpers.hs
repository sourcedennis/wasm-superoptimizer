{-# LANGUAGE FlexibleContexts #-}

module Lang.Wasm.Process.Reconstruct.Helpers
  ( -- * Type aliases
    Builder
  , Visited
  , AvaiLabels
  , NeededLabels
  , OutLabels
    -- * Functions
  -- , selectNaturalTarget
  -- , isVisited
  -- , isNeeded
  -- , isAvailabel
  -- , validateJumpTarget
  -- , maybeEq
  -- , wrapLoop
  -- , wrapBlock
  ) where

-- Stdlib imports
import           Control.Applicative ( Alternative ((<|>)) )
-- Extra stdlib imports
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
import qualified Control.Monad.State as S
import           Control.Monad.State ( MonadState )
import qualified Control.Monad.Reader as R
import           Control.Monad.Reader ( MonadReader )
-- Local imports
import qualified Control.Monad.RSN as RSN
import           Control.Monad.RSN ( RSN )
import           Lang.Wasm.Process.Structures
-- import           Lang.Wasm.Process.Reconstruct.Instr
--   ( Instr (..), fixLabel )

-- | Type abbreviation
type Builder = RSN (NeededLabels,AvaiLabels) Visited


type Visited = IntSet
-- | The subset of Visited, with labels that are available for jumping
type AvaiLabels = IntSet -- elements = SingleId
-- | The node-ids that preceding instructions require within their scope as labels
type NeededLabels = IntSet
-- | The labels that it jumps to; disjoint with Visited.
type OutLabels = IntSet -- elements = SingleId


-- -- | Selects any of the output labels as a natural subsequent node; provided
-- -- that the natural target is /not/ needed externally. (If an external scope
-- -- requires the target, it cannot be used as a subsequent node in this inner
-- -- scope; as scopes only branch out)
-- --
-- -- natural target =
-- --   instruction which immediately follows an instruction (without jumping)
-- selectNaturalTarget :: OutLabels -> Builder (Maybe SingleId)
-- selectNaturalTarget xs =
--   -- trace ("nat " ++ show xs) $
--   do
--     x <- RSN.returnAll (Nothing : map Just (IntSet.toList xs))
--     -- If the preceding scope needs it, it cannot be a natural following
--     -- instruction in this scope.
--     RSN.discardIf =<< maybe (return False) isNeeded x
--     -- Cannot visit it twice
--     RSN.discardIf =<< maybe (return False) isVisited x
--     return x

-- -- | Returns `True` iff the node was visited before.
-- isVisited :: MonadState Visited m => SingleId -> m Bool
-- isVisited = S.gets . IntSet.member

-- -- | Returns `True` iff the surrounding scope needs the node as a jumping
-- -- target. (This means a block has to start higher in the AST)
-- isNeeded :: MonadReader (NeededLabels,AvaiLabels) m => SingleId -> m Bool
-- isNeeded i = R.asks (IntSet.member i . fst)

-- -- | Returns `True` iff the node is known to be available as a label in the
-- -- surrounding scope (e.g., as a loop entry).
-- isAvailabel :: MonadReader (NeededLabels,AvaiLabels) m => SingleId -> m Bool
-- isAvailabel i = R.asks (IntSet.member i . snd)

-- -- | Checks whether the jump is possible. WebAssembly programs can only jump
-- -- to the outer scope (e.g., loop entry). Any node that was previously visited
-- -- must thus be within this outer scope. If a node was not yet visited, it may
-- -- still occur later.
-- validateJumpTarget :: SingleId -> Builder ()
-- validateJumpTarget i =
--   do
--     isOk <- (||) <$> (not <$> isVisited i) <*> isAvailabel i
--     RSN.discardIf (not isOk)

-- -- | Returns the unification over the two inputs. If they cannot be unified
-- -- (over equality), the computation path is /discarded/.
-- maybeEq :: (Alternative m, Eq a) => Maybe a -> Maybe a -> m (Maybe a)
-- maybeEq (Just a) (Just b)
--   | a == b     = pure $ Just a
--   | otherwise  = RSN.discard
-- maybeEq (Just a) Nothing  = pure $ Just a
-- maybeEq Nothing  (Just b) = pure $ Just b
-- maybeEq Nothing  Nothing  = pure Nothing

-- -- | Wraps the sequence of instructions in a loop construct (whose root is the
-- -- given node id), /if this is necessary/.
-- --
-- -- The `OutLabels` represents the set of nodes jumped to by the instructions.
-- wrapLoop :: SingleId -> ([Instr], OutLabels) -> ([Instr], OutLabels)
-- wrapLoop startI (xs,out) =
--   -- Whether `i` /is/ a loop was already determined.
--   if IntSet.member startI out then -- It needs the loop
--     ([InstrLoop undefined $ map (fixLabel startI) xs], IntSet.delete startI out)
--   else
--     (xs, out)
    
-- -- | Wraps the sequence of instructions in a block construct (whose end is the
-- -- given node id), /if this is necessary/.
-- --
-- -- The `OutLabels` represents the set of nodes jumped to by the instructions.
-- wrapBlock :: SingleId -> ([Instr], OutLabels) -> ([Instr], OutLabels)
-- wrapBlock endI (xs,out) =
--   if IntSet.member endI out then
--     ([InstrBlock undefined $ map (fixLabel endI) xs], IntSet.delete endI out)
--   else
--     (xs, out)
