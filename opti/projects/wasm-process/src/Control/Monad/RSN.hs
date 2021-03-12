{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Read-State-Nondeterminism monad.
module Control.Monad.RSN
  ( RSN (..)
  , evalRSN
  , evalRSN1
    -- * Utilities
  , returnAll
  , discard
  , discardIf
  , discardNothing
  , choice
  ) where

import Melude
-- Stdlib imports
import Control.Monad.Identity ( Identity, runIdentity )
import Control.Monad.Reader ( MonadReader (..) )
import Control.Applicative ( Alternative (..) )
import Data.Foldable ( asum )
-- Extra stdlib imports
import Control.Monad.State ( MonadState (..) )

-- | 
--
-- Kind of like `RWST s [] a`, but with more convenient class instances. (At
-- the expense of a less composable monad stack)
newtype RSN r s a = RSN { runRSN :: r -> s -> [(a,s)] }

evalRSN1 :: RSN r s a -> r -> s -> Maybe a
evalRSN1 m r s =
  case evalRSN m r s of
    []    -> Nothing
    (x:_) -> Just x

evalRSN :: RSN r s a -> r -> s -> [a]
evalRSN = map fst .:. runRSN

returnAll :: (Monad m, Alternative m) => [a] -> m a
returnAll = foldr ((<|>) . return) empty

discard :: Alternative m => m a
discard = empty

discardIf :: (Monad m, Alternative m) => Bool -> m ()
discardIf True  = discard
discardIf False = return ()

discardNothing :: (Monad m, Alternative m) => Maybe a -> m a
discardNothing Nothing  = discard
discardNothing (Just a) = return a

choice :: Alternative m => [m a] -> m a
choice = asum

instance Functor (RSN r s) where
  -- fmap :: ( a -> b ) -> RSNT r s m a -> RSNT r s m b
  fmap f m = RSN (map (mapFst f) .: runRSN m)

instance Applicative (RSN r s) where
  pure a = RSN $ \_ s -> [(a,s)]

  -- (<*>) :: RSNT s m ( a -> b ) -> RSNT s m a -> RSNT s m b
  (<*>) mf ma =
    RSN $ \r -> concatMap (\(f, s') -> map (mapFst f) $ runRSN ma r s') . runRSN mf r
      -- concat <$> (traverse (\(f, s') -> map (mapFst f) <$> runRSNT ma r s') =<< runRSNT mf r s)

instance Monad (RSN r s) where
  return = pure

  -- (>>=) :: RSNT s m a -> ( a -> RSNT s m b ) -> RSNT s m b
  (>>=) m f =
    -- RSNT $ \r s -> concat <$> (traverse (\(a,s') -> runRSNT (f a) r s') =<< runRSNT m r s)
    RSN $ \r -> concatMap (\(a, s') -> runRSN (f a) r s') . runRSN m r

instance MonadState s (RSN r s) where
  state f = RSN $ \_ s -> [f s]

instance Alternative (RSN r s) where
  empty = RSN $ const $ const []

  (<|>) a b = RSN $ \r s -> runRSN a r s ++ runRSN b r s

instance MonadReader r (RSN r s) where
  ask = RSN $ \r s -> [(r,s)]

  local f m = RSN $ \r -> runRSN m (f r)
