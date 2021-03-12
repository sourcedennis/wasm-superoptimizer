{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

-- | Non-deterministic State; like `State s [] a`, but with more convenient
-- class instances. (At the expense of a less composable monad stack)
module Control.Monad.StateN
  ( StateNT (..)
  , execStateNT
  , evalStateNT
  , runStateN
  , execStateN
  , evalStateN
    -- * Utilities
  , returnAll
  , discard
  , discardIf
  , discardNothing
  , local
  , mapAlternative
  ) where

import Melude
-- Stdlib imports
import Control.Monad.Identity ( Identity, runIdentity )
import Control.Applicative ( Alternative (..) )
-- Extra stdlib imports
import Control.Monad.State ( MonadState (..) )

-- | 
--
-- Kind of like `State s [] a`, but with more convenient class instances. (At
-- the expense of a less composable monad stack)
newtype StateNT s m a = StateNT { runStateNT :: s -> m [(a,s)] }

type StateN s = StateNT s Identity

execStateNT :: Monad m => StateNT s m a -> s -> m [s]
execStateNT = fmap (map snd) .: runStateNT

evalStateNT :: Monad m => StateNT s m a -> s -> m [a]
evalStateNT = fmap (map fst) .: runStateNT

runStateN :: StateN s a -> s -> [(a,s)]
runStateN = runIdentity .: runStateNT

execStateN :: StateN s a -> s -> [s]
execStateN = runIdentity .: execStateNT

evalStateN :: StateN s a -> s -> [a]
evalStateN = runIdentity .: evalStateNT

returnAll :: Monad m => [a] -> StateNT s m a
returnAll xs = StateNT $ \s -> return (map (,s) xs)

discard :: Monad m => StateNT s m a
discard = empty

discardIf :: Monad m => Bool -> StateNT s m ()
discardIf True  = discard
discardIf False = return ()

discardNothing :: Monad m => Maybe a -> StateNT s m a
discardNothing Nothing  = discard
discardNothing (Just a) = return a

local :: Monad m => ( s -> s2 ) -> StateNT s2 m a -> StateNT s m (a, s2)
local f m = StateNT $ \s -> map (,s) <$> runStateNT m (f s)

mapAlternative :: Alternative m => ( a -> m b ) -> [a] -> m b
mapAlternative f = foldr ((<|>) . f) empty

instance Monad m => Functor (StateNT s m) where
  -- fmap :: ( a -> b ) -> StateNT s m a -> StateNT s m b
  fmap f m = StateNT (fmap (map $ mapFst f) . runStateNT m)

instance Monad m => Applicative (StateNT s m) where
  pure a = StateNT $ \s -> return [(a,s)]

  -- (<*>) :: StateNT s m ( a -> b ) -> StateNT s m a -> StateNT s m b
  (<*>) mf ma =
    StateNT $ \s -> concat <$> (traverse (\(f, s') -> map (mapFst f) <$> runStateNT ma s') =<< runStateNT mf s)

instance Monad m => Monad (StateNT s m) where
  return = pure

  -- (>>=) :: StateNT s m a -> ( a -> StateNT s m b ) -> StateNT s m b
  (>>=) m f =
    StateNT $ \s -> concat <$> (traverse (\(a,s') -> runStateNT (f a) s') =<< runStateNT m s)

instance Monad m => MonadState s (StateNT s m) where
  state f = StateNT $ \s -> return [f s]

instance Monad m => Alternative (StateNT s m) where
  empty = StateNT $ const $ return []

  (<|>) a b = StateNT $ \s -> (++) <$> runStateNT a s <*> runStateNT b s
