{-# LANGUAGE Strict #-}

-- | Helpers for the computing confluence in dataflow
module Algorithm.Confluence
  ( list
  , listNE
  , idList
  ) where

import Melude
-- Stdlib imports
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty ((:|)) )
-- External library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )


-- |
list :: Eq a => ( a -> b -> Maybe c ) -> [a] -> [b] -> Maybe [c]
list _ [] []         = Just []
list f (x:xs) (y:ys) = (:) <$> f x y <*> list f xs ys
list _ _      _      = Nothing

-- |
listNE :: Eq a => ( a -> b -> Maybe c ) -> NonEmpty a -> NonEmpty b -> Maybe (NonEmpty c)
listNE f (x:|xs) (y:|ys) = (:|) <$> f x y <*> list f xs ys

-- |
idList :: Eq a => ( a -> a -> Maybe a ) -> IdList a -> IdList a -> Maybe (IdList a)
idList f xs ys =
  IdList.fromList <$> list f (IdList.toList xs) (IdList.toList ys)
