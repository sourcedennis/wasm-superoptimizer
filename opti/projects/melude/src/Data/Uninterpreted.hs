{-# LANGUAGE RankNTypes, ScopedTypeVariables, ConstraintKinds #-}

-- | Uninterpreted bitvector integer.
--
-- These integers are stored as bitvectors, whose signedness is determined by
-- the operators.
module Data.Uninterpreted
  ( -- * Types
    Uninterpreted (..)
  , IUninterpreted
  , Bitwidth
    -- * Functions
  , toSigned
  , toSignedBw
  , fromSigned
  , fromSignedBw
  , toUnsigned
  , fromUnsigned
  , maxVal
  , maxValS
  , minValS
    -- * Operators
  , unopS
  , unopU
  , unopSM
  , unopUM
  , binopS
  , binopU
  , binopSM
  , binopUM
  , hasMSB
  ) where

-- Stdlib imports
import Numeric.Natural ( Natural )
import Data.Proxy ( Proxy (..) )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Bits ( Bits )


-- # Types

type Bitwidth = Natural

-- | Uninterpreted bitvector of fixed length.
class Bits u => Uninterpreted u where
  bitwidth :: Proxy u -> Bitwidth

type IUninterpreted i = (Uninterpreted i, Integral i)

-- # Functions #

toSigned :: forall i u . (IUninterpreted u, Integral i) => u -> i
toSigned = toSignedBw $ bitwidth (Proxy :: Proxy u)

toSignedBw :: (Integral u, Integral i) => Bitwidth -> u -> i
toSignedBw bw a
  | hasMSBBw bw a  = fromIntegral (toInteger a - toInteger (2^bw))
  | otherwise      = fromIntegral a

toUnsigned :: forall i u . (IUninterpreted u, Integral i) => u -> i
toUnsigned = fromIntegral

fromSigned :: forall i u . (IUninterpreted u, Integral i) => i -> Maybe u
fromSigned = fromSignedBw $ bitwidth (Proxy :: Proxy u)

fromSignedBw :: forall i u . (Integral u, Integral i) => Bitwidth -> i -> Maybe u
fromSignedBw bw a
  | a < minValBwS bw || a > maxValBwS bw  =
      Nothing
  | a < 0      = Just $ fromIntegral ( 2 ^ bw + toInteger a )
  | otherwise  = Just $ fromIntegral a 

fromUnsigned  :: forall i u . (IUninterpreted u, Integral i) => i -> Maybe u
fromUnsigned a
  | a < 0 || a > maxVal p  = Nothing
  | otherwise              = Just $ fromIntegral a
  where p = Proxy :: Proxy u

-- | Unsigned max value for bitwidth.
--
-- >>> maxVal (Proxy :: Proxy Word8)
-- 255
maxVal :: (Uninterpreted u, Num i) => Proxy u -> i
maxVal u = maxValBw (bitwidth u)

-- | Signed maximum value for bitwidth.
--
-- >>> maxValS (Proxy :: Proxy Word8)
-- 127
maxValS :: (Uninterpreted u, Num i) => Proxy u -> i
maxValS u = maxValBw (bitwidth u - 1)

-- | Signed minimum value for bitwidth.
--
-- >>> minValS (Proxy :: Proxy Word8)
-- -128
minValS :: (Uninterpreted u, Num i) => Proxy u -> i
minValS u = -maxValBw (bitwidth u - 1) - 1


-- ## Operators ##

unopS :: (Integral i, IUninterpreted u)
      => ( i -> a ) -> u -> a
unopS f = f . toSigned

unopU :: (Integral i, IUninterpreted u)
      => ( i -> a ) -> u -> a
unopU f = f . toUnsigned

unopSM :: (Integral i, IUninterpreted u, Functor f)
       => ( i -> f a ) -> u -> f a
unopSM f = f . toSigned

unopUM :: (Integral i, IUninterpreted u, Functor f)
       => ( i -> f a ) -> u -> f a
unopUM f = f . toUnsigned

binopS :: (Integral i, IUninterpreted u)
       => ( i -> i -> a ) -> u -> u -> a
binopS f = unopS . f . toSigned

binopU :: (Integral i, IUninterpreted u)
       => ( i -> i -> a ) -> u -> u -> a
binopU f = unopU . f . toUnsigned

binopSM :: (Integral i, IUninterpreted u, Functor f)
        => ( i -> i -> f a ) -> u -> u -> f a
binopSM f = unopSM . f . toSigned

binopUM :: (Integral i, IUninterpreted u, Functor f)
        => ( i -> i -> f a ) -> u -> u -> f a
binopUM f = unopUM . f . toUnsigned

hasMSB :: forall u . IUninterpreted u => u -> Bool
hasMSB a = a > maxValS (Proxy :: Proxy u)

hasMSBBw :: forall u . Integral u => Bitwidth -> u -> Bool
hasMSBBw bw a = a > maxValBwS bw


-- # Internal Helpers #


-- | Unsigned max value for bitwidth.
--
-- >>> maxValBw 8
-- 255
maxValBw :: Num i => Bitwidth -> i
maxValBw bw = 2^bw - 1

maxValBwS :: Num i => Bitwidth -> i
maxValBwS bw = maxValBw (bw - 1)

minValBwS :: Num i => Bitwidth -> i
minValBwS bw = -maxValBw (bw - 1) - 1


-- # Instances

instance Uninterpreted Word8 where
  bitwidth Proxy = 8

instance Uninterpreted Word16 where
  bitwidth Proxy = 16
  
instance Uninterpreted Word32 where
  bitwidth Proxy = 32
  
instance Uninterpreted Word64 where
  bitwidth Proxy = 64
