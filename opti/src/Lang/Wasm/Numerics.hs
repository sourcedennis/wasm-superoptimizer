{-# LANGUAGE ScopedTypeVariables #-}

-- | Concrete execution for WebAssembly integer operations
--
-- The operations are implemented as defined in:
-- https://webassembly.github.io/spec/core/exec/numerics.html
module Lang.Wasm.Numerics
  ( i32add, i32sub, i32mul, i32div, i32rem, i32and, i32or, i32xor, i32shl
  , i32shr, i32rotl, i32rotr, i32clz, i32ctz, i32popcnt, i32eqz, i32eq
  , i32ne, i32lt, i32gt, i32le, i32ge

  , i64add, i64sub, i64mul, i64div, i64rem, i64and, i64or, i64xor, i64shl
  , i64shr, i64rotl, i64rotr, i64clz, i64ctz, i64popcnt, i64eqz, i64eq
  , i64ne, i64lt, i64gt, i64le, i64ge
  
  , i64extendI32
  , i32extend8s, i32extend16s
  , i64extend8s, i64extend16s, i64extend32s
  ) where

import Melude
import Numeric.Natural ( Natural )
import Data.Word ( Word32, Word64 )
import Data.Bits ( Bits ( shiftR, shiftL, xor, (.&.), (.|.) ) )
import Data.Proxy ( Proxy (..) )
import Lang.Wasm.Ast ( Sx (..), WI32 (..), WI64 (..), IBinop (..) )
-- Local library imports
import qualified Data.Uninterpreted as U
import           Data.Uninterpreted ( Bitwidth, Uninterpreted, IUninterpreted )

i32add, i32sub, i32mul, i32and, i32or, i32xor, i32shl, i32rotl, i32rotr
  :: WI32 -> WI32 -> WI32
i32add  (WI32 a) (WI32 b) = WI32 $ iadd  a b
i32sub  (WI32 a) (WI32 b) = WI32 $ isub  a b
i32mul  (WI32 a) (WI32 b) = WI32 $ imul  a b
i32and  (WI32 a) (WI32 b) = WI32 $ iand  a b
i32or   (WI32 a) (WI32 b) = WI32 $ ior   a b
i32xor  (WI32 a) (WI32 b) = WI32 $ ixor  a b
i32shl  (WI32 a) (WI32 b) = WI32 $ ishl  a b
i32rotl (WI32 a) (WI32 b) = WI32 $ irotl a b
i32rotr (WI32 a) (WI32 b) = WI32 $ irotr a b

i32div, i32rem :: Sx -> WI32 -> WI32 -> Maybe WI32
i32div sx (WI32 a) (WI32 b) = WI32 <$> idiv sx a b
i32rem sx (WI32 a) (WI32 b) = WI32 <$> irem sx a b

i32shr :: Sx -> WI32 -> WI32 -> WI32
i32shr sx (WI32 a) (WI32 b) = WI32 $ ishr sx a b

i32clz, i32ctz, i32popcnt :: WI32 -> WI32
i32clz    (WI32 a) = WI32 $ iclz a
i32ctz    (WI32 a) = WI32 $ ictz a
i32popcnt (WI32 a) = WI32 $ ipopcnt a

i32eqz :: WI32 -> Bool
i32eqz (WI32 a) = ieqz a

i32eq, i32ne :: WI32 -> WI32 -> Bool
i32eq (WI32 a) (WI32 b) = ieq a b
i32ne (WI32 a) (WI32 b) = ine a b

i32lt, i32gt, i32le, i32ge :: Sx -> WI32 -> WI32 -> Bool
i32lt sx (WI32 a) (WI32 b) = ilt sx a b
i32gt sx (WI32 a) (WI32 b) = igt sx a b
i32le sx (WI32 a) (WI32 b) = ile sx a b
i32ge sx (WI32 a) (WI32 b) = ige sx a b

i64add, i64sub, i64mul, i64and, i64or, i64xor, i64shl, i64rotl, i64rotr
  :: WI64 -> WI64 -> WI64
i64add  (WI64 a) (WI64 b) = WI64 $ iadd  a b
i64sub  (WI64 a) (WI64 b) = WI64 $ isub  a b
i64mul  (WI64 a) (WI64 b) = WI64 $ imul  a b
i64and  (WI64 a) (WI64 b) = WI64 $ iand  a b
i64or   (WI64 a) (WI64 b) = WI64 $ ior   a b
i64xor  (WI64 a) (WI64 b) = WI64 $ ixor  a b
i64shl  (WI64 a) (WI64 b) = WI64 $ ishl  a b
i64rotl (WI64 a) (WI64 b) = WI64 $ irotl a b
i64rotr (WI64 a) (WI64 b) = WI64 $ irotr a b

i64div, i64rem :: Sx -> WI64 -> WI64 -> Maybe WI64
i64div sx (WI64 a) (WI64 b) = WI64 <$> idiv sx a b
i64rem sx (WI64 a) (WI64 b) = WI64 <$> irem sx a b

i64shr :: Sx -> WI64 -> WI64 -> WI64
i64shr sx (WI64 a) (WI64 b) = WI64 $ ishr sx a b

i64clz, i64ctz, i64popcnt :: WI64 -> WI64
i64clz (WI64 a)    = WI64 $ iclz a
i64ctz (WI64 a)    = WI64 $ ictz a
i64popcnt (WI64 a) = WI64 $ ipopcnt a

i64eqz :: WI64 -> Bool
i64eqz (WI64 a) = ieqz a

i64eq, i64ne :: WI64 -> WI64 -> Bool
i64eq (WI64 a) (WI64 b) = ieq a b
i64ne (WI64 a) (WI64 b) = ine a b

i64lt, i64gt, i64le, i64ge :: Sx -> WI64 -> WI64 -> Bool
i64lt sx (WI64 a) (WI64 b) = ilt sx a b
i64gt sx (WI64 a) (WI64 b) = igt sx a b
i64le sx (WI64 a) (WI64 b) = ile sx a b
i64ge sx (WI64 a) (WI64 b) = ige sx a b

-- |
--
-- >>> i64extendI32s S (WI32 0xEF5F49D1)
-- WI64 0xFFFFFFFFEF5F49D1
i64extendI32 :: Sx -> WI32 -> WI64
i64extendI32 S (WI32 a) = WI64 $ extends a
i64extendI32 U (WI32 a) = WI64 $ toNum a

i32extend8s, i32extend16s :: WI32 -> WI32
i32extend8s  (WI32 a) = WI32 $ extendsBw 8  32 a
i32extend16s (WI32 a) = WI32 $ extendsBw 16 32 a

i64extend8s, i64extend16s, i64extend32s :: WI64 -> WI64
i64extend8s  (WI64 a) = WI64 $ extendsBw 8  64 a
i64extend16s (WI64 a) = WI64 $ extendsBw 16 64 a
i64extend32s (WI64 a) = WI64 $ extendsBw 32 64 a

iadd, isub, imul, iand, ior, ixor, ishl, irotl, irotr :: forall u . IUninterpreted u => u -> u -> u
iadd = U.binopU (+)
isub = U.binopU (-)
imul = U.binopU (*)
iand = (.&.)
ior  = (.|.)
ixor = xor
ishl a b =
  let bw = U.bitwidth (Proxy :: Proxy u)
      k  = toNum (b `mod` toNum bw) :: Int
  in shiftL a k
irotl a b =
  let p  = Proxy :: Proxy u
      bw = toNum (U.bitwidth p) :: Int
      k  = toNum (b `mod` toNum bw) :: Int
  in shiftL a k .|. shiftR a (bw - k)
irotr a b =
  let p  = Proxy :: Proxy u
      bw = toNum (U.bitwidth p) :: Int
      k  = toNum (b `mod` toNum bw) :: Int
  in shiftR a k .|. shiftL a (toNum (bw-k))

idiv, irem :: forall u . IUninterpreted u => Sx -> u -> u -> Maybe u
-- Undefined: x/0. Returns Nothing
idiv U = fmap fromIntegral .: U.binopUM divu'
  where
  divu' :: Natural -> Natural -> Maybe Natural
  divu' _ 0 = Nothing
  divu' a b = Just ( a `div` b )
-- Undefined: -2^(N-1)/(-1), and x/0. These return Nothing
idiv S = fmap fromIntegral .: U.binopUM (divs' (Proxy :: Proxy u))
  where
  divs' :: Proxy u -> Integer -> Integer -> Maybe Integer
  divs' p _ 0 = Nothing
  divs' p a b =
    if a == U.minValS p && b == -1 then
      -- -2^(N-1)/(-1) = 2^(N-1) is not representable
      -- e.g. 8-bit: -2^7 / -1 = -128/-1 = 128. While 127 is max representable.
      Nothing
    else
      Just ( a `div` b )

irem U = fmap fromIntegral .: U.binopUM remu'
  where
  remu' :: Natural -> Natural -> Maybe Natural
  remu' _ 0 = Nothing
  remu' a b = Just ( a - b * ( a `div` b ) )
irem S = fmap fromIntegral .: U.binopUM rems'
  where
  rems' :: Integer -> Integer -> Maybe Integer
  rems' a b = Just ( a - b * ( a `div` b ) )


ishr :: forall u . IUninterpreted u => Sx -> u -> u -> u
ishr U a b =
  let bw = U.bitwidth (Proxy :: Proxy u)
      k  = toNum (b `mod` toNum bw) :: Int
  in shiftR a k
ishr S a b
  | U.hasMSB a  = shiftR a (toNum b) .|. shiftL (U.maxVal p) (bw - k)
  | otherwise   = shiftR a (toNum b)
  where p  = Proxy :: Proxy u
        bw = toNum (U.bitwidth p) :: Int
        k  = toNum (b `mod` toNum bw) :: Int

-- | Count leading zeroes
iclz, ictz, ipopcnt :: forall u . IUninterpreted u => u -> u
iclz a =
  let p  = Proxy :: Proxy u
      bw = toNum (U.bitwidth p)
  in bw - countWhileNotZero a
  where
  countWhileNotZero :: u -> u
  countWhileNotZero 0 = 0
  countWhileNotZero a = 1 + countWhileNotZero ( a `div` 2 )
-- | Count trailing zeroes
ictz a
  | even a     = 1 + ictz ( a `div` 2 )
  | otherwise  = 0
-- | Population count. Count 1 bits
ipopcnt 0 = 0
ipopcnt a = ( a `mod` 2 ) + ipopcnt ( a `div` 2 )

ieqz :: IUninterpreted u => u -> Bool
ieqz 0 = True
ieqz _ = False

ieq, ine :: IUninterpreted u => u -> u -> Bool
ieq  = (==)
ine  = (/=)

ilt, igt, ile, ige :: IUninterpreted u => Sx -> u -> u -> Bool
ilt S = U.binopS (<)
ilt U = U.binopU (<)
igt S = U.binopS (>)
igt U = U.binopU (>)
ile S = U.binopS (<=)
ile U = U.binopU (<=)
ige S = U.binopS (>=)
ige U = U.binopU (>=)

-- | Signed extension of input value
extends :: (IUninterpreted a, IUninterpreted b) => a -> b
extends = fromIntegral . U.toSigned

-- | Signed extension of input value
--
-- >>> U.toSigned (toNum (extendsBw 8 32 (-10 `mod` 0x100)) :: Word32)
-- -10
extendsBw :: (Integral i, Integral j) => Bitwidth -> Bitwidth -> i -> j
extendsBw bwIn bwOut i =
  let iInteger = toInteger i `mod` (2^bwIn)
  in
  case U.fromSignedBw bwOut $ U.toSignedBw bwIn iInteger of
    Just j -> j
    Nothing -> error "extendsBw error"
