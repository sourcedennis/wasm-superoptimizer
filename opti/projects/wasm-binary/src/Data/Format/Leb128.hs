module Data.Format.Leb128 where

-- Stdlib imports
import Data.Word (Word8)
-- 
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P

leb128P :: Integer -> Parser Integer
leb128P n =
  do
    w <- toInteger <$> P.anyWord8
    if w < 0x80 && w < 2 ^ n then
      return w
    else if w >= 0x80 && n > 7 then -- More bytes
      (+) ( w - 0x80 ) <$> ((*) 0x80 <$> leb128P (n-7))
    else
      fail "Malformed LEB"

sleb128P :: Integer -> Parser Integer
sleb128P n =
  do
    w <- toInteger <$> P.anyWord8
    if w < 0x40 && w < 2 ^ ( n - 1 ) then -- Positive
      return w
    else if w < 0x80 && w >= 2 ^ 7 - 2 ^ ( n - 1 ) then -- Negative
      return ( w - 0x80 )
    else if w >= 0x80 && n > 7 then -- More bytes
      (+) (w - 0x80) <$> ((*) 0x80 <$> (sleb128P (n-7)))
    else -- It is positive
      fail "Malformed LEB"

toLeb128 :: Integer -> [Word8]
toLeb128 w
  | w < 0      = fail "Negative Integer"
  | otherwise  =
      if w < 0x80 then
        [ fromInteger w ]
      else
        fromInteger ( 0x80 + w `mod` 0x80 ) : toLeb128 (fromInteger ( w `div` 0x80 ))

toSleb128 :: Integer -> [Word8]
toSleb128 w
  | w < 0x40 && w >= 0   = [ fromInteger w ]
  | w < 0 && w >= -0x40  = [ fromInteger ( w `mod` 0x80 ) ]
  | otherwise            = fromInteger ( 0x80 + w `mod` 0x80 ) : toSleb128 ( w `div` 0x80 )
