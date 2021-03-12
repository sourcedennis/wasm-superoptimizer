{-# LANGUAGE Strict #-}

module Lang.Wasm.BinaryParser.General
  ( vecP
  , vecBytesP
  , s33P
  , u32P
  , u64P
  , s32P
  , s64P
  , i32P
  , i64P
  , f32P
  , f64P
  , typeIdxP
  , funcIdxP
  , tableIdxP
  , memIdxP
  , globalIdxP
  , localIdxP
  , labelIdxP
  , nameP
  ) where

-- Stdlib imports
import Data.Char (chr)
-- External library imports
import           Data.ByteString (ByteString)
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
-- Local library imports
import Lang.Wasm.Ast
  ( TypeIdx (..), FuncIdx (..), TableIdx (..), MemIdx (..), GlobalIdx (..)
  , LocalIdx (..), LabelIdx (..), WName, WI32 (..), WI64 (..), WF32 (..)
  , WF64 (..)
  )
-- Local imports
import Data.Format.Leb128 (leb128P, sleb128P)
import Lang.Wasm.BinaryParser.Helpers ( fromIntegerOrError )

-- | Vectors are encoded with their `u32` length followed by the encoding of
-- their element sequence
vecP :: Parser a -> Parser [a]
vecP p =
  do
    n <- fromInteger <$> u32P
    P.count n p

-- | See `vecP`. This is a faster version specifically for parsing a sequence of
-- bytes into a `ByteString`.
vecBytesP :: Parser ByteString
vecBytesP =
  do
    n <- fromInteger <$> u32P
    P.take n

-- | Special 33-bit integer used for `blockTypeP`
s33P :: Parser Integer
s33P = sleb128P 33

u32P :: Parser Integer
u32P = leb128P 32

u64P :: Parser Integer
u64P = leb128P 64

s32P :: Parser Integer
s32P = sleb128P 32

s64P :: Parser Integer
s64P = sleb128P 64

signedToBits :: Int -> Integer -> Integer
signedToBits numBits x = x `mod` (2 ^ numBits)

-- | Uninterpreted 32-bit integer
i32P :: Parser WI32
i32P = WI32 <$> ((fromInteger . signedToBits 32) <$> s32P)

-- | Uninterpreted 64-bit integer
i64P :: Parser WI64
i64P = WI64 <$> ((fromInteger . signedToBits 64) <$> s64P)

f32P :: Parser WF32
f32P = WF32 <$> (fromInteger <$> integerOctetsP 4)

f64P :: Parser WF64
f64P = WF64 <$> (fromInteger <$> integerOctetsP 8)

-- | Parses `n` Little Endian bytes to an integer
integerOctetsP :: Int -> Parser Integer
integerOctetsP 0 = return 0
integerOctetsP n =
  do
    v <- P.anyWord8
    rem <- integerOctetsP (n-1)
    return (rem * 0x100 + toInteger v)

typeIdxP :: Parser TypeIdx
typeIdxP = (TypeIdx . fromIntegerOrError) <$> u32P

funcIdxP :: Parser FuncIdx
funcIdxP = (FuncIdx . fromIntegerOrError) <$> u32P

tableIdxP :: Parser TableIdx
tableIdxP = (TableIdx . fromIntegerOrError) <$> u32P

memIdxP :: Parser MemIdx
memIdxP = (MemIdx . fromIntegerOrError) <$> u32P

globalIdxP :: Parser GlobalIdx
globalIdxP = (GlobalIdx . fromIntegerOrError) <$> u32P

localIdxP :: Parser LocalIdx
localIdxP = (LocalIdx . fromIntegerOrError) <$> u32P

labelIdxP :: Parser LabelIdx
labelIdxP = (LabelIdx . fromIntegerOrError) <$> u32P

nameP :: Parser WName
nameP = map (chr . fromEnum) <$> vecP P.anyWord8
