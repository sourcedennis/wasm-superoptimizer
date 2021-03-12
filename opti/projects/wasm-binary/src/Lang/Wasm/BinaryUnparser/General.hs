{-# LANGUAGE NumericUnderscores #-}

module Lang.Wasm.BinaryUnparser.General where

-- Stdlib imports
import           Data.Char ( ord )
-- External library imports
import qualified Data.ByteString as BS
import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Builder as B
import           Data.ByteString.Builder ( Builder )
-- Local library imports
import           Lang.Wasm.Ast
  ( WI32 (..), WI64 (..), WF32 (..), WF64 (..)
  , TypeIdx (..), FuncIdx (..), TableIdx (..), MemIdx (..), GlobalIdx (..)
  , LocalIdx (..), LabelIdx (..), WName
  )
-- Local imports
import           Data.Format.Leb128 ( toLeb128, toSleb128 )


vecW :: ( a -> Builder ) -> [a] -> Builder
vecW f xs = u32W (toInteger $ length xs) <> mconcat (map f xs)

vecBytesW :: ByteString -> Builder
vecBytesW xs = u32W (toInteger $ BS.length xs) <> B.byteString xs

s33W :: Integer -> Builder
s33W = sleb128W 33

u32W :: Integer -> Builder
u32W = leb128W 32

u64W :: Integer -> Builder
u64W = leb128W 64

s32W :: Integer -> Builder
s32W = sleb128W 32

s64W :: Integer -> Builder
s64W = sleb128W 64

i32W :: WI32 -> Builder
i32W (WI32 x)
  | x >= 0x8000_0000  = s32W (toInteger x - 0x1_0000_0000)
  | otherwise         = s32W (toInteger x)

i64W :: WI64 -> Builder
i64W (WI64 x)
  | x >= 0x8000_0000_0000_0000  = s64W (toInteger x - 0x1_0000_0000_0000_0000)
  | otherwise                   = s64W (toInteger x)

f32W :: WF32 -> Builder
f32W (WF32 x) = integerOctetsW 4 (toInteger x)

f64W :: WF64 -> Builder
f64W (WF64 x) = integerOctetsW 8 (toInteger x)

sleb128W :: Integer -> Integer -> Builder
sleb128W numBits v
  | v < -( 2 ^ ( numBits - 1 ) ) || v >= 2 ^ ( numBits - 1 )  = error "SLEB128 value outside domain"
  | otherwise                                                 = B.byteString $ BS.pack $ toSleb128 v

leb128W :: Integer -> Integer -> Builder
leb128W numBits v
  | v < 0 || v >= 2 ^ numBits  = error "LEB128 value outside domain"
  | otherwise                  = B.byteString $ BS.pack $ toLeb128 v

typeIdxW :: TypeIdx -> Builder
typeIdxW (TypeIdx i) = u32W $ toInteger i

funcIdxW :: FuncIdx -> Builder
funcIdxW (FuncIdx i) = u32W $ toInteger i

tableIdxW :: TableIdx -> Builder
tableIdxW (TableIdx i) = u32W $ toInteger i

memIdxW :: MemIdx -> Builder
memIdxW (MemIdx i) = u32W $ toInteger i

globalIdxW :: GlobalIdx -> Builder
globalIdxW (GlobalIdx i) = u32W $ toInteger i

localIdxW :: LocalIdx -> Builder
localIdxW (LocalIdx i) = u32W $ toInteger i

labelIdxW :: LabelIdx -> Builder
labelIdxW (LabelIdx i) = u32W $ toInteger i

-- | Writes `n` Little Endian bytes
integerOctetsW :: Int -> Integer -> Builder
integerOctetsW 0 _ = mempty
integerOctetsW n v = B.word8 ( fromInteger ( v `mod` 0x100 ) ) <> integerOctetsW (n-1) ( v `div` 0x100 )

nameW :: WName -> Builder
nameW = vecW B.word8 . map (toEnum . ord)
