{-# LANGUAGE Strict #-}

module Lang.Wasm.BinaryParser.Helpers
  ( parseSub
  , catch
  , fromIntegerOrError
  ) where

-- External library imports
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P

-- | Consumes a subregion of exactly `n` bytes long, which is subsequently
-- parsed with the provided parser. The parser must consume the entire
-- input, or parsing fails.
parseSub :: Int -> Parser a -> Parser a
parseSub n p =
  do
    res  <- P.parseOnly
              (do
                v <- p
                P.endOfInput
                return v
              )
              <$> P.take n -- TODO: Check if this may also consume less than `n`
    case res of
      Left err  -> fail err
      Right r   -> return r

-- | Tries to parse with the given parser. If it fails, `Nothing` is returned
-- and no bytes are consumed.
catch :: Parser a -> Parser (Maybe a)
catch p =
  P.choice
    [ Just <$> p
    , return Nothing
    ]

fromIntegerOrError :: Integer -> Int
fromIntegerOrError i =
  if i >= toInteger ( minBound :: Int ) && i <= toInteger ( maxBound :: Int ) then
    fromInteger i
  else
    error "Integer out of bounds"
