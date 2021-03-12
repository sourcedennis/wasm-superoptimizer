{-# LANGUAGE Strict #-}

module Lang.Wasm.BinaryParser.Types
  ( valTypeP
  , resultTypeP
  , funcTypeP
  , limitsP
  , tableTypeP
  , globalTypeP
  ) where

-- Extra stdlib imports
import qualified Data.Vector as Vector
-- External library imports
import           Data.Attoparsec.ByteString ( Parser )
import qualified Data.Attoparsec.ByteString as P
-- Local library imports
import Lang.Wasm.Ast
  ( ValType (..), ResultType (..), FuncType (..), Limits (..)
  , TableType (..), ElemType (..), GlobalType (..), Mut (..)
  )
-- Local imports
import Lang.Wasm.BinaryParser.General ( u32P, u64P, vecP )

-- | A parser for Value types, which are encoded by a single byte
valTypeP :: Parser ValType
valTypeP =
  do
    v <- P.satisfy (\v -> v `elem` [0x7F, 0x7E, 0x7D, 0x7C])
    case v of
      0x7F -> return TI32
      0x7E -> return TI64
      0x7D -> return TF32
      0x7C -> return TF64

resultTypeP :: Parser ResultType
resultTypeP = Vector.fromList <$> vecP valTypeP

funcTypeP :: Parser FuncType
funcTypeP =
  do
    P.word8 0x60
    FuncType <$> resultTypeP <*> resultTypeP

limitsP :: Parser Limits
limitsP =
  do
    v <- P.satisfy (\v -> v == 0x00 || v == 0x01)
    case v of
      0x00  -> Limits <$> u32P <*> pure Nothing
      0x01  -> Limits <$> u32P <*> (Just <$> u32P)

tableTypeP :: Parser Limits
tableTypeP =
  do
    -- In the current WebAssembly version, the element type remains unused
    elemTypeP
    limitsP
  where
  elemTypeP :: Parser ElemType
  elemTypeP = P.word8 0x70 >> return FuncRef

globalTypeP :: Parser GlobalType
globalTypeP =
  do
    t <- valTypeP
    m <- mutP
    return $ GlobalType m t
  where
  mutP :: Parser Mut
  mutP =
    do
      v <- P.satisfy (\v -> v == 0x00 || v == 0x01)
      case v of
        0x00  -> return MConst
        0x01  -> return MVar
