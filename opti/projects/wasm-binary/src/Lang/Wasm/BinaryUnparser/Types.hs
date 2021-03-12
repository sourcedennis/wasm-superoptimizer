module Lang.Wasm.BinaryUnparser.Types where

-- Extra stdlib imports
import qualified Data.Vector as Vector
-- External library imports
import qualified Data.ByteString.Builder as B
import           Data.ByteString.Builder ( Builder )
-- Local library imports
import           Lang.Wasm.Ast
  ( ValType (..), Mut (..), GlobalType (..), Limits (..), TableType (..)
  , Limits (..), ElemType (..), ResultType (..), FuncType (..)
  )
-- Local imports
import           Lang.Wasm.BinaryUnparser.General ( u32W, vecW )


valTypeW :: ValType -> Builder
valTypeW TI32 = B.word8 0x7F
valTypeW TI64 = B.word8 0x7E
valTypeW TF32 = B.word8 0x7D
valTypeW TF64 = B.word8 0x7C

resultTypeW :: ResultType -> Builder
resultTypeW = vecW valTypeW . Vector.toList

funcTypeW :: FuncType -> Builder
funcTypeW ft =
  B.word8 0x60 <> resultTypeW (ftParams ft) <> resultTypeW (ftResults ft)

limitsW :: Limits -> Builder
limitsW (Limits low Nothing)     = B.word8 0x00 <> u32W low
limitsW (Limits low (Just high)) = B.word8 0x01 <> u32W low <> u32W high

tableTypeW :: TableType -> Builder
tableTypeW (TableType limits et) = elemTypeW et <> limitsW limits
  where
  elemTypeW :: ElemType -> Builder
  elemTypeW et = B.word8 0x70

globalTypeW :: GlobalType -> Builder
globalTypeW (GlobalType m vt) =
  valTypeW vt <> mutW m
  where
  mutW :: Mut -> Builder
  mutW MConst = B.word8 0x00
  mutW MVar   = B.word8 0x01
