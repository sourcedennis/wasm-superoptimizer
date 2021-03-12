{-# LANGUAGE Strict #-}

module Lang.Wasm.BinaryParser.Instrs
  ( funcTypeIdxP
  , instrP
  , exprP
  ) where

import Melude
-- External library imports
import qualified Data.Attoparsec.ByteString as P
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import Lang.Wasm.Ast
  ( BlockType (..), MemArg (..), Expr (..), TypeIdx (..)
  , Sx (..), Instr (..), IUnop (..), IBinop (..), FUnop (..), FBinop (..)
  , ITestop (..), IRelop (..), FRelop (..), Cvtop (..), PrmInstr (..)
  , VarInstr (..), MemInstr (..), SimpleInstr (..), ResultType (..), ValType (..)
  , FuncType (..), Sat (..)
  )
-- Local imports
import Lang.Wasm.BinaryParser.General
  ( u32P, s33P, i32P, i64P, f32P, f64P, vecP, typeIdxP, labelIdxP, funcIdxP, localIdxP, globalIdxP )
import Lang.Wasm.BinaryParser.Types ( valTypeP )
import Lang.Wasm.BinaryParser.Helpers ( fromIntegerOrError )

funcTypeIdxP :: Vector FuncType -> Parser FuncType
funcTypeIdxP fts =
  do
    TypeIdx i <- typeIdxP
    return $ fromMaybe (error $ "No type at type idx " ++ show i ++ " " ++ show fts) (fts !? i)

blockTypeP :: Vector FuncType -> Parser BlockType
blockTypeP fts =
  P.choice
    [ P.word8 0x40 >> return (Ast.newFuncType [] [])
    , primitiveResultType <$> valTypeP
    , do
        i <- fromIntegerOrError <$> s33P
        case fts !? i of
          Just v  -> return v
          Nothing -> error "BlockTypeIdx out of range"
    ]

primitiveResultType :: ValType -> FuncType
primitiveResultType t = Ast.newFuncType [] [t]

memArgP :: Parser MemArg
memArgP =
  do
    align <- u32P
    offset <- u32P
    return $ MemArg (fromInteger offset) (fromInteger align)

instrP :: Vector FuncType -> Parser Instr
instrP fts =
  do
    v <- P.anyWord8
    case v of
      0x00 -> return InstrUnreachable 
      0x01 -> return InstrNop
      0x02 ->
        do
          bt <- blockTypeP fts
          instrs <- P.many' (instrP fts)
          P.word8 0x0B
          return $ InstrBlock bt instrs
      0x03 ->
        do
          bt <- blockTypeP fts
          instrs <- P.many' (instrP fts)
          P.word8 0x0B
          return $ InstrLoop bt instrs
      0x04 ->
        do
          bt <- blockTypeP fts
          ifInstrs <- P.many' (instrP fts)
          hasElse <- P.satisfy (\x -> x == 0x05 || x == 0x0B)
          case hasElse of
            0x05 ->
              do
                elseInstrs <- P.many' (instrP fts)
                P.word8 0x0B
                return $ InstrIf bt ifInstrs elseInstrs
            0x0B -> return $ InstrIf bt ifInstrs []
      0x0C -> InstrBr <$> labelIdxP
      0x0D -> InstrBrIf <$> labelIdxP
      0x0E -> InstrBrTable <$> vecP labelIdxP <*> labelIdxP
      0x0F -> return InstrReturn
      0x10 -> InstrCall <$> funcIdxP
      0x11 ->
        do
          TypeIdx x <- typeIdxP
          P.word8 0x00
          return $ InstrCallIndirect $ fromMaybe (error $ "No type at type idx " ++ show x) (fts !? x)
      -- Parameteric Instructions
      0x1A -> return $ InstrSimple $ SPrmInstrI32 PDrop
      0x1B -> return $ InstrSimple $ SPrmInstrI32 PSelect
      -- Variable Instructions
      0x20 -> ( InstrSimple . SVarInstr . VLocalGet  ) <$> localIdxP
      0x21 -> ( InstrSimple . SVarInstr . VLocalSet  ) <$> localIdxP
      0x22 -> ( InstrSimple . SVarInstr . VLocalTee  ) <$> localIdxP
      0x23 -> ( InstrSimple . SVarInstr . VGlobalGet ) <$> globalIdxP
      0x24 -> ( InstrSimple . SVarInstr . VGlobalSet ) <$> globalIdxP
      -- Memory Instructions
      0x28 -> ( InstrSimple . SMemInstr . MI32Load     ) <$> memArgP
      0x29 -> ( InstrSimple . SMemInstr . MI64Load     ) <$> memArgP
      0x2A -> ( InstrSimple . SMemInstr . MF32Load     ) <$> memArgP
      0x2B -> ( InstrSimple . SMemInstr . MF64Load     ) <$> memArgP
      0x2C -> ( InstrSimple . SMemInstr . MI32Load8 S  ) <$> memArgP
      0x2D -> ( InstrSimple . SMemInstr . MI32Load8 U  ) <$> memArgP
      0x2E -> ( InstrSimple . SMemInstr . MI32Load16 S ) <$> memArgP
      0x2F -> ( InstrSimple . SMemInstr . MI32Load16 U ) <$> memArgP
      0x30 -> ( InstrSimple . SMemInstr . MI64Load8 S  ) <$> memArgP
      0x31 -> ( InstrSimple . SMemInstr . MI64Load8 U  ) <$> memArgP
      0x32 -> ( InstrSimple . SMemInstr . MI64Load16 S ) <$> memArgP
      0x33 -> ( InstrSimple . SMemInstr . MI64Load16 U ) <$> memArgP
      0x34 -> ( InstrSimple . SMemInstr . MI64Load32 S ) <$> memArgP
      0x35 -> ( InstrSimple . SMemInstr . MI64Load32 U ) <$> memArgP
      0x36 -> ( InstrSimple . SMemInstr . MI32Store    ) <$> memArgP
      0x37 -> ( InstrSimple . SMemInstr . MI64Store    ) <$> memArgP
      0x38 -> ( InstrSimple . SMemInstr . MF32Store    ) <$> memArgP
      0x39 -> ( InstrSimple . SMemInstr . MF64Store    ) <$> memArgP
      0x3A -> ( InstrSimple . SMemInstr . MI32Store8   ) <$> memArgP
      0x3B -> ( InstrSimple . SMemInstr . MI32Store16  ) <$> memArgP
      0x3C -> ( InstrSimple . SMemInstr . MI64Store8   ) <$> memArgP
      0x3D -> ( InstrSimple . SMemInstr . MI64Store16  ) <$> memArgP
      0x3E -> ( InstrSimple . SMemInstr . MI64Store32  ) <$> memArgP
      0x3F -> P.word8 0x00 >> return ( InstrSimple $ SMemInstr $ MMemorySize )
      0x40 -> P.word8 0x00 >> return ( InstrSimple $ SMemInstr $ MMemoryGrow )
      -- Numeric Instructions
      0x41 -> ( InstrSimple . SConstI32 ) <$> i32P
      0x42 -> ( InstrSimple . SConstI64 ) <$> i64P
      0x43 -> ( InstrSimple . SConstF32 ) <$> f32P
      0x44 -> ( InstrSimple . SConstF64 ) <$> f64P

      0x45 -> return $ InstrSimple $ STestopI32 ITestEqz

      0x46 -> return $ InstrSimple $ SRelopI32 IRelEq
      0x47 -> return $ InstrSimple $ SRelopI32 IRelNe
      0x48 -> return $ InstrSimple $ SRelopI32 (IRelLt S)
      0x49 -> return $ InstrSimple $ SRelopI32 (IRelLt U)
      0x4A -> return $ InstrSimple $ SRelopI32 (IRelGt S)
      0x4B -> return $ InstrSimple $ SRelopI32 (IRelGt U)
      0x4C -> return $ InstrSimple $ SRelopI32 (IRelLe S)
      0x4D -> return $ InstrSimple $ SRelopI32 (IRelLe U)
      0x4E -> return $ InstrSimple $ SRelopI32 (IRelGe S)
      0x4F -> return $ InstrSimple $ SRelopI32 (IRelGe U)
      
      0x50 -> return $ InstrSimple $ STestopI64 ITestEqz

      0x51 -> return $ InstrSimple $ SRelopI64 IRelEq
      0x52 -> return $ InstrSimple $ SRelopI64 IRelNe
      0x53 -> return $ InstrSimple $ SRelopI64 (IRelLt S)
      0x54 -> return $ InstrSimple $ SRelopI64 (IRelLt U)
      0x55 -> return $ InstrSimple $ SRelopI64 (IRelGt S)
      0x56 -> return $ InstrSimple $ SRelopI64 (IRelGt U)
      0x57 -> return $ InstrSimple $ SRelopI64 (IRelLe S)
      0x58 -> return $ InstrSimple $ SRelopI64 (IRelLe U)
      0x59 -> return $ InstrSimple $ SRelopI64 (IRelGe S)
      0x5A -> return $ InstrSimple $ SRelopI64 (IRelGe U)

      0x5B -> return $ InstrSimple $ SRelopF32 FRelEq
      0x5C -> return $ InstrSimple $ SRelopF32 FRelNe
      0x5D -> return $ InstrSimple $ SRelopF32 FRelLt
      0x5E -> return $ InstrSimple $ SRelopF32 FRelGt
      0x5F -> return $ InstrSimple $ SRelopF32 FRelLe
      0x60 -> return $ InstrSimple $ SRelopF32 FRelGe

      0x61 -> return $ InstrSimple $ SRelopF64 FRelEq
      0x62 -> return $ InstrSimple $ SRelopF64 FRelNe
      0x63 -> return $ InstrSimple $ SRelopF64 FRelLt
      0x64 -> return $ InstrSimple $ SRelopF64 FRelGt
      0x65 -> return $ InstrSimple $ SRelopF64 FRelLe
      0x66 -> return $ InstrSimple $ SRelopF64 FRelGe

      0x67 -> return $ InstrSimple $ SUnopI32 IUnClz
      0x68 -> return $ InstrSimple $ SUnopI32 IUnCtz
      0x69 -> return $ InstrSimple $ SUnopI32 IUnPopcnt

      0x6A -> return $ InstrSimple $ SBinopI32 IBinAdd
      0x6B -> return $ InstrSimple $ SBinopI32 IBinSub
      0x6C -> return $ InstrSimple $ SBinopI32 IBinMul
      0x6D -> return $ InstrSimple $ SBinopI32 (IBinDiv S)
      0x6E -> return $ InstrSimple $ SBinopI32 (IBinDiv U)
      0x6F -> return $ InstrSimple $ SBinopI32 (IBinRem S)
      0x70 -> return $ InstrSimple $ SBinopI32 (IBinRem U)
      0x71 -> return $ InstrSimple $ SBinopI32 IBinAnd
      0x72 -> return $ InstrSimple $ SBinopI32 IBinOr
      0x73 -> return $ InstrSimple $ SBinopI32 IBinXor
      0x74 -> return $ InstrSimple $ SBinopI32 IBinShl
      0x75 -> return $ InstrSimple $ SBinopI32 (IBinShr S)
      0x76 -> return $ InstrSimple $ SBinopI32 (IBinShr U)
      0x77 -> return $ InstrSimple $ SBinopI32 IBinRotl
      0x78 -> return $ InstrSimple $ SBinopI32 IBinRotr
      
      0x79 -> return $ InstrSimple $ SUnopI64 IUnClz
      0x7A -> return $ InstrSimple $ SUnopI64 IUnCtz
      0x7B -> return $ InstrSimple $ SUnopI64 IUnPopcnt

      0x7C -> return $ InstrSimple $ SBinopI64 IBinAdd
      0x7D -> return $ InstrSimple $ SBinopI64 IBinSub
      0x7E -> return $ InstrSimple $ SBinopI64 IBinMul
      0x7F -> return $ InstrSimple $ SBinopI64 (IBinDiv S)
      0x80 -> return $ InstrSimple $ SBinopI64 (IBinDiv U)
      0x81 -> return $ InstrSimple $ SBinopI64 (IBinRem S)
      0x82 -> return $ InstrSimple $ SBinopI64 (IBinRem U)
      0x83 -> return $ InstrSimple $ SBinopI64 IBinAnd
      0x84 -> return $ InstrSimple $ SBinopI64 IBinOr
      0x85 -> return $ InstrSimple $ SBinopI64 IBinXor
      0x86 -> return $ InstrSimple $ SBinopI64 IBinShl
      0x87 -> return $ InstrSimple $ SBinopI64 (IBinShr S)
      0x88 -> return $ InstrSimple $ SBinopI64 (IBinShr U)
      0x89 -> return $ InstrSimple $ SBinopI64 IBinRotl
      0x8A -> return $ InstrSimple $ SBinopI64 IBinRotr

      0x8B -> return $ InstrSimple $ SUnopF32 FUnAbs
      0x8C -> return $ InstrSimple $ SUnopF32 FUnNeg
      0x8D -> return $ InstrSimple $ SUnopF32 FUnCeil
      0x8E -> return $ InstrSimple $ SUnopF32 FUnFloor
      0x8F -> return $ InstrSimple $ SUnopF32 FUnTrunc
      0x90 -> return $ InstrSimple $ SUnopF32 FUnNearest
      0x91 -> return $ InstrSimple $ SUnopF32 FUnSqrt

      0x92 -> return $ InstrSimple $ SBinopF32 FBinAdd
      0x93 -> return $ InstrSimple $ SBinopF32 FBinSub
      0x94 -> return $ InstrSimple $ SBinopF32 FBinMul
      0x95 -> return $ InstrSimple $ SBinopF32 FBinDiv
      0x96 -> return $ InstrSimple $ SBinopF32 FBinMin
      0x97 -> return $ InstrSimple $ SBinopF32 FBinMax
      0x98 -> return $ InstrSimple $ SBinopF32 FBinCopysign

      0x99 -> return $ InstrSimple $ SUnopF64 FUnAbs
      0x9A -> return $ InstrSimple $ SUnopF64 FUnNeg
      0x9B -> return $ InstrSimple $ SUnopF64 FUnCeil
      0x9C -> return $ InstrSimple $ SUnopF64 FUnFloor
      0x9D -> return $ InstrSimple $ SUnopF64 FUnTrunc
      0x9E -> return $ InstrSimple $ SUnopF64 FUnNearest
      0x9F -> return $ InstrSimple $ SUnopF64 FUnSqrt
      
      0xA0 -> return $ InstrSimple $ SBinopF64 FBinAdd
      0xA1 -> return $ InstrSimple $ SBinopF64 FBinSub
      0xA2 -> return $ InstrSimple $ SBinopF64 FBinMul
      0xA3 -> return $ InstrSimple $ SBinopF64 FBinDiv
      0xA4 -> return $ InstrSimple $ SBinopF64 FBinMin
      0xA5 -> return $ InstrSimple $ SBinopF64 FBinMax
      0xA6 -> return $ InstrSimple $ SBinopF64 FBinCopysign

      0xA7 -> return $ InstrSimple $ SCvtop CI32WrapI64
      0xA8 -> return $ InstrSimple $ SCvtop (CI32TruncF32 NoSat S)
      0xA9 -> return $ InstrSimple $ SCvtop (CI32TruncF32 NoSat U)
      0xAA -> return $ InstrSimple $ SCvtop (CI32TruncF64 NoSat S)
      0xAB -> return $ InstrSimple $ SCvtop (CI32TruncF64 NoSat U)
      0xAC -> return $ InstrSimple $ SCvtop (CI64ExtendI32 S)
      0xAD -> return $ InstrSimple $ SCvtop (CI64ExtendI32 U)
      0xAE -> return $ InstrSimple $ SCvtop (CI64TruncF32 NoSat S)
      0xAF -> return $ InstrSimple $ SCvtop (CI64TruncF32 NoSat U)
      0xB0 -> return $ InstrSimple $ SCvtop (CI64TruncF64 NoSat S)
      0xB1 -> return $ InstrSimple $ SCvtop (CI64TruncF64 NoSat U)
      0xB2 -> return $ InstrSimple $ SCvtop (CF32ConvertI32 S)
      0xB3 -> return $ InstrSimple $ SCvtop (CF32ConvertI32 U)
      0xB4 -> return $ InstrSimple $ SCvtop (CF32ConvertI64 S)
      0xB5 -> return $ InstrSimple $ SCvtop (CF32ConvertI64 U)
      0xB6 -> return $ InstrSimple $ SCvtop CF32DemoteF64
      0xB7 -> return $ InstrSimple $ SCvtop (CF64ConvertI32 S)
      0xB8 -> return $ InstrSimple $ SCvtop (CF64ConvertI32 U)
      0xB9 -> return $ InstrSimple $ SCvtop (CF64ConvertI64 S)
      0xBA -> return $ InstrSimple $ SCvtop (CF64ConvertI64 U)
      0xBB -> return $ InstrSimple $ SCvtop CF64PromoteF32
      0xBC -> return $ InstrSimple $ SCvtop CI32ReinterpretF32
      0xBD -> return $ InstrSimple $ SCvtop CI64ReinterpretF64
      0xBE -> return $ InstrSimple $ SCvtop CF32ReinterpretI32
      0xBF -> return $ InstrSimple $ SCvtop CF64ReinterpretI64

      0xC0 -> return $ InstrSimple $ SCvtop CI32Extend8S
      0xC1 -> return $ InstrSimple $ SCvtop CI32Extend16S
      0xC2 -> return $ InstrSimple $ SCvtop CI64Extend8S
      0xC3 -> return $ InstrSimple $ SCvtop CI64Extend16S
      0xC4 -> return $ InstrSimple $ SCvtop CI64Extend32S

      0xFC ->
        do
          y <- P.satisfy (\x -> x >= 0x00 || x <= 0x07)
          case y of
            0x00 -> return $ InstrSimple $ SCvtop (CI32TruncF32 Sat S)
            0x01 -> return $ InstrSimple $ SCvtop (CI32TruncF32 Sat U)
            0x02 -> return $ InstrSimple $ SCvtop (CI32TruncF64 Sat S)
            0x03 -> return $ InstrSimple $ SCvtop (CI32TruncF64 Sat U)
            0x04 -> return $ InstrSimple $ SCvtop (CI64TruncF32 Sat S)
            0x05 -> return $ InstrSimple $ SCvtop (CI64TruncF32 Sat U)
            0x06 -> return $ InstrSimple $ SCvtop (CI64TruncF64 Sat S)
            0x07 -> return $ InstrSimple $ SCvtop (CI64TruncF64 Sat U)

      _ -> fail "Invalid instruction"

exprP :: Vector FuncType -> Parser Expr
exprP fts =
  do
    instrs <- P.many' (instrP fts)
    P.word8 0x0B
    return instrs
