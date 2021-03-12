module Lang.Wasm.BinaryUnparser.Instrs where

-- Extra stdlib imports
import qualified Data.Vector as Vector
-- External library imports
import qualified Data.ByteString as BS
import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Builder as B
import           Data.ByteString.Builder ( Builder )
import qualified Data.IdHashSet as IdHashSet
import           Data.IdHashSet ( IdHashSet )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( TypeIdx (..), BlockType (..), Expr (..)
  , Instr (..), IUnop (..), IBinop (..), FUnop (..), FBinop (..), ITestop (..)
  , IRelop (..), FRelop (..), Cvtop (..), PrmInstr (..), VarInstr (..)
  , MemInstr (..), SimpleInstr (..), Sx (..), Sat (..), MemArg (..)
  , FuncType (..)
  )
-- Local imports
import           Lang.Wasm.BinaryUnparser.General
  ( vecW, s33W, u32W, u64W, s32W, s64W, i32W, i64W, f32W, f64W, typeIdxW
  , funcIdxW, globalIdxW, localIdxW, labelIdxW
  )
import           Lang.Wasm.BinaryUnparser.Types ( valTypeW )

blockTypeW :: IdHashSet FuncType -> BlockType -> Builder
blockTypeW fts bt@(FuncType xs ys)
  | Vector.length xs == 0 && Vector.length ys == 0  =
      B.word8 0x40
  | Vector.length xs == 0 && Vector.length ys == 1  =
      valTypeW $ Vector.head ys
  | otherwise =
      maybe
        (error $ "Missing FuncType " ++ show bt ++ " " ++ show (IdHashSet.elems fts))
        (s33W . toInteger) $ IdHashSet.lookupKey bt fts

funcTypeIdxW :: IdHashSet FuncType -> FuncType -> Builder
funcTypeIdxW fts bt =
  maybe (error $ "Missing FuncType " ++ show bt ++ " " ++ show (IdHashSet.elems fts)) (u32W . toInteger) $ IdHashSet.lookupKey bt fts

memArgW :: MemArg -> Builder
memArgW (MemArg offset align) = u32W (toInteger align) <> u32W (toInteger offset)

instrW :: IdHashSet FuncType -> Instr -> Builder
instrW fts InstrUnreachable   = B.word8 0x00
instrW fts InstrNop           = B.word8 0x01
instrW fts (InstrBlock bt xs) = B.word8 0x02 <> blockTypeW fts bt <> mconcat (map (instrW fts) xs) <> B.word8 0x0B
instrW fts (InstrLoop bt xs)  = B.word8 0x03 <> blockTypeW fts bt <> mconcat (map (instrW fts) xs) <> B.word8 0x0B
instrW fts (InstrIf bt xs []) = B.word8 0x04 <> blockTypeW fts bt <> mconcat (map (instrW fts) xs) <> B.word8 0x0B
instrW fts (InstrIf bt xs ys) =
  B.word8 0x04 <> blockTypeW fts bt <> mconcat (map (instrW fts) xs) <> B.word8 0x05 <> mconcat (map (instrW fts) ys) <> B.word8 0x0B
instrW fts (InstrBr lIdx)            = B.word8 0x0C <> labelIdxW lIdx
instrW fts (InstrBrIf lIdx)          = B.word8 0x0D <> labelIdxW lIdx
instrW fts (InstrBrTable lIdxs lIdx) = B.word8 0x0E <> vecW labelIdxW lIdxs <> labelIdxW lIdx
instrW fts InstrReturn               = B.word8 0x0F
instrW fts (InstrCall fIdx)          = B.word8 0x10 <> funcIdxW fIdx
instrW fts (InstrCallIndirect ft)    = B.word8 0x11 <> funcTypeIdxW fts ft <> B.word8 0x00
-- Parameteric Instructions
instrW _ (InstrSimple (SPrmInstrI32 PDrop))   = B.word8 0x1A
instrW _ (InstrSimple (SPrmInstrI64 PDrop))   = B.word8 0x1A
instrW _ (InstrSimple (SPrmInstrF32 PDrop))   = B.word8 0x1A
instrW _ (InstrSimple (SPrmInstrF64 PDrop))   = B.word8 0x1A

instrW _ (InstrSimple (SPrmInstrI32 PSelect)) = B.word8 0x1B
instrW _ (InstrSimple (SPrmInstrI64 PSelect)) = B.word8 0x1B
instrW _ (InstrSimple (SPrmInstrF32 PSelect)) = B.word8 0x1B
instrW _ (InstrSimple (SPrmInstrF64 PSelect)) = B.word8 0x1B
-- Variable Instructions
instrW _ (InstrSimple (SVarInstr (VLocalGet lIdx)))  = B.word8 0x20 <> localIdxW lIdx
instrW _ (InstrSimple (SVarInstr (VLocalSet lIdx)))  = B.word8 0x21 <> localIdxW lIdx
instrW _ (InstrSimple (SVarInstr (VLocalTee lIdx)))  = B.word8 0x22 <> localIdxW lIdx
instrW _ (InstrSimple (SVarInstr (VGlobalGet lIdx))) = B.word8 0x23 <> globalIdxW lIdx
instrW _ (InstrSimple (SVarInstr (VGlobalSet lIdx))) = B.word8 0x24 <> globalIdxW lIdx
-- Memory Instructions
instrW _ (InstrSimple (SMemInstr (MI32Load     ma))) = B.word8 0x28 <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI64Load     ma))) = B.word8 0x29 <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MF32Load     ma))) = B.word8 0x2A <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MF64Load     ma))) = B.word8 0x2B <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI32Load8  S ma))) = B.word8 0x2C <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI32Load8  U ma))) = B.word8 0x2D <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI32Load16 S ma))) = B.word8 0x2E <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI32Load16 U ma))) = B.word8 0x2F <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI64Load8  S ma))) = B.word8 0x30 <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI64Load8  U ma))) = B.word8 0x31 <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI64Load16 S ma))) = B.word8 0x32 <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI64Load16 U ma))) = B.word8 0x33 <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI64Load32 S ma))) = B.word8 0x34 <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI64Load32 U ma))) = B.word8 0x35 <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI32Store    ma))) = B.word8 0x36 <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI64Store    ma))) = B.word8 0x37 <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MF32Store    ma))) = B.word8 0x38 <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MF64Store    ma))) = B.word8 0x39 <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI32Store8   ma))) = B.word8 0x3A <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI32Store16  ma))) = B.word8 0x3B <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI64Store8   ma))) = B.word8 0x3C <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI64Store16  ma))) = B.word8 0x3D <> memArgW ma
instrW _ (InstrSimple (SMemInstr (MI64Store32  ma))) = B.word8 0x3E <> memArgW ma
instrW _ (InstrSimple (SMemInstr MMemorySize))       = B.word8 0x3F <> B.word8 0x00
instrW _ (InstrSimple (SMemInstr MMemoryGrow))       = B.word8 0x40 <> B.word8 0x00
-- Numeric Instructions
instrW _ (InstrSimple (SConstI32 v)) = B.word8 0x41 <> i32W v
instrW _ (InstrSimple (SConstI64 v)) = B.word8 0x42 <> i64W v
instrW _ (InstrSimple (SConstF32 v)) = B.word8 0x43 <> f32W v
instrW _ (InstrSimple (SConstF64 v)) = B.word8 0x44 <> f64W v

instrW _ (InstrSimple (STestopI32 ITestEqz))  = B.word8 0x45

instrW _ (InstrSimple (SRelopI32 IRelEq))     = B.word8 0x46
instrW _ (InstrSimple (SRelopI32 IRelNe))     = B.word8 0x47
instrW _ (InstrSimple (SRelopI32 (IRelLt S))) = B.word8 0x48
instrW _ (InstrSimple (SRelopI32 (IRelLt U))) = B.word8 0x49
instrW _ (InstrSimple (SRelopI32 (IRelGt S))) = B.word8 0x4A
instrW _ (InstrSimple (SRelopI32 (IRelGt U))) = B.word8 0x4B
instrW _ (InstrSimple (SRelopI32 (IRelLe S))) = B.word8 0x4C
instrW _ (InstrSimple (SRelopI32 (IRelLe U))) = B.word8 0x4D
instrW _ (InstrSimple (SRelopI32 (IRelGe S))) = B.word8 0x4E
instrW _ (InstrSimple (SRelopI32 (IRelGe U))) = B.word8 0x4F

instrW _ (InstrSimple (STestopI64 ITestEqz))  = B.word8 0x50

instrW _ (InstrSimple (SRelopI64 IRelEq))     = B.word8 0x51
instrW _ (InstrSimple (SRelopI64 IRelNe))     = B.word8 0x52
instrW _ (InstrSimple (SRelopI64 (IRelLt S))) = B.word8 0x53
instrW _ (InstrSimple (SRelopI64 (IRelLt U))) = B.word8 0x54
instrW _ (InstrSimple (SRelopI64 (IRelGt S))) = B.word8 0x55
instrW _ (InstrSimple (SRelopI64 (IRelGt U))) = B.word8 0x56
instrW _ (InstrSimple (SRelopI64 (IRelLe S))) = B.word8 0x57
instrW _ (InstrSimple (SRelopI64 (IRelLe U))) = B.word8 0x58
instrW _ (InstrSimple (SRelopI64 (IRelGe S))) = B.word8 0x59
instrW _ (InstrSimple (SRelopI64 (IRelGe U))) = B.word8 0x5A

instrW _ (InstrSimple (SRelopF32 FRelEq)) = B.word8 0x5B
instrW _ (InstrSimple (SRelopF32 FRelNe)) = B.word8 0x5C
instrW _ (InstrSimple (SRelopF32 FRelLt)) = B.word8 0x5D
instrW _ (InstrSimple (SRelopF32 FRelGt)) = B.word8 0x5E
instrW _ (InstrSimple (SRelopF32 FRelLe)) = B.word8 0x5F
instrW _ (InstrSimple (SRelopF32 FRelGe)) = B.word8 0x60

instrW _ (InstrSimple (SRelopF64 FRelEq)) = B.word8 0x61
instrW _ (InstrSimple (SRelopF64 FRelNe)) = B.word8 0x62
instrW _ (InstrSimple (SRelopF64 FRelLt)) = B.word8 0x63
instrW _ (InstrSimple (SRelopF64 FRelGt)) = B.word8 0x64
instrW _ (InstrSimple (SRelopF64 FRelLe)) = B.word8 0x65
instrW _ (InstrSimple (SRelopF64 FRelGe)) = B.word8 0x66

instrW _ (InstrSimple (SUnopI32 IUnClz))    = B.word8 0x67
instrW _ (InstrSimple (SUnopI32 IUnCtz))    = B.word8 0x68
instrW _ (InstrSimple (SUnopI32 IUnPopcnt)) = B.word8 0x69

instrW _ (InstrSimple (SBinopI32 IBinAdd    )) = B.word8 0x6A
instrW _ (InstrSimple (SBinopI32 IBinSub    )) = B.word8 0x6B
instrW _ (InstrSimple (SBinopI32 IBinMul    )) = B.word8 0x6C
instrW _ (InstrSimple (SBinopI32 (IBinDiv S))) = B.word8 0x6D
instrW _ (InstrSimple (SBinopI32 (IBinDiv U))) = B.word8 0x6E
instrW _ (InstrSimple (SBinopI32 (IBinRem S))) = B.word8 0x6F
instrW _ (InstrSimple (SBinopI32 (IBinRem U))) = B.word8 0x70
instrW _ (InstrSimple (SBinopI32 IBinAnd    )) = B.word8 0x71
instrW _ (InstrSimple (SBinopI32 IBinOr     )) = B.word8 0x72
instrW _ (InstrSimple (SBinopI32 IBinXor    )) = B.word8 0x73
instrW _ (InstrSimple (SBinopI32 IBinShl    )) = B.word8 0x74
instrW _ (InstrSimple (SBinopI32 (IBinShr S))) = B.word8 0x75
instrW _ (InstrSimple (SBinopI32 (IBinShr U))) = B.word8 0x76
instrW _ (InstrSimple (SBinopI32 IBinRotl   )) = B.word8 0x77
instrW _ (InstrSimple (SBinopI32 IBinRotr   )) = B.word8 0x78

instrW _ (InstrSimple (SUnopI64 IUnClz))    = B.word8 0x79
instrW _ (InstrSimple (SUnopI64 IUnCtz))    = B.word8 0x7A
instrW _ (InstrSimple (SUnopI64 IUnPopcnt)) = B.word8 0x7B

instrW _ (InstrSimple (SBinopI64 IBinAdd    )) = B.word8 0x7C
instrW _ (InstrSimple (SBinopI64 IBinSub    )) = B.word8 0x7D
instrW _ (InstrSimple (SBinopI64 IBinMul    )) = B.word8 0x7E
instrW _ (InstrSimple (SBinopI64 (IBinDiv S))) = B.word8 0x7F
instrW _ (InstrSimple (SBinopI64 (IBinDiv U))) = B.word8 0x80
instrW _ (InstrSimple (SBinopI64 (IBinRem S))) = B.word8 0x81
instrW _ (InstrSimple (SBinopI64 (IBinRem U))) = B.word8 0x82
instrW _ (InstrSimple (SBinopI64 IBinAnd    )) = B.word8 0x83
instrW _ (InstrSimple (SBinopI64 IBinOr     )) = B.word8 0x84
instrW _ (InstrSimple (SBinopI64 IBinXor    )) = B.word8 0x85
instrW _ (InstrSimple (SBinopI64 IBinShl    )) = B.word8 0x86
instrW _ (InstrSimple (SBinopI64 (IBinShr S))) = B.word8 0x87
instrW _ (InstrSimple (SBinopI64 (IBinShr U))) = B.word8 0x88
instrW _ (InstrSimple (SBinopI64 IBinRotl   )) = B.word8 0x89
instrW _ (InstrSimple (SBinopI64 IBinRotr   )) = B.word8 0x8A

instrW _ (InstrSimple (SUnopF32 FUnAbs       )) = B.word8 0x8B
instrW _ (InstrSimple (SUnopF32 FUnNeg       )) = B.word8 0x8C
instrW _ (InstrSimple (SUnopF32 FUnCeil      )) = B.word8 0x8D
instrW _ (InstrSimple (SUnopF32 FUnFloor     )) = B.word8 0x8E
instrW _ (InstrSimple (SUnopF32 FUnTrunc     )) = B.word8 0x8F
instrW _ (InstrSimple (SUnopF32 FUnNearest   )) = B.word8 0x90
instrW _ (InstrSimple (SUnopF32 FUnSqrt      )) = B.word8 0x91

instrW _ (InstrSimple (SBinopF32 FBinAdd     )) = B.word8 0x92
instrW _ (InstrSimple (SBinopF32 FBinSub     )) = B.word8 0x93
instrW _ (InstrSimple (SBinopF32 FBinMul     )) = B.word8 0x94
instrW _ (InstrSimple (SBinopF32 FBinDiv     )) = B.word8 0x95
instrW _ (InstrSimple (SBinopF32 FBinMin     )) = B.word8 0x96
instrW _ (InstrSimple (SBinopF32 FBinMax     )) = B.word8 0x97
instrW _ (InstrSimple (SBinopF32 FBinCopysign)) = B.word8 0x98

instrW _ (InstrSimple (SUnopF64 FUnAbs       )) = B.word8 0x99
instrW _ (InstrSimple (SUnopF64 FUnNeg       )) = B.word8 0x9A
instrW _ (InstrSimple (SUnopF64 FUnCeil      )) = B.word8 0x9B
instrW _ (InstrSimple (SUnopF64 FUnFloor     )) = B.word8 0x9C
instrW _ (InstrSimple (SUnopF64 FUnTrunc     )) = B.word8 0x9D
instrW _ (InstrSimple (SUnopF64 FUnNearest   )) = B.word8 0x9E
instrW _ (InstrSimple (SUnopF64 FUnSqrt      )) = B.word8 0x9F

instrW _ (InstrSimple (SBinopF64 FBinAdd     )) = B.word8 0xA0
instrW _ (InstrSimple (SBinopF64 FBinSub     )) = B.word8 0xA1
instrW _ (InstrSimple (SBinopF64 FBinMul     )) = B.word8 0xA2
instrW _ (InstrSimple (SBinopF64 FBinDiv     )) = B.word8 0xA3
instrW _ (InstrSimple (SBinopF64 FBinMin     )) = B.word8 0xA4
instrW _ (InstrSimple (SBinopF64 FBinMax     )) = B.word8 0xA5
instrW _ (InstrSimple (SBinopF64 FBinCopysign)) = B.word8 0xA6

instrW _ (InstrSimple (SCvtop CI32WrapI64       ))     = B.word8 0xA7
instrW _ (InstrSimple (SCvtop (CI32TruncF32 NoSat S))) = B.word8 0xA8
instrW _ (InstrSimple (SCvtop (CI32TruncF32 NoSat U))) = B.word8 0xA9
instrW _ (InstrSimple (SCvtop (CI32TruncF64 NoSat S))) = B.word8 0xAA
instrW _ (InstrSimple (SCvtop (CI32TruncF64 NoSat U))) = B.word8 0xAB
instrW _ (InstrSimple (SCvtop (CI64ExtendI32 S) ))     = B.word8 0xAC
instrW _ (InstrSimple (SCvtop (CI64ExtendI32 U) ))     = B.word8 0xAD
instrW _ (InstrSimple (SCvtop (CI64TruncF32 NoSat S))) = B.word8 0xAE
instrW _ (InstrSimple (SCvtop (CI64TruncF32 NoSat U))) = B.word8 0xAF
instrW _ (InstrSimple (SCvtop (CI64TruncF64 NoSat S))) = B.word8 0xB0
instrW _ (InstrSimple (SCvtop (CI64TruncF64 NoSat U))) = B.word8 0xB1
instrW _ (InstrSimple (SCvtop (CF32ConvertI32 S)))     = B.word8 0xB2
instrW _ (InstrSimple (SCvtop (CF32ConvertI32 U)))     = B.word8 0xB3
instrW _ (InstrSimple (SCvtop (CF32ConvertI64 S)))     = B.word8 0xB4
instrW _ (InstrSimple (SCvtop (CF32ConvertI64 U)))     = B.word8 0xB5
instrW _ (InstrSimple (SCvtop CF32DemoteF64     ))     = B.word8 0xB6
instrW _ (InstrSimple (SCvtop (CF64ConvertI32 S)))     = B.word8 0xB7
instrW _ (InstrSimple (SCvtop (CF64ConvertI32 U)))     = B.word8 0xB8
instrW _ (InstrSimple (SCvtop (CF64ConvertI64 S)))     = B.word8 0xB9
instrW _ (InstrSimple (SCvtop (CF64ConvertI64 U)))     = B.word8 0xBA
instrW _ (InstrSimple (SCvtop CF64PromoteF32    ))     = B.word8 0xBB
instrW _ (InstrSimple (SCvtop CI32ReinterpretF32))     = B.word8 0xBC
instrW _ (InstrSimple (SCvtop CI64ReinterpretF64))     = B.word8 0xBD
instrW _ (InstrSimple (SCvtop CF32ReinterpretI32))     = B.word8 0xBE
instrW _ (InstrSimple (SCvtop CF64ReinterpretI64))     = B.word8 0xBF

instrW _ (InstrSimple (SCvtop CI32Extend8S )) = B.word8 0xC0
instrW _ (InstrSimple (SCvtop CI32Extend16S)) = B.word8 0xC1
instrW _ (InstrSimple (SCvtop CI64Extend8S )) = B.word8 0xC2
instrW _ (InstrSimple (SCvtop CI64Extend16S)) = B.word8 0xC3
instrW _ (InstrSimple (SCvtop CI64Extend32S)) = B.word8 0xC4

instrW _ (InstrSimple (SCvtop (CI32TruncF32 Sat S))) = B.word8 0xFC <> B.word8 0x00
instrW _ (InstrSimple (SCvtop (CI32TruncF32 Sat U))) = B.word8 0xFC <> B.word8 0x01
instrW _ (InstrSimple (SCvtop (CI32TruncF64 Sat S))) = B.word8 0xFC <> B.word8 0x02
instrW _ (InstrSimple (SCvtop (CI32TruncF64 Sat U))) = B.word8 0xFC <> B.word8 0x03
instrW _ (InstrSimple (SCvtop (CI64TruncF32 Sat S))) = B.word8 0xFC <> B.word8 0x04
instrW _ (InstrSimple (SCvtop (CI64TruncF32 Sat U))) = B.word8 0xFC <> B.word8 0x05
instrW _ (InstrSimple (SCvtop (CI64TruncF64 Sat S))) = B.word8 0xFC <> B.word8 0x06
instrW _ (InstrSimple (SCvtop (CI64TruncF64 Sat U))) = B.word8 0xFC <> B.word8 0x07

exprW :: IdHashSet FuncType -> Expr -> Builder
exprW fts xs = mconcat (map (instrW fts) xs) <> B.word8 0x0B
