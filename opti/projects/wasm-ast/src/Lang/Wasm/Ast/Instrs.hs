{-# LANGUAGE DeriveGeneric, UnicodeSyntax, StrictData #-}

  
-- |
--
-- Boolean interpretation function:
-- bool(C) = 1  (if C)
-- bool(C) = 0  (otherwise)
-- 
-- The type `bool32` in this document describes a bitvector of length 32 that
--   can have either 0 or 1 as its value.
module Lang.Wasm.Ast.Instrs where

import Melude
-- Stdlib imports
import GHC.Generics (Generic)
import Control.DeepSeq ( NFData )
import Data.Hashable (Hashable)
import Numeric (showHex)
-- Local imports
import Lang.Wasm.Ast.Indices ( LabelIdx, FuncIdx, TypeIdx, GlobalIdx (..), LocalIdx (..) )
import Lang.Wasm.Ast.Types ( ValType, FuncType )
import Lang.Wasm.Ast.Values ( WI32 (..), WI64 (..), WF32 (..), WF64 (..) )


-- | Argument for memory instructions
data MemArg =
  MemArg {
    maOffset  :: !Int
  , maAlign   :: !Int -- Alignment is 2^align
  }
  deriving (Eq, Show, Generic)

-- | The function-type of a block.
type BlockType = FuncType

-- | Marks whether an instruction is signed or unsigned.
data Sx = U | S
  deriving (Eq, Show, Generic)

-- | `True` iff the float conversion is /saturating/. Saturating float-to-int
-- conversion does /not/ trap when exceeding the int bounds, but clamps the
-- value within the bounds of the target type.
data Sat = Sat | NoSat
  deriving (Eq, Show, Generic)

-- | Unary integer operator. Takes one value of an int-type, and produces
-- another value of that type.
--
-- The type parameter acts only as a marker which prevents accidental type
-- coercions.
data IUnop a
  = IUnClz
  | IUnCtz
  | IUnPopcnt
  deriving (Eq, Show, Generic)

-- | Binary integer operator. Takes two values of an int-type, and produces
-- another value of that type.
--
-- The type parameter acts only as a marker which prevents accidental type
-- coercions.
data IBinop a
  = IBinAdd
  | IBinSub
  | IBinMul
  | IBinDiv !Sx
  | IBinRem !Sx
  | IBinAnd
  | IBinOr
  | IBinXor
  | IBinShl
  | IBinShr !Sx
  | IBinRotl
  | IBinRotr
  deriving (Eq, Show, Generic)

-- | Unary floating-point operator. Takes one value of a float-type, and
-- produces another value of that type.
--
-- The type parameter acts only as a marker which prevents accidental type
-- coercions.
data FUnop a
  = FUnAbs
  | FUnNeg
  | FUnSqrt
  | FUnCeil
  | FUnFloor
  | FUnTrunc
  | FUnNearest
  deriving (Eq, Show, Generic)

-- | Binary floating-point operator. Takes two values of a float-type, and
-- produces another value of that type.
--
-- The type parameter acts only as a marker which prevents accidental type
-- coercions.
data FBinop a
  = FBinAdd
  | FBinSub
  | FBinMul
  | FBinDiv
  | FBinMin
  | FBinMax
  | FBinCopysign
  deriving (Eq, Show, Generic)

-- | Integer test operator. Takes one value of an int-type, and produces a
-- @bool32@ value.
--
-- The type parameter acts only as a marker which prevents accidental type
-- coercions.
data ITestop a
  = ITestEqz
  deriving (Eq, Show, Generic)

-- | Integer relation operator. Takes two values of an int-type, and produces a
-- value of type @bool32@.
--
-- The type parameter acts only as a marker which prevents accidental type
-- coercions.
data IRelop a
  = IRelEq
  | IRelNe
  | IRelLt !Sx
  | IRelGt !Sx
  | IRelLe !Sx
  | IRelGe !Sx
  deriving (Eq, Show, Generic)

-- | Floating point relation operator. Takes two values of a float-type, and
-- produces a value of type @bool32@.
--
-- The type parameter acts only as a marker which prevents accidental type
-- coercions.
data FRelop a
  = FRelEq
  | FRelNe
  | FRelLt
  | FRelGt
  | FRelLe
  | FRelGe
  deriving (Eq, Show, Generic)

-- | Conversion operators
data Cvtop
  = CI32Extend8S
  | CI32Extend16S
  | CI64Extend8S
  | CI64Extend16S
  | CI64Extend32S
  | CI32WrapI64
  | CI64ExtendI32 !Sx
  | CI32TruncF32 !Sat !Sx
  | CI32TruncF64 !Sat !Sx
  | CI64TruncF32 !Sat !Sx
  | CI64TruncF64 !Sat !Sx
  | CF32DemoteF64
  | CF64PromoteF32
  | CF32ConvertI32 !Sx
  | CF32ConvertI64 !Sx
  | CF64ConvertI32 !Sx
  | CF64ConvertI64 !Sx
  | CI32ReinterpretF32
  | CI64ReinterpretF64
  | CF32ReinterpretI32
  | CF64ReinterpretI64
  deriving (Eq, Show, Generic)

-- | Parametric instructions. These are polymorphic to their values
-- Drop   :: forall a . a -> ()
-- Select :: forall a . a -> a -> bool32 -> a
data PrmInstr a
  = PDrop
  | PSelect
  deriving (Eq, Show, Generic)

-- | Variable instructions
data VarInstr
  = VLocalGet !LocalIdx
  | VLocalSet !LocalIdx
  | VLocalTee !LocalIdx
  | VGlobalGet !GlobalIdx
  | VGlobalSet !GlobalIdx
  deriving (Eq, Show, Generic)

-- | Memory instructions
data MemInstr
  = MI32Load !MemArg
  | MI64Load !MemArg
  | MF32Load !MemArg
  | MF64Load !MemArg
  | MI32Store !MemArg
  | MI64Store !MemArg
  | MF32Store !MemArg
  | MF64Store !MemArg
  | MI32Load8 !Sx !MemArg
  | MI64Load8 !Sx !MemArg
  | MI32Load16 !Sx !MemArg
  | MI64Load16 !Sx !MemArg
  | MI64Load32 !Sx !MemArg
  | MI32Store8 !MemArg
  | MI64Store8 !MemArg
  | MI32Store16 !MemArg
  | MI64Store16 !MemArg
  | MI64Store32 !MemArg
  | MMemorySize
  | MMemoryGrow
  deriving (Eq, Show, Generic)

-- | Simple instructions. These perform elementary operations; and thus include
-- no control flow.
data SimpleInstr
  = SConstI32 !WI32
  | SConstI64 !WI64
  | SConstF32 !WF32
  | SConstF64 !WF64
  | SUnopI32 !(IUnop WI32)
  | SUnopI64 !(IUnop WI64)
  | SBinopI32 !(IBinop WI32)
  | SBinopI64 !(IBinop WI64)
  | SUnopF32 !(FUnop WF32)
  | SUnopF64 !(FUnop WF64)
  | SBinopF32 !(FBinop WF32)
  | SBinopF64 !(FBinop WF64)
  | STestopI32 !(ITestop WI32)
  | STestopI64 !(ITestop WI64)
  | SRelopI32 !(IRelop WI32)
  | SRelopI64 !(IRelop WI64)
  | SRelopF32 !(FRelop WF32)
  | SRelopF64 !(FRelop WF64)
  | SCvtop !Cvtop
  | SPrmInstrI32 !(PrmInstr WI32)
  | SPrmInstrI64 !(PrmInstr WI64)
  | SPrmInstrF32 !(PrmInstr WF32)
  | SPrmInstrF64 !(PrmInstr WF64)
  | SVarInstr !VarInstr
  | SMemInstr !MemInstr
  deriving (Eq, Generic)

-- | Any instruction. Function types are embedded in the instruction instead
-- of referenced in a global table.
data Instr
  = InstrNop
  | InstrUnreachable
  | InstrSimple !SimpleInstr
  | InstrBlock !FuncType ![Instr]
  | InstrLoop !FuncType ![Instr]
  | InstrIf !FuncType [Instr] ![Instr]
  | InstrBr !LabelIdx
  | InstrBrIf !LabelIdx
  | InstrBrTable ![LabelIdx] !LabelIdx
  | InstrReturn
  | InstrCall !FuncIdx
  | InstrCallIndirect !FuncType
  deriving (Eq, Show, Generic)

-- | A sequence of instructions
type Expr = [Instr]

instance Hashable MemArg
instance Hashable Sx
instance Hashable (IUnop a)
instance Hashable (IBinop a)
instance Hashable (FUnop a)
instance Hashable (FBinop a)
instance Hashable (ITestop a)
instance Hashable (IRelop a)
instance Hashable (FRelop a)
instance Hashable Sat
instance Hashable Cvtop
instance Hashable (PrmInstr a)
instance Hashable VarInstr
instance Hashable MemInstr
instance Hashable SimpleInstr
instance Hashable Instr

instance NFData MemArg
instance NFData Sx
instance NFData (IUnop a)
instance NFData (IBinop a)
instance NFData (FUnop a)
instance NFData (FBinop a)
instance NFData (ITestop a)
instance NFData (IRelop a)
instance NFData (FRelop a)
instance NFData Sat
instance NFData Cvtop
instance NFData (PrmInstr a)
instance NFData VarInstr
instance NFData MemInstr
instance NFData SimpleInstr
instance NFData Instr


-- # Show instances

-- | Application precedence for showing
appPrec :: Int
appPrec = 10

instance Show SimpleInstr where
  showsPrec d (SConstI32 (WI32 i))  = showParen (d > appPrec) (ß "i32.const " . showsPrec (appPrec+1) i)
  showsPrec d (SConstI64 (WI64 i))  = showParen (d > appPrec) (ß "i64.const " . showsPrec (appPrec+1) i)
  showsPrec d (SConstF32 (WF32 i))  = showParen (d > appPrec) (ß "f32.const 0x" . showHex i)
  showsPrec d (SConstF64 (WF64 i))  = showParen (d > appPrec) (ß "f64.const 0x" . showHex i)
  showsPrec _ (SUnopI32 a)          = ß "i32." . showIUnop a
  showsPrec _ (SUnopI64 a)          = ß "i64." . showIUnop a
  showsPrec _ (SBinopI32 a)         = ß "i32." . showIBinop a
  showsPrec _ (SBinopI64 a)         = ß "i64." . showIBinop a
  showsPrec _ (SUnopF32 a)          = ß "f32." . showFUnop a
  showsPrec _ (SUnopF64 a)          = ß "f64." . showFUnop a
  showsPrec _ (SBinopF32 a)         = ß "f32." . showFBinop a
  showsPrec _ (SBinopF64 a)         = ß "f64." . showFBinop a
  showsPrec _ (STestopI32 ITestEqz) = ß "i32.eqz"
  showsPrec _ (STestopI64 ITestEqz) = ß "i64.eqz"
  showsPrec _ (SRelopI32 a)         = ß "i32." . showIRelop a
  showsPrec _ (SRelopI64 a)         = ß "i64." . showIRelop a
  showsPrec _ (SRelopF32 a)         = ß "f32." . showFRelop a
  showsPrec _ (SRelopF64 a)         = ß "f64." . showFRelop a
  showsPrec _ (SCvtop a)            = showCvtop a
  showsPrec _ (SPrmInstrI32 a)      = showPrmop a
  showsPrec _ (SPrmInstrI64 a)      = showPrmop a
  showsPrec _ (SPrmInstrF32 a)      = showPrmop a
  showsPrec _ (SPrmInstrF64 a)      = showPrmop a
  showsPrec d (SVarInstr a)         = showVarInstr d a
  showsPrec d (SMemInstr a)         = showMemInstr d a

showIUnop :: IUnop a -> ShowS
showIUnop IUnClz    = ß "clz"
showIUnop IUnCtz    = ß "ctz"
showIUnop IUnPopcnt = ß "popcnt"

showIBinop :: IBinop a -> ShowS
showIBinop IBinAdd     = ß "add"
showIBinop IBinSub     = ß "sub"
showIBinop IBinMul     = ß "mul"
showIBinop (IBinDiv U) = ß "div_u"
showIBinop (IBinDiv S) = ß "div_s"
showIBinop (IBinRem U) = ß "rem_u"
showIBinop (IBinRem S) = ß "rem_s"
showIBinop IBinAnd     = ß "and"
showIBinop IBinOr      = ß "or"
showIBinop IBinXor     = ß "xor"
showIBinop IBinShl     = ß "shl"
showIBinop (IBinShr U) = ß "shr_u"
showIBinop (IBinShr S) = ß "shr_s"
showIBinop IBinRotl    = ß "rotl"
showIBinop IBinRotr    = ß "rotr"

showFUnop :: FUnop a -> ShowS
showFUnop FUnAbs     = ß "abs"
showFUnop FUnNeg     = ß "neg"
showFUnop FUnSqrt    = ß "sqrt"
showFUnop FUnCeil    = ß "ceil"
showFUnop FUnFloor   = ß "floor"
showFUnop FUnTrunc   = ß "trunc"
showFUnop FUnNearest = ß "nearest"

showFBinop :: FBinop a -> ShowS
showFBinop FBinAdd      = ß "add"
showFBinop FBinSub      = ß "sub"
showFBinop FBinMul      = ß "mul"
showFBinop FBinDiv      = ß "div"
showFBinop FBinMin      = ß "min"
showFBinop FBinMax      = ß "max"
showFBinop FBinCopysign = ß "copysign"

showIRelop :: IRelop a -> ShowS
showIRelop IRelEq      = ß "eq"
showIRelop IRelNe      = ß "ne"
showIRelop (IRelLt sx) = ß "lt_" . showSx sx
showIRelop (IRelGt sx) = ß "gt_" . showSx sx
showIRelop (IRelLe sx) = ß "le_" . showSx sx
showIRelop (IRelGe sx) = ß "ge_" . showSx sx

showFRelop :: FRelop a -> ShowS
showFRelop FRelEq = ß "eq"
showFRelop FRelNe = ß "ne"
showFRelop FRelLt = ß "lt"
showFRelop FRelGt = ß "gt"
showFRelop FRelLe = ß "le"
showFRelop FRelGe = ß "ge"

showCvtop :: Cvtop -> ShowS
showCvtop CI32Extend8S          = ß "i32.extend8_s"
showCvtop CI32Extend16S         = ß "i32.extend16_s"
showCvtop CI64Extend8S          = ß "i64.extend8_s"
showCvtop CI64Extend16S         = ß "i64.extend16_s"
showCvtop CI64Extend32S         = ß "i64.extend32_s"
showCvtop CI32WrapI64           = ß "i32.wrap_i64"
showCvtop (CI64ExtendI32 sx)    = ß "i64.extend_i32_" . showSx sx
showCvtop (CI32TruncF32 sat sx) = ß "i32.trunc_" . showSat sat . ß "f32_" . showSx sx
showCvtop (CI32TruncF64 sat sx) = ß "i32.trunc_" . showSat sat . ß "f64_" . showSx sx
showCvtop (CI64TruncF32 sat sx) = ß "i64.trunc_" . showSat sat . ß "f32_" . showSx sx
showCvtop (CI64TruncF64 sat sx) = ß "i64.trunc_" . showSat sat . ß "f64_" . showSx sx
showCvtop CF32DemoteF64         = ß "f32.demote_f64"
showCvtop CF64PromoteF32        = ß "f64.promote_f32"
showCvtop (CF32ConvertI32 sx)   = ß "f32.convert_i32_" . showSx sx
showCvtop (CF32ConvertI64 sx)   = ß "f32.convert_i64_" . showSx sx
showCvtop (CF64ConvertI32 sx)   = ß "f64.convert_i32_" . showSx sx
showCvtop (CF64ConvertI64 sx)   = ß "f64.convert_i64_" . showSx sx
showCvtop CI32ReinterpretF32    = ß "i32.reinterpret_f32"
showCvtop CI64ReinterpretF64    = ß "i64.reinterpret_f64"
showCvtop CF32ReinterpretI32    = ß "f32.reinterpret_i32"
showCvtop CF64ReinterpretI64    = ß "f64.reinterpret_i64"

showPrmop :: PrmInstr a -> ShowS
showPrmop PDrop   = ß "drop"
showPrmop PSelect = ß "select"

showVarInstr :: Int -> VarInstr -> ShowS
showVarInstr d (VLocalGet (LocalIdx i))   = showParen (d > appPrec) (ß "local.get " . shows i)
showVarInstr d (VLocalSet (LocalIdx i))   = showParen (d > appPrec) (ß "local.set " . shows i)
showVarInstr d (VLocalTee (LocalIdx i))   = showParen (d > appPrec) (ß "local.tee " . shows i)
showVarInstr d (VGlobalGet (GlobalIdx i)) = showParen (d > appPrec) (ß "global.get " . shows i)
showVarInstr d (VGlobalSet (GlobalIdx i)) = showParen (d > appPrec) (ß "global.set " . shows i)

showMemInstr :: Int -> MemInstr -> ShowS
showMemInstr d (MI32Load ma)      = showParenJust (d > appPrec) (ß "i32.load") $ showMemArg ma
showMemInstr d (MI64Load ma)      = showParenJust (d > appPrec) (ß "i64.load") $ showMemArg ma
showMemInstr d (MF32Load ma)      = showParenJust (d > appPrec) (ß "f32.load") $ showMemArg ma
showMemInstr d (MF64Load ma)      = showParenJust (d > appPrec) (ß "f64.load") $ showMemArg ma
showMemInstr d (MI32Store ma)     = showParenJust (d > appPrec) (ß "i32.store") $ showMemArg ma
showMemInstr d (MI64Store ma)     = showParenJust (d > appPrec) (ß "i64.store") $ showMemArg ma
showMemInstr d (MF32Store ma)     = showParenJust (d > appPrec) (ß "f32.store") $ showMemArg ma
showMemInstr d (MF64Store ma)     = showParenJust (d > appPrec) (ß "f64.store") $ showMemArg ma
showMemInstr d (MI32Load8 sx ma)  = showParenJust (d > appPrec) (ß "i32.load8_"  . showSx sx) $ showMemArg ma
showMemInstr d (MI64Load8 sx ma)  = showParenJust (d > appPrec) (ß "i64.load8_"  . showSx sx) $ showMemArg ma
showMemInstr d (MI32Load16 sx ma) = showParenJust (d > appPrec) (ß "i32.load16_" . showSx sx) $ showMemArg ma
showMemInstr d (MI64Load16 sx ma) = showParenJust (d > appPrec) (ß "i64.load16_" . showSx sx) $ showMemArg ma
showMemInstr d (MI64Load32 sx ma) = showParenJust (d > appPrec) (ß "i64.load32_" . showSx sx) $ showMemArg ma
showMemInstr d (MI32Store8 ma)    = showParenJust (d > appPrec) (ß "i32.store8")$ showMemArg ma
showMemInstr d (MI64Store8 ma)    = showParenJust (d > appPrec) (ß "i64.store8")$ showMemArg ma
showMemInstr d (MI32Store16 ma)   = showParenJust (d > appPrec) (ß "i32.store16") $ showMemArg ma
showMemInstr d (MI64Store16 ma)   = showParenJust (d > appPrec) (ß "i64.store16") $ showMemArg ma
showMemInstr d (MI64Store32 ma)   = showParenJust (d > appPrec) (ß "i64.store32") $ showMemArg ma
showMemInstr _ MMemorySize        = ß "memory.size"
showMemInstr _ MMemoryGrow        = ß "memory.grow"

showSat :: Sat -> ShowS
showSat Sat   = ß "sat_"
showSat NoSat = id

showSx :: Sx -> ShowS
showSx S = ß "s"
showSx U = ß "u"

showMemArg :: MemArg -> Maybe ShowS
showMemArg ma =
  let off  = toMaybe (maOffset ma > 0) (ß " offset=" . shows (maOffset ma))
      algn = toMaybe (maAlign ma /= 0) (ß " align=" . shows (2 ^ maAlign ma))
  in
  combine (.) off algn

-- | Don't show the parentheses when `Nothing`.
showParenJust :: Bool -> ShowS -> Maybe ShowS -> ShowS
showParenJust _ x Nothing  = x
showParenJust b x (Just y) = showParen b (x . y)
