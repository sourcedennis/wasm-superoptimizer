
-- | Peephole optimization that cleans up dropped results. Effectively
-- transition chain compression.
module Lang.Wasm.Optimizations.PullDrop
  ( pullDrop
  ) where

-- Extra stdlib imports
import qualified Data.Vector as Vector
-- Local library imports
import Lang.Wasm.Ast


pullDrop :: Module -> Module
pullDrop m = m { mFuncsDefined = Vector.map pullDropFunc (mFuncsDefined m) }

pullDropFunc :: DefinedFunc -> DefinedFunc
pullDropFunc d = DefinedFunc (fType d) (fLocals d) (dropInstrsFix $ fBody d)

dropInstrsFix :: [Instr] -> [Instr]
dropInstrsFix xs =
  -- Try to remove drop instructions, and repeat until it converges.
  let ys = dropInstrs xs
  in
  if xs == ys then
    ys
  else
    dropInstrsFix ys

--
-- TODO: View patterns
dropInstrs :: [Instr] -> [Instr]
dropInstrs (InstrNop:xs) = dropInstrs xs
dropInstrs (InstrSimple (SConstI32 _):InstrSimple (SPrmInstrI32 PDrop):xs) =
  dropInstrs xs
dropInstrs (InstrSimple (SConstI64 _):InstrSimple (SPrmInstrI64 PDrop):xs) =
  dropInstrs xs
dropInstrs (InstrSimple (SConstF32 _):InstrSimple (SPrmInstrF32 PDrop):xs) =
  dropInstrs xs
dropInstrs (InstrSimple (SConstF64 _):InstrSimple (SPrmInstrF64 PDrop):xs) =
  dropInstrs xs
dropInstrs (InstrSimple (SUnopI32 _):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SUnopI64 _):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SBinopI32 _):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SBinopI64 _):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI64 PDrop):InstrSimple (SPrmInstrI64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SUnopF32 _):InstrSimple (SPrmInstrF32 PDrop):xs) =
  InstrSimple (SPrmInstrF32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SUnopF64 _):InstrSimple (SPrmInstrF64 PDrop):xs) =
  InstrSimple (SPrmInstrF64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SBinopF32 _):InstrSimple (SPrmInstrF32 PDrop):xs) =
  InstrSimple (SPrmInstrF32 PDrop):InstrSimple (SPrmInstrF32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SBinopF64 _):InstrSimple (SPrmInstrF64 PDrop):xs) =
  InstrSimple (SPrmInstrF64 PDrop):InstrSimple (SPrmInstrF64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (STestopI32 ITestEqz):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (STestopI64 ITestEqz):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrI64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SRelopI32 _):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SRelopI64 _):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI64 PDrop):InstrSimple (SPrmInstrI64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SRelopF32 _):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrF32 PDrop):InstrSimple (SPrmInstrF32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SRelopF64 _):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrF64 PDrop):InstrSimple (SPrmInstrF64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SPrmInstrI32 PSelect):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):InstrSimple (SPrmInstrI32 PDrop):InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SPrmInstrI64 PSelect):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):InstrSimple (SPrmInstrI64 PDrop):InstrSimple (SPrmInstrI64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SPrmInstrF32 PSelect):InstrSimple (SPrmInstrF32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):InstrSimple (SPrmInstrF32 PDrop):InstrSimple (SPrmInstrF32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SPrmInstrF64 PSelect):InstrSimple (SPrmInstrF64 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):InstrSimple (SPrmInstrF64 PDrop):InstrSimple (SPrmInstrF64 PDrop):dropInstrs xs

dropInstrs (InstrSimple (SVarInstr (VLocalGet _)):InstrSimple (SPrmInstrI32 PDrop):xs) = dropInstrs xs
dropInstrs (InstrSimple (SVarInstr (VLocalGet _)):InstrSimple (SPrmInstrI64 PDrop):xs) = dropInstrs xs
dropInstrs (InstrSimple (SVarInstr (VLocalGet _)):InstrSimple (SPrmInstrF32 PDrop):xs) = dropInstrs xs
dropInstrs (InstrSimple (SVarInstr (VLocalGet _)):InstrSimple (SPrmInstrF64 PDrop):xs) = dropInstrs xs
dropInstrs (InstrSimple (SVarInstr (VLocalTee idx)):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SVarInstr (VLocalSet idx)):dropInstrs xs
dropInstrs (InstrSimple (SVarInstr (VLocalTee idx)):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SVarInstr (VLocalSet idx)):dropInstrs xs
dropInstrs (InstrSimple (SVarInstr (VLocalTee idx)):InstrSimple (SPrmInstrF32 PDrop):xs) =
  InstrSimple (SVarInstr (VLocalSet idx)):dropInstrs xs
dropInstrs (InstrSimple (SVarInstr (VLocalTee idx)):InstrSimple (SPrmInstrF64 PDrop):xs) =
  InstrSimple (SVarInstr (VLocalSet idx)):dropInstrs xs
  
dropInstrs (InstrSimple (SVarInstr (VGlobalGet _)):InstrSimple (SPrmInstrI32 PDrop):xs) = dropInstrs xs
dropInstrs (InstrSimple (SVarInstr (VGlobalGet _)):InstrSimple (SPrmInstrI64 PDrop):xs) = dropInstrs xs
dropInstrs (InstrSimple (SVarInstr (VGlobalGet _)):InstrSimple (SPrmInstrF32 PDrop):xs) = dropInstrs xs
dropInstrs (InstrSimple (SVarInstr (VGlobalGet _)):InstrSimple (SPrmInstrF64 PDrop):xs) = dropInstrs xs

dropInstrs (InstrSimple (SMemInstr (MI32Load _)):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SMemInstr (MI64Load _)):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SMemInstr (MF32Load _)):InstrSimple (SPrmInstrF32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SMemInstr (MF64Load _)):InstrSimple (SPrmInstrF64 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
  
dropInstrs (InstrSimple (SMemInstr (MI32Load8 _ _)):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SMemInstr (MI32Load16 _ _)):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SMemInstr (MI64Load8 _ _)):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SMemInstr (MI64Load16 _ _)):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SMemInstr (MI64Load32 _ _)):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SMemInstr MMemorySize):InstrSimple (SPrmInstrI32 PDrop):xs) =
  dropInstrs xs

dropInstrs (InstrSimple (SCvtop CI32Extend8S):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop CI32Extend16S):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop CI64Extend8S):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrI64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop CI64Extend16S):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrI64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop CI64Extend32S):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrI64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop CI32WrapI64):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrI64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop (CI64ExtendI32 _)):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop (CI32TruncF32 _ _)):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrF32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop (CI32TruncF64 _ _)):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrF64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop (CI64TruncF32 _ _)):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrF32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop (CI64TruncF64 _ _)):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrF64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop CF32DemoteF64):InstrSimple (SPrmInstrF32 PDrop):xs) =
  InstrSimple (SPrmInstrF64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop CF64PromoteF32):InstrSimple (SPrmInstrF64 PDrop):xs) =
  InstrSimple (SPrmInstrF32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop (CF32ConvertI32 _)):InstrSimple (SPrmInstrF32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop (CF32ConvertI64 _)):InstrSimple (SPrmInstrF32 PDrop):xs) =
  InstrSimple (SPrmInstrI64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop (CF64ConvertI32 _)):InstrSimple (SPrmInstrF64 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop (CF64ConvertI64 _)):InstrSimple (SPrmInstrF64 PDrop):xs) =
  InstrSimple (SPrmInstrI64 PDrop):dropInstrs xs
  
dropInstrs (InstrSimple (SCvtop CI32ReinterpretF32):InstrSimple (SPrmInstrI32 PDrop):xs) =
  InstrSimple (SPrmInstrF32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop CF32ReinterpretI32):InstrSimple (SPrmInstrF32 PDrop):xs) =
  InstrSimple (SPrmInstrI32 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop CI64ReinterpretF64):InstrSimple (SPrmInstrI64 PDrop):xs) =
  InstrSimple (SPrmInstrF64 PDrop):dropInstrs xs
dropInstrs (InstrSimple (SCvtop CF64ReinterpretI64):InstrSimple (SPrmInstrF64 PDrop):xs) =
  InstrSimple (SPrmInstrI64 PDrop):dropInstrs xs
dropInstrs (InstrUnreachable:xs) = [InstrUnreachable]
dropInstrs (InstrBlock ft xs:zs) =
  InstrBlock ft (dropInstrs xs) : dropInstrs zs
dropInstrs (InstrLoop ft xs:zs) =
  InstrLoop ft (dropInstrs xs) : dropInstrs zs
dropInstrs (InstrIf ft xs ys:zs) =
  InstrIf ft (dropInstrs xs) (dropInstrs ys) : dropInstrs zs
dropInstrs (InstrBr lIdx:zs) = [InstrBr lIdx]
dropInstrs (InstrBrIf lIdx:zs) = InstrBrIf lIdx : dropInstrs zs
dropInstrs (InstrBrTable lIdxs lIdx:zs) =
  InstrBrTable lIdxs lIdx: dropInstrs zs
dropInstrs (InstrReturn:zs) = [InstrReturn]
dropInstrs (InstrCall fIdx:zs) = InstrCall fIdx : dropInstrs zs
dropInstrs (InstrCallIndirect ft:zs) = InstrCallIndirect ft : dropInstrs zs
dropInstrs (x:xs) = x : dropInstrs xs
dropInstrs [] = []
