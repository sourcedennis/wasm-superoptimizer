
-- | A simple cost function for simple instructions. These approximate
-- execution time.
--
-- The costs are rougly modeled along (with a multiplication factor):
-- <https://www.agner.org/optimize/instruction_tables.pdf>
module Lang.Wasm.InstrCost
  ( Cost
  , simpleCost
  , instrCost
  ) where

-- Local library imports
import Lang.Wasm.Ast
  ( SimpleInstr (..)
  , Cvtop (..), MemInstr (..), VarInstr (..)
  , PrmInstr (..), IUnop (..), IBinop (..), FUnop (..), FBinop (..)
  , ITestop (..), IRelop (..), FRelop (..), Instr (..)
  )

type Cost = Int

instrCost :: Instr -> Cost
instrCost (InstrSimple x)       = simpleCost x
instrCost InstrNop              = 1
instrCost InstrUnreachable      = 2
instrCost (InstrBlock _ xs)     = 10 + sum (map instrCost xs)
instrCost (InstrLoop _ xs)      = 10 + 16 * sum (map instrCost xs)
instrCost (InstrIf _ xs ys)     =
  -- Averages over both branches
  10 + sum (map instrCost xs) `div` 2 + 10 + sum (map instrCost ys) `div` 2
instrCost (InstrBr _)           = 40
instrCost (InstrBrIf _)         = 80
instrCost (InstrBrTable _ _)    = 80
instrCost InstrReturn           = 10
instrCost (InstrCall _)         = 200
instrCost (InstrCallIndirect _) = 200


-- | An approximate cost model for WebAssembly instructions.
--
-- The returned unit arbitrary, but satisfying a ratio.
-- E.g., @ instrCost (SBinopI32 IBinMul) = 15 @, while
-- @ instrCost (SBinopI32 IBinAdd) = 5 @; this indicates that multiplication takes
-- 3 times as long as addition. The 15 and 5 are otherwise arbitrary.
simpleCost :: SimpleInstr -> Cost
simpleCost (SConstI32 _)     = 5
simpleCost (SConstI64 _)     = 5
simpleCost (SConstF32 _)     = 5
simpleCost (SConstF64 _)     = 5
simpleCost (SUnopI32 op)     = costIUnop op
simpleCost (SUnopI64 op)     = costIUnop op
simpleCost (SBinopI32 op)    = costIBinop op
simpleCost (SBinopI64 op)    = costIBinop op
simpleCost (SUnopF32 op)     = costFUnop op
simpleCost (SUnopF64 op)     = costFUnop op
simpleCost (SBinopF32 op)    = costFBinop op
simpleCost (SBinopF64 op)    = costFBinop op
simpleCost (STestopI32 op)   = costITestop op
simpleCost (STestopI64 op)   = costITestop op
simpleCost (SRelopI32 op)    = costIRelop op
simpleCost (SRelopI64 op)    = costIRelop op
simpleCost (SRelopF32 op)    = costFRelop op
simpleCost (SRelopF64 op)    = costFRelop op
simpleCost (SCvtop op)       = costCvtop op
simpleCost (SPrmInstrI32 op) = costPrmInstr op
simpleCost (SPrmInstrI64 op) = costPrmInstr op
simpleCost (SPrmInstrF32 op) = costPrmInstr op
simpleCost (SPrmInstrF64 op) = costPrmInstr op
simpleCost (SVarInstr op)    = costVarInstr op
simpleCost (SMemInstr op)    = costMemInstr op

-- # Int Operators #
-- This assumes 32-bit instructions are similarly expensive to 64-bit
-- instructions.

-- IUnClz, IUnCtz, IUnPopcnt
costIUnop :: IUnop a -> Cost
costIUnop _ = 5

costIBinop :: IBinop a -> Cost
costIBinop IBinAdd     = 5
costIBinop IBinSub     = 5
costIBinop IBinMul     = 15
costIBinop (IBinDiv _) = 80
costIBinop (IBinRem _) = 80
costIBinop IBinAnd     = 3
costIBinop IBinOr      = 3
costIBinop IBinXor     = 3
costIBinop IBinShl     = 3
costIBinop (IBinShr _) = 3
costIBinop IBinRotl    = 3
costIBinop IBinRotr    = 3

costITestop :: ITestop a -> Cost
costITestop ITestEqz = 3

costIRelop :: IRelop a -> Cost
costIRelop _ = 6


-- # Float Operators #
-- This assumes 32-bit instructions are similarly expensive to 64-bit
-- instructions.

costFUnop :: FUnop a -> Cost
costFUnop FUnAbs     = 10
costFUnop FUnNeg     = 10
costFUnop FUnSqrt    = 175
costFUnop FUnCeil    = 15
costFUnop FUnFloor   = 15
costFUnop FUnTrunc   = 15
costFUnop FUnNearest = 15

costFBinop :: FBinop a -> Cost
costFBinop FBinAdd      = 20
costFBinop FBinSub      = 20
costFBinop FBinMul      = 20
costFBinop FBinDiv      = 80
costFBinop FBinMin      = 25
costFBinop FBinMax      = 25
costFBinop FBinCopysign = 20

costFRelop :: FRelop a -> Cost
costFRelop _ = 10


-- # Other Instructions #

costCvtop :: Cvtop -> Cost
costCvtop CI32Extend8S       = 5
costCvtop CI32Extend16S      = 5
costCvtop CI64Extend8S       = 5
costCvtop CI64Extend16S      = 5
costCvtop CI64Extend32S      = 5
costCvtop CI32WrapI64        = 5
costCvtop (CI64ExtendI32 _)  = 5
costCvtop (CI32TruncF32 _ _) = 20
costCvtop (CI32TruncF64 _ _) = 20
costCvtop (CI64TruncF32 _ _) = 20
costCvtop (CI64TruncF64 _ _) = 20
costCvtop CF32DemoteF64      = 20
costCvtop CF64PromoteF32     = 20
costCvtop (CF32ConvertI32 _) = 20
costCvtop (CF32ConvertI64 _) = 20
costCvtop (CF64ConvertI32 _) = 20
costCvtop (CF64ConvertI64 _) = 20
costCvtop CI32ReinterpretF32 = 10
costCvtop CI64ReinterpretF64 = 10
costCvtop CF32ReinterpretI32 = 10
costCvtop CF64ReinterpretI64 = 10

costPrmInstr :: PrmInstr a -> Cost
costPrmInstr PDrop   = 5
costPrmInstr PSelect = 10

costVarInstr :: VarInstr -> Cost
costVarInstr _ = 10

costMemInstr :: MemInstr -> Cost
costMemInstr MMemorySize = 20
costMemInstr MMemoryGrow = 300
costMemInstr _           = 10
