{-# LANGUAGE KindSignatures #-}

-- | General definition for *backward* `SimpleInstr` operations. This detaches
-- operator semantics from program-state transformations (e.g., stack push/pops).
--
-- This may be instantiated for liveness analysis, type checking, etc.
module Lang.Wasm.AlgebraBwd where

import Melude
-- Stdlib imports
import Control.Monad ( void )
-- Local library imports
import Lang.Wasm.Ast
-- Local imports
import qualified Lang.Wasm.Algebra as A
import           Lang.Wasm.Algebra ( MonadWasmState )

-- | A specification of a particular semantics for backward execution of
-- WebAssembly instructions (`SimpleInstr`).
--
-- This backward execution is mainly relevant for dataflow analysis. For this
-- reason (and current purposes), memory instructions are omitted from this
-- algebra.
--
-- Note that many backward operations are either /partial/ or
-- /non-deterministic/ (or both). Consider `i32.div_u`.
--
-- Non-deterministic inverse (both 4 and 5 produce the same result):
--   (i32.div_u (i32.const 4) (i32.const 2))
-- = (i32.div_u (i32.const 5) (i32.const 2))
-- = (i32.const 2)
--
-- Partial inverse (no i32 lhs exists):
-- (i32.div_u (i32.const ?) (i32.const 4000000000)) = (i32.const 100)
--
--
-- TODO: Eliminate i8
data SimpleBwdAlgebra i8 i32 i64 f32 f64 ( m :: * -> * ) =
  SimpleBwdAlgebra {
    -- top
    topI32  :: m i32
  , topI64  :: m i64
  , topF32  :: m f32
  , topF64  :: m f64

    -- bottom
  , bottomI32  :: m i32
  , bottomI64  :: m i64
  , bottomF32  :: m f32
  , bottomF64  :: m f64

  , confluenceI32 :: i32 -> i32 -> m i32
  , confluenceI64 :: i64 -> i64 -> m i64
  , confluenceF32 :: f32 -> f32 -> m f32
  , confluenceF64 :: f64 -> f64 -> m f64

  , bwdUnopI32   :: IUnop WI32 -> i32 -> m i32
  , bwdUnopI64   :: IUnop WI64 -> i64 -> m i64
  , bwdBinopI32  :: IBinop WI32 -> i32 -> m (i32, i32)
  , bwdBinopI64  :: IBinop WI64 -> i64 -> m (i64, i64)

  , bwdUnopF32   :: FUnop WF32 -> f32 -> m f32
  , bwdUnopF64   :: FUnop WF64 -> f64 -> m f64
  , bwdBinopF32  :: FBinop WF32 -> f32 -> m (f32, f32)
  , bwdBinopF64  :: FBinop WF64 -> f64 -> m (f64, f64)

  , bwdTestopI32  :: ITestop WI32 -> i32 -> m i32
  , bwdTestopI64  :: ITestop WI64 -> i32 -> m i64

  , bwdRelopI32  :: IRelop WI32 -> i32 -> m (i32, i32)
  , bwdRelopI64  :: IRelop WI64 -> i32 -> m (i64, i64)
  , bwdRelopF32  :: FRelop WF32 -> i32 -> m (f32, f32)
  , bwdRelopF64  :: FRelop WF64 -> i32 -> m (f64, f64)

  , bwdSelectI32  :: i32 -> m (i32, i32, i32) -- true false cond
  , bwdSelectI64  :: i64 -> m (i64, i64, i32) -- true false cond
  , bwdSelectF32  :: f32 -> m (f32, f32, i32) -- true false cond
  , bwdSelectF64  :: f64 -> m (f64, f64, i32) -- true false cond
  
    -- These are different from extracting and i8 from an i32. Formally, these
    -- operators are /partial/, as forward satisfies:
    --  i32.extend_i8 :: [0,2^32) -> [0,2^8)
    -- Then backward satisfies:
    --  inv.i32.extend_i8 :: [0,2^8) -> [0,2^32)
    -- However, the type system does /not/ enforce this, so enforcing this is
    -- left to the `SimpleBwdAlgebra` implementer.
  , bwdExtend1I8toI32 :: Sx -> i32 -> m     i8
  , bwdExtend2I8toI32 :: Sx -> i32 -> m (T2 i8)
  , bwdExtend1I8toI64 :: Sx -> i64 -> m     i8
  , bwdExtend2I8toI64 :: Sx -> i64 -> m (T2 i8)
  , bwdExtend4I8toI64 :: Sx -> i64 -> m (T4 i8)

    -- These are different from extending i8 to i32. The upper 24 bits are
    -- /unknown/ (as opposed to 0).
  , bwdExtractI32to1I8 ::    i8 -> m i32
  , bwdExtractI32to2I8 :: T2 i8 -> m i32

  , bwdExtractI64to1I8 ::    i8 -> m i64
  , bwdExtractI64to2I8 :: T2 i8 -> m i64
  , bwdExtractI64to4I8 :: T4 i8 -> m i64

  , bwdReinterpretI32to4I8 :: T4 i8 -> m i32
  , bwdReinterpretI64to8I8 :: T8 i8 -> m i64
  , bwdReinterpret4I8toI32 :: i32 -> m (T4 i8)
  , bwdReinterpret8I8toI64 :: i64 -> m (T8 i8)
  , bwdReinterpretI32toF32 :: f32 -> m i32
  , bwdReinterpretI64toF64 :: f64 -> m i64
  , bwdReinterpretF32toI32 :: i32 -> m f32
  , bwdReinterpretF64toI64 :: i64 -> m f64
  
  , bwdTruncF32toI32  :: Sat -> Sx -> i32 -> m f32
  , bwdTruncF64toI32  :: Sat -> Sx -> i32 -> m f64
  , bwdTruncF32toI64  :: Sat -> Sx -> i64 -> m f32
  , bwdTruncF64toI64  :: Sat -> Sx -> i64 -> m f64

    -- Note, these are also very different from forward conversion; in
    -- particular, `inv.f64.promote_f32` is /partial/, and
    -- `inv.f32.demote_f64` is non-deterministic.
  , bwdConvertF32toF64 :: f64 -> m f32
  , bwdConvertF64toF32 :: f32 -> m f64
  , bwdConvertI32toF32 :: Sx -> f32 -> m i32
  , bwdConvertI32toF64 :: Sx -> f64 -> m i32
  , bwdConvertI64toF32 :: Sx -> f32 -> m i64
  , bwdConvertI64toF64 :: Sx -> f64 -> m i64
  }

execBwdAlgebra :: MonadWasmState i32 i64 f32 f64 mem m
               => SimpleBwdAlgebra i8 i32 i64 f32 f64 m
               -> SimpleInstr
               -> m ()
execBwdAlgebra a (SConstI32 v)   = void A.popStackI32 -- Ignore these for now
execBwdAlgebra a (SConstI64 v)   = void A.popStackI64 -- Ignore these for now
execBwdAlgebra a (SConstF32 v)   = void A.popStackF32 -- Ignore these for now
execBwdAlgebra a (SConstF64 v)   = void A.popStackF64 -- Ignore these for now
execBwdAlgebra a (SUnopI32 op)   = unaryOp (bwdUnopI32 a op) A.popStackI32 A.pushStackI32
execBwdAlgebra a (SUnopI64 op)   = unaryOp (bwdUnopI64 a op) A.popStackI64 A.pushStackI64
execBwdAlgebra a (SUnopF32 op)   = unaryOp (bwdUnopF32 a op) A.popStackF32 A.pushStackF32
execBwdAlgebra a (SUnopF64 op)   = unaryOp (bwdUnopF64 a op) A.popStackF64 A.pushStackF64
execBwdAlgebra a (STestopI32 op) = unaryOp (bwdTestopI32 a op) A.popStackI32 A.pushStackI32
execBwdAlgebra a (STestopI64 op) = unaryOp (bwdTestopI64 a op) A.popStackI32 A.pushStackI64
execBwdAlgebra a (SBinopI32 op)  = invBinaryOp (bwdBinopI32 a op) A.popStackI32 A.pushStackI32 A.pushStackI32
execBwdAlgebra a (SBinopI64 op)  = invBinaryOp (bwdBinopI64 a op) A.popStackI64 A.pushStackI64 A.pushStackI64
execBwdAlgebra a (SBinopF32 op)  = invBinaryOp (bwdBinopF32 a op) A.popStackF32 A.pushStackF32 A.pushStackF32
execBwdAlgebra a (SBinopF64 op)  = invBinaryOp (bwdBinopF64 a op) A.popStackF64 A.pushStackF64 A.pushStackF64
execBwdAlgebra a (SRelopI32 op)  = invBinaryOp (bwdRelopI32 a op) A.popStackI32 A.pushStackI32 A.pushStackI32
execBwdAlgebra a (SRelopI64 op)  = invBinaryOp (bwdRelopI64 a op) A.popStackI32 A.pushStackI64 A.pushStackI64
execBwdAlgebra a (SRelopF32 op)  = invBinaryOp (bwdRelopF32 a op) A.popStackI32 A.pushStackF32 A.pushStackF32
execBwdAlgebra a (SRelopF64 op)  = invBinaryOp (bwdRelopF64 a op) A.popStackI32 A.pushStackF64 A.pushStackF64
execBwdAlgebra a (SPrmInstrI32 PDrop) = bottomI32 a >>= A.pushStackI32
execBwdAlgebra a (SPrmInstrI64 PDrop) = bottomI64 a >>= A.pushStackI64
execBwdAlgebra a (SPrmInstrF32 PDrop) = bottomF32 a >>= A.pushStackF32
execBwdAlgebra a (SPrmInstrF64 PDrop) = bottomF64 a >>= A.pushStackF64
execBwdAlgebra a (SPrmInstrI32 PSelect) = execBwdSelect (bwdSelectI32 a) A.popStackI32 A.pushStackI32
execBwdAlgebra a (SPrmInstrI64 PSelect) = execBwdSelect (bwdSelectI64 a) A.popStackI64 A.pushStackI64
execBwdAlgebra a (SPrmInstrF32 PSelect) = execBwdSelect (bwdSelectF32 a) A.popStackF32 A.pushStackF32
execBwdAlgebra a (SPrmInstrF64 PSelect) = execBwdSelect (bwdSelectF64 a) A.popStackF64 A.pushStackF64

-- These inverse variable instructions are a bit weird. They are correct for my
-- current purposes.
execBwdAlgebra a (SVarInstr (VLocalGet lIdx))  =
  do
    v <- A.getLocal lIdx
    case v of
      VI32 x -> A.popStackI32 >>= confluenceI32 a x >>= (A.setLocal lIdx . VI32)
      VI64 x -> A.popStackI64 >>= confluenceI64 a x >>= (A.setLocal lIdx . VI64)
      VF32 x -> A.popStackF32 >>= confluenceF32 a x >>= (A.setLocal lIdx . VF32)
      VF64 x -> A.popStackF64 >>= confluenceF64 a x >>= (A.setLocal lIdx . VF64)
execBwdAlgebra a (SVarInstr (VLocalSet lIdx))  =
  do
    v <- A.getLocal lIdx
    case v of
      VI32 x -> A.setLocal lIdx . VI32 =<< bottomI32 a
      VI64 x -> A.setLocal lIdx . VI64 =<< bottomI64 a
      VF32 x -> A.setLocal lIdx . VF32 =<< bottomF32 a
      VF64 x -> A.setLocal lIdx . VF64 =<< bottomF64 a
    A.pushStack v
execBwdAlgebra a (SVarInstr (VLocalTee lIdx))  =
  do
    -- Consider, for example, liveness analysis. The input value is live iff
    -- /either/ the local or the remaining value is live.
    v <- A.getLocal lIdx
    case v of
      VI32 x ->
        do
          A.setLocal lIdx . VI32 =<< bottomI32 a
          A.popStackI32 >>= confluenceI32 a x >>= A.pushStackI32
      VI64 x ->
        do
          A.setLocal lIdx . VI64 =<< bottomI64 a
          A.popStackI64 >>= confluenceI64 a x >>= A.pushStackI64
      VF32 x ->
        do
          A.setLocal lIdx . VF32 =<< bottomF32 a
          A.popStackF32 >>= confluenceF32 a x >>= A.pushStackF32
      VF64 x ->
        do
          A.setLocal lIdx . VF64 =<< bottomF64 a
          A.popStackF64 >>= confluenceF64 a x >>= A.pushStackF64
execBwdAlgebra a (SVarInstr (VGlobalGet gIdx)) =
  do
    v <- A.getGlobal gIdx
    case v of
      VI32 x -> A.popStackI32 >>= confluenceI32 a x >>= (A.setGlobal gIdx . VI32)
      VI64 x -> A.popStackI64 >>= confluenceI64 a x >>= (A.setGlobal gIdx . VI64)
      VF32 x -> A.popStackF32 >>= confluenceF32 a x >>= (A.setGlobal gIdx . VF32)
      VF64 x -> A.popStackF64 >>= confluenceF64 a x >>= (A.setGlobal gIdx . VF64)
execBwdAlgebra a (SVarInstr (VGlobalSet gIdx)) =
  do
    v <- A.getGlobal gIdx
    case v of
      VI32 x -> A.setGlobal gIdx . VI32 =<< bottomI32 a
      VI64 x -> A.setGlobal gIdx . VI64 =<< bottomI64 a
      VF32 x -> A.setGlobal gIdx . VF32 =<< bottomF32 a
      VF64 x -> A.setGlobal gIdx . VF64 =<< bottomF64 a
    A.pushStack v

-- Pop the output value, and push the address
execBwdAlgebra a (SMemInstr (MI32Load _))     = A.popStackI32 >> (A.pushStackI32 =<< topI32 a)
execBwdAlgebra a (SMemInstr (MI64Load _))     = A.popStackI64 >> (A.pushStackI32 =<< topI32 a)
execBwdAlgebra a (SMemInstr (MF32Load _))     = A.popStackF32 >> (A.pushStackI32 =<< topI32 a)
execBwdAlgebra a (SMemInstr (MF64Load _))     = A.popStackF64 >> (A.pushStackI32 =<< topI32 a)
execBwdAlgebra a (SMemInstr (MI32Load8  _ _)) = A.popStackI32 >> (A.pushStackI32 =<< topI32 a)
execBwdAlgebra a (SMemInstr (MI32Load16 _ _)) = A.popStackI32 >> (A.pushStackI32 =<< topI32 a)
execBwdAlgebra a (SMemInstr (MI64Load8  _ _)) = A.popStackI64 >> (A.pushStackI32 =<< topI32 a)
execBwdAlgebra a (SMemInstr (MI64Load16 _ _)) = A.popStackI64 >> (A.pushStackI32 =<< topI32 a)
execBwdAlgebra a (SMemInstr (MI64Load32 _ _)) = A.popStackI64 >> (A.pushStackI32 =<< topI32 a)

-- Push the address and value. (forward, the value is on top of the stack)
execBwdAlgebra a (SMemInstr (MI32Store _))   = (A.pushStackI32 =<< topI32 a) >> (A.pushStackI32 =<< topI32 a)
execBwdAlgebra a (SMemInstr (MI64Store _))   = (A.pushStackI32 =<< topI32 a) >> (A.pushStackI64 =<< topI64 a)
execBwdAlgebra a (SMemInstr (MF32Store _))   = (A.pushStackI32 =<< topI32 a) >> (A.pushStackF32 =<< topF32 a)
execBwdAlgebra a (SMemInstr (MF64Store _))   = (A.pushStackI32 =<< topI32 a) >> (A.pushStackF64 =<< topF64 a)
execBwdAlgebra a (SMemInstr (MI32Store8  _)) = (A.pushStackI32 =<< topI32 a) >> (A.pushStackI32 =<< topI32 a)
execBwdAlgebra a (SMemInstr (MI32Store16 _)) = (A.pushStackI32 =<< topI32 a) >> (A.pushStackI32 =<< topI32 a)
execBwdAlgebra a (SMemInstr (MI64Store8  _)) = (A.pushStackI32 =<< topI32 a) >> (A.pushStackI64 =<< topI64 a)
execBwdAlgebra a (SMemInstr (MI64Store16 _)) = (A.pushStackI32 =<< topI32 a) >> (A.pushStackI64 =<< topI64 a)
execBwdAlgebra a (SMemInstr (MI64Store32 _)) = (A.pushStackI32 =<< topI32 a) >> (A.pushStackI64 =<< topI64 a)

execBwdAlgebra a (SMemInstr MMemorySize) = void A.popStackI32
execBwdAlgebra a (SMemInstr MMemoryGrow) = void A.popStackI32 >> (A.pushStackI32 =<< topI32 a)

execBwdAlgebra a (SCvtop CI32Extend8S)  = unaryOp (bwdExtend1I8toI32 a S >=> bwdExtractI32to1I8 a) A.popStackI32 A.pushStackI32
execBwdAlgebra a (SCvtop CI32Extend16S) = unaryOp (bwdExtend2I8toI32 a S >=> bwdExtractI32to2I8 a) A.popStackI32 A.pushStackI32
execBwdAlgebra a (SCvtop CI64Extend8S)  = unaryOp (bwdExtend1I8toI64 a S >=> bwdExtractI64to1I8 a) A.popStackI64 A.pushStackI64
execBwdAlgebra a (SCvtop CI64Extend16S) = unaryOp (bwdExtend2I8toI64 a S >=> bwdExtractI64to2I8 a) A.popStackI64 A.pushStackI64
execBwdAlgebra a (SCvtop CI64Extend32S) = unaryOp (bwdExtend4I8toI64 a S >=> bwdExtractI64to4I8 a) A.popStackI64 A.pushStackI64
execBwdAlgebra a (SCvtop CI32WrapI64)   = unaryOp (bwdReinterpret4I8toI32 a >=> bwdExtractI64to4I8 a) A.popStackI32 A.pushStackI64
execBwdAlgebra a (SCvtop (CI64ExtendI32 sx))    = unaryOp (bwdExtend4I8toI64 a sx >=> bwdReinterpretI32to4I8 a) A.popStackI64 A.pushStackI32
execBwdAlgebra a (SCvtop (CI32TruncF32 sat sx)) = unaryOp (bwdTruncF32toI32 a sat sx) A.popStackI32 A.pushStackF32
execBwdAlgebra a (SCvtop (CI32TruncF64 sat sx)) = unaryOp (bwdTruncF64toI32 a sat sx) A.popStackI32 A.pushStackF64
execBwdAlgebra a (SCvtop (CI64TruncF32 sat sx)) = unaryOp (bwdTruncF32toI64 a sat sx) A.popStackI64 A.pushStackF32
execBwdAlgebra a (SCvtop (CI64TruncF64 sat sx)) = unaryOp (bwdTruncF64toI64 a sat sx) A.popStackI64 A.pushStackF64
execBwdAlgebra a (SCvtop CF32DemoteF64)         = unaryOp (bwdConvertF64toF32 a) A.popStackF32 A.pushStackF64
execBwdAlgebra a (SCvtop CF64PromoteF32)        = unaryOp (bwdConvertF32toF64 a) A.popStackF64 A.pushStackF32
execBwdAlgebra a (SCvtop (CF32ConvertI32 sx))   = unaryOp (bwdConvertI32toF32 a sx) A.popStackF32 A.pushStackI32
execBwdAlgebra a (SCvtop (CF32ConvertI64 sx))   = unaryOp (bwdConvertI64toF32 a sx) A.popStackF32 A.pushStackI64
execBwdAlgebra a (SCvtop (CF64ConvertI32 sx))   = unaryOp (bwdConvertI32toF64 a sx) A.popStackF64 A.pushStackI32
execBwdAlgebra a (SCvtop (CF64ConvertI64 sx))   = unaryOp (bwdConvertI64toF64 a sx) A.popStackF64 A.pushStackI64
execBwdAlgebra a (SCvtop CI32ReinterpretF32)    = unaryOp (bwdReinterpretF32toI32 a) A.popStackI32 A.pushStackF32
execBwdAlgebra a (SCvtop CI64ReinterpretF64)    = unaryOp (bwdReinterpretF64toI64 a) A.popStackI64 A.pushStackF64
execBwdAlgebra a (SCvtop CF32ReinterpretI32)    = unaryOp (bwdReinterpretI32toF32 a) A.popStackF32 A.pushStackI32
execBwdAlgebra a (SCvtop CF64ReinterpretI64)    = unaryOp (bwdReinterpretI64toF64 a) A.popStackF64 A.pushStackI64

top :: MonadWasmState i32 i64 f32 f64 mem m
    => SimpleBwdAlgebra i8 i32 i64 f32 f64 m
    -> ValType
    -> m (PVal i32 i64 f32 f64)
top a TI32 = VI32 <$> topI32 a
top a TI64 = VI64 <$> topI64 a
top a TF32 = VF32 <$> topF32 a
top a TF64 = VF64 <$> topF64 a

bottom :: MonadWasmState i32 i64 f32 f64 mem m
       => SimpleBwdAlgebra i8 i32 i64 f32 f64 m
       -> ValType
       -> m (PVal i32 i64 f32 f64)
bottom a TI32 = VI32 <$> bottomI32 a
bottom a TI64 = VI64 <$> bottomI64 a
bottom a TF32 = VF32 <$> bottomF32 a
bottom a TF64 = VF64 <$> bottomF64 a

execBwdSelect :: MonadWasmState i32 i64 f32 f64 mem m
              => ( a -> m (a, a, i32) ) -> m a -> ( a -> m x ) -> m ()
execBwdSelect fInvSelect fPop fPush =
  do
    res <- fPop
    (iTrue, iFalse, cond) <- fInvSelect res
    fPush iTrue
    fPush iFalse
    A.pushStackI32 cond


-- | Unary WASM operator, for which input popping and result pushing is handled.
unaryOp :: MonadWasmState i32 i64 f32 f64 mem m => ( a -> m c ) -> m a -> ( c -> m x ) -> m x
unaryOp fOp a fPush =
  do
    a' <- a
    c <- fOp a'
    fPush c

invBinaryOp :: MonadWasmState i32 i64 f32 f64 mem m => ( c -> m (a, b) ) -> m c -> ( a -> m x ) -> ( b -> m y ) -> m y
invBinaryOp fOp fPop fPushA fPushB =
  do
    c <- fPop
    (a, b) <- fOp c
    fPushA a
    fPushB b
