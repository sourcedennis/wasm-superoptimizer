{-# LANGUAGE UnicodeSyntax #-}

-- | Z3 representation for WebAssembly /integer/ numeric instructions and memory
-- instructions.
--
-- Note that Floating-point operations are omitted entirely. These are handled
-- differently, as equivalence checking of floating-point in Z3 (and its Haskell
-- bindings) is not sufficient for WebAssembly. (Those are handled through
-- uninterpreted functions instead)
--
-- WebAssembly Instructions:
-- https://webassembly.github.io/spec/core/syntax/instructions.html
--
-- WebAssembly Numerics:
-- https://webassembly.github.io/spec/core/exec/numerics.html
--
-- WARNING:
-- Z3 treats integer division-by-0 as an uninterpreted function. So `y=x/0` will
-- always assign the same value to `y` for the same value of `x`. However,
-- WebAssembly's division by 0 should trigger a trap. This must be externally
-- ensured.
--
-- Also, memory bounds are /not/ checked by this module.
module Binding.Z3.WasmZ3
  ( -- * Data Structures
    AST (..)
  , -- * Uninterpreted integers
    -- ** Construction
    mkVarI
  , mkConstI
    -- *** Stronger instances
  , mkVarI32
  , mkConstI32
  , mkVarI64
  , mkConstI64
    -- ** Unary Operators
  , mkUnopI
    -- *** Stronger instances
  , mkUnopI32
  , mkUnopI64
    -- ** Binary Operators
  , mkBinopI
    -- *** Stronger instances
  , mkBinopI32
  , mkBinopI64
    -- ** Test Operators
  , mkTestI
    -- *** Stronger instances
  , mkTestI32
  , mkTestI64
    -- ** Relative Operators
  , mkRelopI
    -- *** Stronger instances
  , mkRelopI32
  , mkRelopI64
    -- * Conversion Operators
  , mkExtend8sI32
  , mkExtend8sI64
  , mkExtend16sI32
  , mkExtend16sI64
  , mkExtend32sI64
  , mkWrapI64toI32
  , mkExtendI32toI64
  , mkExtendI8toI32
  , mkExtend2I8toI32
  , mkConcatI8toI32
  , mkConcatI32toI64
    -- * Memory Operators
  , mkAddress
  , mkMemory
  , mkLoadI32
  , mkLoadI64
  , mkLoad8sxI32
  , mkLoad8sxI64
  , mkLoad16sxI32
  , mkLoad16sxI64
  , mkLoad32sxI64
  , mkStoreI32
  , mkStoreI64
  , mkStore8I32
  , mkStore8I64
  , mkStore16I32
  , mkStore16I64
  , mkStore32I64
    -- * Parametric Operators
  , mkSelect
    -- * Other
  , mkBoolToI32
  , mkI32ToBool
  , mkLtU33
  , mkEq
  , mkNe
  , mkNot
  , mkAnd
  , mkAnd2
  , mkOr
  , mkOr2
  , mkBool
  ) where

import Melude
-- Stdlib imports
import           Data.Word ( Word32, Word64 )
-- Extra stdlib imports
import           Control.Monad ( join )
-- External library imports
import qualified Z3.Monad as Z3
import           Z3.Monad ( MonadZ3 )
-- Local library imports
import           Lang.Wasm.Ast
  ( TI32, TI64, Sx (..), IUnop (..), IBinop (..), ITestop (..), IRelop (..) )
-- Local imports
import           Lang.Wasm.Markers ( TI33, TMem )
-- import           Binding.Z3.Helpers ( mkNe )


-- | A type-safe-ish Z3 structure.
newtype AST a = AST { unAST :: Z3.AST }


-- # Markers

-- | Internal.
data TI8
data TI16


-- # 

type Bitwidth = Int


-- # I32/64 Construction #

-- | Creates a symbolic bitvector of the given size
mkVarI :: MonadZ3 z3 ⇒ Bitwidth → Z3.Symbol → z3 (AST int)
mkVarI bw symbol = AST <$> Z3.mkBvVar symbol bw

-- | A constant interpreted bitvector
mkConstI :: MonadZ3 z3 ⇒ Bitwidth → Natural → z3 (AST int)
mkConstI bw v = AST <$> Z3.mkBitvector bw (toNum v)


-- ## Stronger Instances ##

mkVarI32 :: MonadZ3 z3 ⇒ Z3.Symbol → z3 (AST TI32)
mkVarI32 = mkVarI 32

mkVarI64 :: MonadZ3 z3 ⇒ Z3.Symbol → z3 (AST TI64)
mkVarI64 = mkVarI 64

-- | A constant interpreted bitvector
mkConstI32 :: MonadZ3 z3 ⇒ Word32 → z3 (AST TI32)
mkConstI32 = mkConstI 32 . toNum

-- | A constant interpreted bitvector
mkConstI64 :: MonadZ3 z3 ⇒ Word64 → z3 (AST TI64)
mkConstI64 = mkConstI 64 . toNum



-- # I32/64 Unary Operators #

mkUnopI :: MonadZ3 z3 ⇒ Bitwidth → IUnop int → AST int → z3 (AST int)
mkUnopI bw IUnClz    a = AST <$> clz bw (unAST a)
mkUnopI bw IUnCtz    a = AST <$> ctz bw (unAST a)
mkUnopI bw IUnPopcnt a = AST <$> popcnt bw (unAST a)


-- ## Stronger Instances ##

mkUnopI32 :: MonadZ3 z3 ⇒ IUnop TI32 → AST TI32 → z3 (AST TI32)
mkUnopI32 = mkUnopI 32

mkUnopI64 :: MonadZ3 z3 ⇒ IUnop TI64 → AST TI64 → z3 (AST TI64)
mkUnopI64 = mkUnopI 64



-- # I32/64 Binary Operators #

mkBinopI :: MonadZ3 z3 ⇒ Bitwidth → IBinop int → AST int → AST int → z3 (AST int) 
mkBinopI _  IBinAdd     (AST a) (AST b) = AST <$> Z3.mkBvadd a b
mkBinopI _  IBinSub     (AST a) (AST b) = AST <$> Z3.mkBvsub a b
mkBinopI _  IBinMul     (AST a) (AST b) = AST <$> Z3.mkBvmul a b
mkBinopI _  IBinAnd     (AST a) (AST b) = AST <$> Z3.mkBvand a b
mkBinopI _  IBinOr      (AST a) (AST b) = AST <$> Z3.mkBvor a b
mkBinopI _  IBinXor     (AST a) (AST b) = AST <$> Z3.mkBvxor a b
mkBinopI bw IBinShl     (AST a) (AST b) = AST <$> (Z3.mkBvshl a =<< mkMod bw b)
mkBinopI _  IBinRotl    (AST a) (AST b) = AST <$> Z3.mkExtRotateLeft a b
mkBinopI _  IBinRotr    (AST a) (AST b) = AST <$> Z3.mkExtRotateRight a b
mkBinopI _  (IBinDiv U) (AST a) (AST b) = AST <$> Z3.mkBvudiv a b
mkBinopI _  (IBinDiv S) (AST a) (AST b) = AST <$> Z3.mkBvsdiv a b
mkBinopI _  (IBinRem U) (AST a) (AST b) = AST <$> Z3.mkBvurem a b
mkBinopI _  (IBinRem S) (AST a) (AST b) = AST <$> Z3.mkBvsrem a b
mkBinopI bw (IBinShr U) (AST a) (AST b) = AST <$> (Z3.mkBvlshr a =<< mkMod bw b)
mkBinopI bw (IBinShr S) (AST a) (AST b) = AST <$> (Z3.mkBvashr a =<< mkMod bw b)


-- ## Stronger Instances ##

mkBinopI32 :: MonadZ3 z3 ⇒ IBinop TI32 → AST TI32 → AST TI32 → z3 (AST TI32)
mkBinopI32 = mkBinopI 32

mkBinopI64 :: MonadZ3 z3 ⇒ IBinop TI64 → AST TI64 → AST TI64 → z3 (AST TI64)
mkBinopI64 = mkBinopI 64



-- # I32/64 Test Operators #

mkTestI :: MonadZ3 z3 ⇒ Bitwidth → ITestop int → AST int → z3 (AST Bool)
mkTestI bw ITestEqz (AST a) = AST <$> (boolToInt32 =<< Z3.mkEq a =<< zeroI bw)


-- ## Stronger Instances ##

mkTestI32 :: MonadZ3 z3 ⇒ ITestop TI32 → AST TI32 → z3 (AST Bool)
mkTestI32 = mkTestI 32

mkTestI64 :: MonadZ3 z3 ⇒ ITestop TI64 → AST TI64 → z3 (AST Bool)
mkTestI64 = mkTestI 64



-- # I32/64 Relative Operators #

mkRelopI :: MonadZ3 z3 ⇒ Bitwidth → IRelop int → AST int → AST int → z3 (AST Bool)
mkRelopI _ IRelEq     (AST a) (AST b) = AST <$> Z3.mkEq a b
mkRelopI _ IRelNe     a       b       = mkNe a b
mkRelopI _ (IRelLt U) (AST a) (AST b) = AST <$> Z3.mkBvult a b
mkRelopI _ (IRelLt S) (AST a) (AST b) = AST <$> Z3.mkBvslt a b
mkRelopI _ (IRelGt U) (AST a) (AST b) = AST <$> Z3.mkBvugt a b
mkRelopI _ (IRelGt S) (AST a) (AST b) = AST <$> Z3.mkBvsgt a b
mkRelopI _ (IRelLe U) (AST a) (AST b) = AST <$> Z3.mkBvule a b
mkRelopI _ (IRelLe S) (AST a) (AST b) = AST <$> Z3.mkBvsle a b
mkRelopI _ (IRelGe U) (AST a) (AST b) = AST <$> Z3.mkBvuge a b
mkRelopI _ (IRelGe S) (AST a) (AST b) = AST <$> Z3.mkBvsge a b


-- ## Stronger Instances ##

mkRelopI32 :: MonadZ3 z3 ⇒ IRelop TI32 → AST TI32 → AST TI32 → z3 (AST Bool)
mkRelopI32 = mkRelopI 32

mkRelopI64 :: MonadZ3 z3 ⇒ IRelop TI64 → AST TI64 → AST TI64 → z3 (AST Bool)
mkRelopI64 = mkRelopI 64



-- # Conversion Operators #

-- Note that conversion operators involving Floating-Points are explicitly
-- omitted. (See module docs)

-- | i32.extend8_s
mkExtend8sI32 :: MonadZ3 z3 ⇒ AST TI32 → z3 (AST TI32)
mkExtend8sI32 (AST a) = AST <$> (Z3.mkSignExt (32-8) =<< Z3.mkExtract 7 0 a)

-- | i64.extend8_s
mkExtend8sI64 :: MonadZ3 z3 ⇒ AST TI64 → z3 (AST TI64)
mkExtend8sI64 (AST a) = AST <$> (Z3.mkSignExt (64-8) =<< Z3.mkExtract 7 0 a)

-- | i32.extend16_s
mkExtend16sI32 :: MonadZ3 z3 ⇒ AST TI32 → z3 (AST TI32)
mkExtend16sI32 (AST a) = AST <$> (Z3.mkSignExt (32-16) =<< Z3.mkExtract 15 0 a)

-- | i64.extend16_s
mkExtend16sI64 :: MonadZ3 z3 ⇒ AST TI64 → z3 (AST TI64)
mkExtend16sI64 (AST a) = AST <$> (Z3.mkSignExt (64-16) =<< Z3.mkExtract 15 0 a)

-- | i64.extend32_s
mkExtend32sI64 :: MonadZ3 z3 ⇒ AST TI64 → z3 (AST TI64)
mkExtend32sI64 (AST a) = AST <$> (Z3.mkSignExt (64-32) =<< Z3.mkExtract 31 0 a)

-- | i32.wrap_i64
mkWrapI64toI32 :: MonadZ3 z3 ⇒ AST TI64 → z3 (AST TI32)
mkWrapI64toI32 (AST a) = AST <$> Z3.mkExtract 31 0 a

-- | i64.extend_i32
mkExtendI32toI64 :: MonadZ3 z3 ⇒ Sx → AST TI32 → z3 (AST TI64)
mkExtendI32toI64 U (AST a) = AST <$> Z3.mkZeroExt (64-32) a
mkExtendI32toI64 S (AST a) = AST <$> Z3.mkSignExt (64-32) a

mkExtendI8toI32 :: MonadZ3 z3 ⇒ Sx → AST TI8 → z3 (AST TI32)
mkExtendI8toI32 U (AST a) = AST <$> Z3.mkZeroExt (32-8) a
mkExtendI8toI32 S (AST a) = AST <$> Z3.mkSignExt (32-8) a

mkExtend2I8toI32 :: MonadZ3 z3 ⇒ Sx → AST TI8 → AST TI8 → z3 (AST TI32)
mkExtend2I8toI32 U (AST a) (AST b) = AST <$> (Z3.mkZeroExt (32-16) =<< Z3.mkConcat a b)
mkExtend2I8toI32 S (AST a) (AST b) = AST <$> (Z3.mkSignExt (32-16) =<< Z3.mkConcat a b)

mkConcatI8toI32 :: MonadZ3 z3 ⇒ AST TI8 → AST TI8 → AST TI8 → AST TI8 → z3 (AST TI32)
mkConcatI8toI32 (AST a) (AST b) (AST c) (AST d) = AST <$> join (Z3.mkConcat <$> Z3.mkConcat a b <*> Z3.mkConcat c d)

mkConcatI32toI64 :: MonadZ3 z3 ⇒ AST TI32 → AST TI32 → z3 (AST TI64)
mkConcatI32toI64 (AST a) (AST b) = AST <$> Z3.mkConcat a b


-- # Memory Operators #

-- Note that /memory alignment/ is irrelevant for proving equality, as it does
-- /not/ affect semantics:
-- https://webassembly.github.io/spec/core/exec/instructions.html#memory-instructions
-- Unaligned memory access may affect performance, though. (Irrelevant here)
--
-- Note that memory operations are Little Endian.

-- | Produces a memory address. This is obtained by adding a /constant offset/
-- to a 32-bit word.
mkAddress :: MonadZ3 z3 ⇒ AST TI32 → Word32 → z3 (AST TI33)
mkAddress (AST a) off = AST <$> join (Z3.mkBvadd <$> Z3.mkZeroExt 1 a <*> Z3.mkBitvector 33 (toInteger off))

-- | Produces a memory which is indexed by 33-bit-wide bitvectors. Note that the
-- caller is responsible for bounds-checking.
mkMemory :: MonadZ3 z3 ⇒ Z3.Symbol → z3 (AST TMem)
mkMemory symbol =
  do
    -- Domain: 33-bit bitvector. Range: 8-bit bitvector
    arraySort <- join (Z3.mkArraySort <$> Z3.mkBvSort 33 <*> Z3.mkBvSort 8)
    AST <$> Z3.mkConst symbol arraySort

-- | i32.load - Loads a 32-bit bitvector from memory starting at the given
-- address.
mkLoadI32 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → z3 (AST TI32)
mkLoadI32 mem addr =
  do
    AST a1 <- mkLoadI16 mem =<< mkAddAddress addr 2
    AST a0 <- mkLoadI16 mem addr
    AST <$> Z3.mkConcat a1 a0

-- | i64.load - Loads a 64-bit bitvector from memory starting at the given
-- address.
mkLoadI64 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → z3 (AST TI64)
mkLoadI64 mem addr =
  do
    AST a1 <- mkLoadI32 mem =<< mkAddAddress addr 4
    AST a0 <- mkLoadI32 mem addr
    AST <$> Z3.mkConcat a1 a0

-- | i32.load8_{sx} - Loads a 8-bit bitvector from memory and extends it to a
-- 32-bit bitvector.
mkLoad8sxI32 :: MonadZ3 z3 ⇒ Sx → AST TMem → AST TI33 → z3 (AST TI32)
mkLoad8sxI32 U mem addr = AST <$> (Z3.mkZeroExt (32 - 8) =<< (unAST <$> mkLoadI8 mem addr))
mkLoad8sxI32 S mem addr = AST <$> (Z3.mkSignExt (32 - 8) =<< (unAST <$> mkLoadI8 mem addr))

-- | i64.load8_{sx} - Loads a 8-bit bitvector from memory and extends it to a
-- 64-bit bitvector.
mkLoad8sxI64 :: MonadZ3 z3 ⇒ Sx → AST TMem → AST TI33 → z3 (AST TI64)
mkLoad8sxI64 U mem addr = AST <$> (Z3.mkZeroExt (64 - 8) =<< (unAST <$> mkLoadI8 mem addr))
mkLoad8sxI64 S mem addr = AST <$> (Z3.mkSignExt (64 - 8) =<< (unAST <$> mkLoadI8 mem addr))

-- | i32.load16_{sx} - Loads a 16-bit bitvector from memory and extends it to a
-- 32-bit bitvector.
mkLoad16sxI32 :: MonadZ3 z3 ⇒ Sx → AST TMem → AST TI33 → z3 (AST TI32)
mkLoad16sxI32 U mem addr = AST <$> (Z3.mkZeroExt (32 - 16) =<< (unAST <$> mkLoadI16 mem addr))
mkLoad16sxI32 S mem addr = AST <$> (Z3.mkSignExt (32 - 16) =<< (unAST <$> mkLoadI16 mem addr))

-- | i64.load16_{sx} - Loads a 16-bit bitvector from memory and extends it to a
-- 64-bit bitvector.
mkLoad16sxI64 :: MonadZ3 z3 ⇒ Sx → AST TMem → AST TI33 → z3 (AST TI64)
mkLoad16sxI64 U mem addr = AST <$> (Z3.mkZeroExt (64 - 16) =<< (unAST <$> mkLoadI16 mem addr))
mkLoad16sxI64 S mem addr = AST <$> (Z3.mkSignExt (64 - 16) =<< (unAST <$> mkLoadI16 mem addr))

-- | i64.load32_{sx} - Loads a 32-bit bitvector from memory and extends it to a
-- 64-bit bitvector.
mkLoad32sxI64 :: MonadZ3 z3 ⇒ Sx → AST TMem → AST TI33 → z3 (AST TI64)
mkLoad32sxI64 U mem addr = AST <$> (Z3.mkZeroExt (64 - 32) =<< (unAST <$> mkLoadI32 mem addr))
mkLoad32sxI64 S mem addr = AST <$> (Z3.mkSignExt (64 - 32) =<< (unAST <$> mkLoadI32 mem addr))

-- | i32.store - Stores a I32 at the 4 consecutive memory locations starting a
-- the given address in Little-Endian.
mkStoreI32 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → AST TI32 → z3 (AST TMem)
mkStoreI32 mem addr0 (AST val) =
  do
    a0 <- AST <$> Z3.mkExtract 15 0 val
    a1 <- AST <$> Z3.mkExtract 31 16 val
    addr1 <- mkAddAddress addr0 2
    mem' <- mkStoreI16 mem addr0 a0
    mkStoreI16 mem' addr1 a1

-- | i64.store - Stores a I64 at the 8 consecutive memory locations starting a
-- the given address in Little-Endian.
mkStoreI64 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → AST TI64 → z3 (AST TMem)
mkStoreI64 mem addr0 (AST val) =
  do
    a0 <- AST <$> Z3.mkExtract 31 0 val
    a1 <- AST <$> Z3.mkExtract 63 32 val
    addr1 <- mkAddAddress addr0 4
    mem' <- mkStoreI32 mem addr0 a0
    mkStoreI32 mem' addr1 a1

-- | i32.store8 - Stores the lowest 8 bits of the I32 at the given address.
mkStore8I32 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → AST TI32 → z3 (AST TMem)
mkStore8I32 mem addr (AST val) = mkStoreI8 mem addr =<< (AST <$> Z3.mkExtract 7 0 val)

-- | i64.store8 - Stores the lowest 8 bits of the I64 at the given address.
mkStore8I64 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → AST TI64 → z3 (AST TMem)
mkStore8I64 mem addr (AST val) = mkStoreI8 mem addr =<< (AST <$> Z3.mkExtract 7 0 val)

-- | i32.store16 - Stores the lowest 16 bits of the I32 at the 2 consecutive
-- memory locations starting at the given address in Little-Endian.
mkStore16I32 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → AST TI32 → z3 (AST TMem)
mkStore16I32 mem addr (AST val) = mkStoreI16 mem addr =<< (AST <$> Z3.mkExtract 15 0 val)

-- | i64.store16 - Stores the lowest 16 bits of the I64 at the 2 consecutive
-- memory locations starting at the given address in Little-Endian.
mkStore16I64 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → AST TI64 → z3 (AST TMem)
mkStore16I64 mem addr (AST val) = mkStoreI16 mem addr =<< (AST <$> Z3.mkExtract 15 0 val)

-- | i64.store32 - Stores the lowest 32 bits of the I64 at the 4 consecutive
-- memory locations starting at the given address in Little-Endian.
mkStore32I64 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → AST TI64 → z3 (AST TMem)
mkStore32I64 mem addr (AST val) = mkStoreI32 mem addr =<< (AST <$> Z3.mkExtract 31 0 val)

-- | Internal. Read a 8-bit bitvector.
mkLoadI8 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → z3 (AST TI8)
mkLoadI8 (AST mem) (AST idx) = AST <$> Z3.mkSelect mem idx

-- | Internal. Read a 16-bit bitvector. (Little-Endian)
mkLoadI16 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → z3 (AST TI16)
mkLoadI16 mem addr =
  do
    AST a1 <- mkLoadI8 mem =<< mkAddAddress addr 1
    AST a0 <- mkLoadI8 mem addr
    AST <$> Z3.mkConcat a1 a0

-- | Internal. Stores a 8-bit bitvector at the address
mkStoreI8 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → AST TI8 → z3 (AST TMem)
mkStoreI8 (AST mem) (AST idx) (AST val) = AST <$> Z3.mkStore mem idx val

-- | Internal. Stores a 16-bit bitvector at the address in Little-Endian.
mkStoreI16 :: MonadZ3 z3 ⇒ AST TMem → AST TI33 → AST TI16 → z3 (AST TMem)
mkStoreI16 mem addr0 (AST val) =
  do
    a0 <- AST <$> Z3.mkExtract 7 0 val
    a1 <- AST <$> Z3.mkExtract 15 8 val
    addr1 <- mkAddAddress addr0 1
    mem' <- mkStoreI8 mem addr0 a0
    mkStoreI8 mem' addr1 a1

-- | Internal. Adds a constant offset to a 33-bit address.
mkAddAddress :: MonadZ3 z3 ⇒ AST TI33 → Int → z3 (AST TI33)
mkAddAddress (AST addr) off =
  AST <$> (Z3.mkBvadd addr =<< Z3.mkBitvector 33 (toInteger off))


-- # Parametric Operators #

-- | Non-branching if-then-else. (WASM's `select` instruction)
--
-- WARNING: This is different from Z3's `mkSelect`, which indexes an array.
mkSelect :: MonadZ3 z3 ⇒ AST Bool → AST a → AST a → z3 (AST a)
mkSelect (AST c) (AST t) (AST f) = AST <$> Z3.mkIte c t f


-- # Other #

mkBoolToI32 :: MonadZ3 z3 ⇒ AST Bool → z3 (AST TI32)
mkBoolToI32 (AST b) = AST <$> join (Z3.mkIte b <$> Z3.mkBitvector 32 1 <*> Z3.mkBitvector 32 0)

mkI32ToBool :: MonadZ3 z3 ⇒ AST TI32 → z3 (AST Bool)
mkI32ToBool = fmap AST . i32Bool

-- | Unsigned less-than-or-equal
mkLtU33 :: MonadZ3 z3 ⇒ AST TI33 → AST TI33 → z3 (AST Bool)
mkLtU33 (AST a) (AST b) = AST <$> Z3.mkBvult a b

mkEq :: MonadZ3 z3 => AST a -> AST a -> z3 (AST Bool)
mkEq (AST a) (AST b) = AST <$> Z3.mkEq a b

mkNe :: MonadZ3 z3 => AST a -> AST a -> z3 (AST Bool)
mkNe (AST a) (AST b) = AST <$> (Z3.mkNot =<< Z3.mkEq a b)

mkNot :: MonadZ3 z3 => AST Bool -> z3 (AST Bool)
mkNot (AST a) = AST <$> Z3.mkNot a

mkAnd :: MonadZ3 z3 => [AST Bool] -> z3 (AST Bool)
mkAnd = fmap AST . Z3.mkAnd . map unAST

mkAnd2 :: MonadZ3 z3 => AST Bool -> AST Bool -> z3 (AST Bool)
mkAnd2 a b = mkAnd [a, b]

mkOr :: MonadZ3 z3 => [AST Bool] -> z3 (AST Bool)
mkOr = fmap AST . Z3.mkOr . map unAST

mkOr2 :: MonadZ3 z3 => AST Bool -> AST Bool -> z3 (AST Bool)
mkOr2 a b = mkOr [a, b]

mkBool :: MonadZ3 z3 => Bool -> z3 (AST Bool)
mkBool = fmap AST . Z3.mkBool


-- # Helpers #

-- | Converts an integer to a boolean. 0 converts to False, while anything else
-- converts to True.
i32Bool :: Z3.MonadZ3 z3 ⇒ AST TI32 → z3 Z3.AST
i32Bool (AST b) = Z3.mkNot =<< Z3.mkEq b =<< zeroI 32

-- -- | Returns the N-bit bitvector whose value is in module N space
mkMod :: MonadZ3 z3 ⇒ Bitwidth → Z3.AST → z3 Z3.AST
mkMod bw a = Z3.mkBvurem a =<< Z3.mkBitvector bw (toInteger bw)

-- | Converts a boolean to 1 (for True) or 0 (for False) as a bitvector of the
-- given bitwidth.
boolToInt32 :: Z3.MonadZ3 z3 ⇒ Z3.AST → z3 Z3.AST
boolToInt32 b = join (Z3.mkIte b <$> oneI 32 <*> zeroI 32)

-- | A bitvector with the value /1/, of the given bitwidth.
oneI :: MonadZ3 z3 ⇒ Int → z3 Z3.AST
oneI bitWidth = Z3.mkBitvector bitWidth 1

-- | A bitvector with the value /0/, of the given bitwidth.
zeroI :: MonadZ3 z3 ⇒ Int → z3 Z3.AST
zeroI bitWidth = Z3.mkBitvector bitWidth 0

-- | Counts the number of 1-bits. The returned value is of the given bitwidth.
--
-- Note that Z3 has no built-in operator for population counting.
popcnt :: MonadZ3 z3 ⇒ Int → Z3.AST → z3 Z3.AST
popcnt numBits x = popcnt' numBits =<< Z3.mkBitvector numBits (toInteger 0)
  where
  popcnt' :: MonadZ3 z3 ⇒ Int → Z3.AST → z3 Z3.AST
  popcnt' 0          acc = return acc
  popcnt' numRemBits acc = popcnt' (numRemBits - 1) =<< Z3.mkBvadd acc =<< bitAtExt (numRemBits - 1) numBits x

-- | Count trailing zeroes
--
-- Note that Z3 has no built-in operator for zero counting.
ctz :: MonadZ3 z3 ⇒ Int → Z3.AST → z3 Z3.AST
ctz = cz (\numRemBits → 32 - numRemBits)

-- | Count leading zeroes.
--
-- Note that Z3 has no built-in operator for zero counting.
clz :: MonadZ3 z3 ⇒ Int → Z3.AST → z3 Z3.AST
clz = cz (\numRemBits → numRemBits - 1)

-- | Generalization of count-leading-zeroes and count-trailing-zeroes
--
-- It builds the expression as follows:
-- (ite (b[f(0)] == 1) 0
--   (ite (b[f(1)] == 1) 1
--     (...)
--   )
-- )
cz :: MonadZ3 z3 ⇒ ( Int → Int ) → Int → Z3.AST → z3 Z3.AST
cz fBitIdx numBits x = cz' numBits
  where
  cz' :: MonadZ3 z3 ⇒ Int → z3 Z3.AST
  cz' 0          = Z3.mkBitvector numBits (toInteger numBits)
  cz' numRemBits =
    do
      oneBit <- Z3.mkBitvector 1 1
      currBit <- extractBit (fBitIdx numRemBits) x
      currIsOne <- Z3.mkEq oneBit currBit
      count <- Z3.mkBitvector numBits (toInteger (numBits - numRemBits))
      Z3.mkIte currIsOne count =<< cz' (numRemBits - 1)

-- | Extracts a single bit at the given index. This produces a bit-vector of
-- size 1.
extractBit :: MonadZ3 z3 ⇒ Int → Z3.AST → z3 Z3.AST
extractBit bitIdx = Z3.mkExtract bitIdx bitIdx

-- | Extracts a single bit and zero-extends the result to the given bit-width.
bitAtExt :: MonadZ3 z3 ⇒ Int → Int → Z3.AST → z3 Z3.AST
bitAtExt i numBits x = Z3.mkZeroExt (numBits - 1) =<< extractBit i x
