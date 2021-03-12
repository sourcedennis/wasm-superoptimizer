{-# LANGUAGE DeriveGeneric, TemplateHaskell, StrictData #-}

module Lang.Wasm.Ast.Values where

import Melude
import qualified Data.Uninterpreted as U
import           Data.Uninterpreted ( Uninterpreted (..) )
-- Stdlib imports
import Control.Monad.Fail ( MonadFail )
import GHC.Generics ( Generic )
import Numeric.Natural ( Natural )
import Data.Word ( Word32, Word64 )
import Data.Maybe ( fromMaybe )
-- Extra stdlib imports
import Control.DeepSeq ( NFData )
import Data.Hashable ( Hashable )


newtype WI32 = WI32 Word32
  deriving (Eq, Show, Generic)
  
newtype WI64 = WI64 Word64
  deriving (Eq, Show, Generic)

newtype WF32 = WF32 Word32
  deriving (Eq, Show, Generic)

newtype WF64 = WF64 Word64
  deriving (Eq, Show, Generic)

-- | Concrete value definition.
type Val = PVal WI32 WI64 WF32 WF64

-- | Parametric value definition.
--
-- As different value representations are frequently used within the project,
-- it's convenient to define those over this parametric structure. This
-- alleviates the burden of having to redefine these 4 cases. (Not strictly
-- necessary for the AST, though)
data PVal i32 i64 f32 f64
  = VI32 !i32
  | VI64 !i64
  | VF32 !f32
  | VF64 !f64
  deriving (Show, Generic, Eq)

type WName = String

makePrisms ''PVal

zipValM :: MonadFail m
        => ( a -> e -> m i )
        -> ( b -> f -> m j )
        -> ( c -> g -> m k )
        -> ( d -> h -> m l )
        -> PVal a b c d
        -> PVal e f g h
        -> m (PVal i j k l)
zipValM f _ _ _ (VI32 a) (VI32 b) = VI32 <$> f a b
zipValM _ f _ _ (VI64 a) (VI64 b) = VI64 <$> f a b
zipValM _ _ f _ (VF32 a) (VF32 b) = VF32 <$> f a b
zipValM _ _ _ f (VF64 a) (VF64 b) = VF64 <$> f a b
zipValM _ _ _ _ _        _        = fail "Type mismatch"

applyValM :: Applicative m
          => ( a -> m e )
          -> ( b -> m f )
          -> ( c -> m g )
          -> ( d -> m h )
          -> PVal a b c d
          -> m (PVal e f g h)
applyValM f _ _ _ (VI32 a) = VI32 <$> f a
applyValM _ f _ _ (VI64 a) = VI64 <$> f a
applyValM _ _ f _ (VF32 a) = VF32 <$> f a
applyValM _ _ _ f (VF64 a) = VF64 <$> f a


-- # Functions #

-- | Constructs an uninterpreted 32-bit integer from a signed value.
wi32s :: Integral i => i -> Maybe WI32
wi32s = fmap WI32 . U.fromSigned

-- | Constructs an uninterpreted 32-bit integer from an unsigned value.
wi32u :: Integral i => i -> Maybe WI32
wi32u = fmap WI32 . U.fromUnsigned

-- | Constructs an uninterpreted 64-bit integer from a signed value.
wi64s :: Integral i => i -> Maybe WI64
wi64s = fmap WI64 . U.fromSigned

-- | Constructs an uninterpreted 64-bit integer from an unsigned value.
wi64u :: Integral i => i -> Maybe WI64
wi64u = fmap WI64 . U.fromUnsigned

-- | Returns the /signed/ interpretation of the 32-bit uninterpreted integer.
to32s :: Integral i => WI32 -> i
to32s (WI32 i) = U.toSigned i

-- | Returns the /unsigned/ interpretation of the 32-bit uninterpreted integer.
to32u :: Integral i => WI32 -> i
to32u (WI32 i) = U.toSigned i

-- | Returns the /signed/ interpretation of the 32-bit uninterpreted integer.
to64s :: Integral i => WI64 -> i
to64s (WI64 i) = U.toSigned i

-- | Returns the /unsigned/ interpretation of the 32-bit uninterpreted integer.
to64u :: Integral i => WI64 -> i
to64u (WI64 i) = U.toSigned i


-- # Instances #

instance Hashable WI32
instance Hashable WI64
instance Hashable WF32
instance Hashable WF64
instance (Hashable i32, Hashable i64, Hashable f32, Hashable f64) => Hashable (PVal i32 i64 f32 f64)

instance NFData WI32
instance NFData WI64
instance NFData WF32
instance NFData WF64
instance (NFData i32, NFData i64, NFData f32, NFData f64) => NFData (PVal i32 i64 f32 f64)
