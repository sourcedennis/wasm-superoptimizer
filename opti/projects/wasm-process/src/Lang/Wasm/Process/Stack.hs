
module Lang.Wasm.Process.Stack where

import Melude
-- Stdlib imports
import           Control.Monad.Fail ( MonadFail )
import qualified Data.List.NonEmpty as NE
-- Extra stdlib imports
import qualified Data.Vector as Vector
import           Data.Vector ( Vector )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( LabelIdx (..), TypeStack, ResultType, ValType, FuncType (..) )
-- import Lang.Wasm.Process.Structures ( KeptVals, DroppedVals )

-- | A frame for a label. The /activation also gets a label/.
data LabelFrame a =
  LabelFrame {
    lfStack   :: TypeStack
  , lfType    :: ResultType
  , lfVal     :: a
  }
  deriving Show

newtype FullStack a = FullStack [LabelFrame a]
  deriving Show

applyFuncType :: MonadFail m => FuncType -> FullStack a -> m (FullStack a)
applyFuncType (FuncType xs ys) =
  applyEffect (Vector.toList xs, Vector.toList ys)

applyEffect :: MonadFail m
            => ([ValType], [ValType])
            -> FullStack a
            -> m (FullStack a)
applyEffect eff (FullStack []) = fail "Empty stack"
applyEffect eff (FullStack (x:xs)) =
  do
    xStack <- Ast.applyEffect eff (lfStack x)
    return $ FullStack (x { lfStack = xStack } : xs)

scopeVals :: FullStack a -> Maybe TypeStack
scopeVals (FullStack (x:_)) = Just $ lfStack x
scopeVals (FullStack []) = Nothing

-- |
popLabel :: MonadFail m
         => LabelIdx
         -> FullStack a
         -> m (NonEmpty TypeStack, ResultType, a, FullStack a)
popLabel (LabelIdx 0) (FullStack (x:xs)) =
  return (lfStack x :| [], lfType x, lfVal x, FullStack xs)
popLabel (LabelIdx i) (FullStack (x:xs)) =
  do
    (xs', rt, a, s) <- popLabel (LabelIdx (i-1)) (FullStack xs)
    return (lfStack x :| NE.toList xs', rt, a, s)
popLabel (LabelIdx _) (FullStack []) = fail "Missing label"

popAll :: MonadFail m
       => FullStack a
       -> m (NonEmpty TypeStack, ResultType, a)
popAll (FullStack [])  = fail "Empty stack"
popAll (FullStack [x]) = return (lfStack x :| [], lfType x, lfVal x)
popAll (FullStack (x:xs)) =
  do
    (xs', rt, a) <- popAll (FullStack xs)
    return (lfStack x :| NE.toList xs', rt, a)

-- |
--
applyPushLabel :: MonadFail m
               => FuncType
               -> a
               -> FullStack a
               -> m (FullStack a)
applyPushLabel (FuncType ps rs) a s =
  do
    let psL = Vector.toList ps
    s' <- applyEffect (psL, []) s
    return $ pushLabel (reverse psL) rs a s'
    
applyPushLabelFix :: MonadFail m
                => FuncType
                -> ( FullStack a -> a )
                -> FullStack a
                -> m (a, FullStack a)
applyPushLabelFix (FuncType ps rs) a s =
  do
    let psL = Vector.toList ps
    s' <- applyEffect (psL, []) s
    let res = pushLabel (reverse psL) rs aRes s'
        aRes = a res
    return (aRes, res)

pushLabel :: TypeStack -> ResultType -> a -> FullStack a -> FullStack a
pushLabel s rs a (FullStack xs) =
  FullStack (LabelFrame s rs a : xs)

fullStackVals :: FullStack a -> TypeStack
fullStackVals (FullStack xs) = concatMap lfStack xs
