{-# LANGUAGE DeriveGeneric #-}

module Helpers
  ( FuncDesc (..)
  , buildFuncDescs
  , buildFuncDesc
  , buildInstrCtx
  , definedFuncs
  , mem0
  , fgGraphSize
  , toAst
  , cleanAndToAst
  ) where

import Melude
-- Stdlib imports
import           GHC.Generics ( Generic )
import           Control.DeepSeq ( NFData, force )
-- Extra stdlib imports
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( Module (..), DefinedFunc (..), FuncType, GlobalType, ValType, TypeIdx (..)
  , FuncIdx (..), InstrCtx (..) , ResultType, PVal (..), Instr (..)
  , LabelIdx (..), funcType, funcs, globals, globalType
  )
import qualified Lang.Wasm.Process as P
import           Lang.Wasm.Process
import Lang.Wasm.Optimizations.CleanDead ( removeDead )
  -- ( SingleId, SingleGraph, SingleGraphFrozen, GraphNode (..), Edge (..), DropKeep (..)
  -- , KeptVals, DroppedVals
  -- , freezeSingle, buildGraph, isNodeTerminal, isNodeTrapped )


data FuncDesc =
  FuncDesc {
    fgIdx     :: FuncIdx
  , fgFunc    :: DefinedFunc
  , fgGraph   :: GraphFrozen ()
  , fgSfc     :: InstrCtx
  }
  deriving (Show, Generic)

instance NFData FuncDesc

fgGraphSize :: FuncDesc -> Int
fgGraphSize d =
  let (_, g, _) = fgGraph d
  in Vector.length g

buildFuncDescs :: MonadFail m => Module -> m [FuncDesc]
buildFuncDescs m =
  let fs = Vector.toList $ mFuncsDefined m
      gs  = Vector.fromList $ map globalType $ globals m
      fts = Vector.fromList $ map funcType $ funcs m
      numImportedFuncs = Vector.length $ mFuncsImported m
  in zipWithM (buildFuncDesc' gs fts) [numImportedFuncs..] fs


-- |
--
-- WARNING: When using multiple functions from a module, use `buildFuncDescs`!
--   That method shared the `FuncType` and `GlobalType` vectors, ensuring
--   significantly less memory is used (for large WASM programs).
buildFuncDesc :: MonadFail m => Module -> FuncIdx -> m (Maybe FuncDesc)
buildFuncDesc m fIdx@(FuncIdx i) =
  let gs  = Vector.fromList $ map globalType $ globals m
      fts = Vector.fromList $ map funcType $ funcs m
  in
  case Ast.lookupFunc fIdx m of
    Just (Ast.FuncDefined d) -> Just <$> buildFuncDesc' gs fts i d
    _ -> return Nothing

definedFuncs :: Module -> [(FuncIdx,DefinedFunc)]
definedFuncs m = 
  let numImportedFuncs = Vector.length $ mFuncsImported m
  in zip (map FuncIdx [numImportedFuncs..]) (Vector.toList $ mFuncsDefined m)

buildFuncDesc' :: MonadFail m => Vector GlobalType -> Vector FuncType -> Int -> DefinedFunc -> m FuncDesc
buildFuncDesc' gs fts fIdx func =
  do
    let sfc = InstrCtx gs (Ast.fParams func) (fLocals func) fts
    g <- buildGraph sfc (Ast.fBody func)
    return $ FuncDesc (FuncIdx fIdx) func (freezeGraph g) sfc

buildInstrCtx :: Module -> DefinedFunc -> InstrCtx
buildInstrCtx m d =
  let gs  = Vector.fromList $ map globalType $ globals m
      fts = Vector.fromList $ map funcType $ funcs m
  in InstrCtx gs (Ast.fParams d) (fLocals d) fts

-- | Returns the static bounds of the /first/ memory block, if it exists.
mem0 :: Ast.Module -> Maybe Ast.Limits
mem0 m =
  case Ast.mems m of
    []    -> Nothing
    (x:_) -> Just $ Ast.memLimits x

cleanAndToAst :: FuncDesc -> IO Ast.DefinedFunc
cleanAndToAst d =
  case removeDead (fgSfc d) (Ast.ftResults $ Ast.fType $ fgFunc d) (fgGraph d) of
    Nothing ->
      do
        putStrLn "Liveness error"
        case toAst d of
          Just ast  -> return ast
          Nothing   -> putStrLn "Replacement ERROR!" >> return (fgFunc d)
    Just g ->
      case toAst $ d { fgGraph = g } of
        Just ast -> return ast
        Nothing  ->
          do
            putStrLn "Liveness graph error"
            case toAst d of
              Just ast  -> return ast
              Nothing   -> putStrLn "Replacement ERROR!" >> return (fgFunc d)

toAst :: MonadFail m => FuncDesc -> m Ast.DefinedFunc
toAst d =
  do
    body <- P.graphToAst (fgSfc d) (fgGraph d)
    return $ Ast.DefinedFunc (Ast.fType $ fgFunc d) (Ast.fLocals $ fgFunc d) body
