{-# LANGUAGE ScopedTypeVariables #-}

module Lang.Wasm.Process.TreeConstruction where

import Melude
-- Stdlib imports
import           Data.Maybe ( fromJust )
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty )
import qualified Control.Monad.State as S
-- Extra stdlib imports
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
-- External library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast ( InstrCtx, FuncType (..), ValType (..), ResultType )
-- Local imports
import Lang.Wasm.Process.Structures
import qualified Lang.Wasm.Process.Stack as Stack
import           Lang.Wasm.Process.Stack ( FullStack (..), LabelFrame (..) )


type Graph a = Vector (GraphNode a)

boundTree :: Int -> Tree a -> Tree a
boundTree 0 _ = NodeStaticError
boundTree n (Node a (PteInstr x t)) =
  Node a (PteInstr x (boundTree (n-1) t))
boundTree n (Node a (PteIf ft t f)) =
  Node a (PteIf ft (boundTree (n-1) t) (boundTree (n-1) f))
boundTree n (Node a (PteCall fIdx i)) =
  Node a (PteCall fIdx (boundTree (n-1) i))
boundTree n (Node a (PteCallIndirect ft i)) =
  Node a (PteCallIndirect ft (boundTree (n-1) i))
boundTree n (Node a (PteBr lIdx)) =
  Node a (PteBr lIdx)
boundTree n (Node a (PteBrIf lIdx t)) =
  Node a (PteBrIf lIdx t)
boundTree n (Node a (PteBrTable lIdx lIdxs)) =
  Node a (PteBrTable lIdx lIdxs)
boundTree n (Node a (PteKeepDrop kv dv i)) =
  Node a (PteKeepDrop kv dv (boundTree (n-1) i))
boundTree _ t = t

treeToGraph :: forall m a . MonadFail m => Tree a -> m (GraphFrozen ())
treeToGraph t =
  do
    ((startI, endI), g) <- runStateT ((,) <$> buildGraph t <*> newNode (NodeTerminal ())) IdList.empty
    return $ freezeGraph (startI, g, endI)
  where
  buildGraph :: Tree a -> StateT (IdList (GraphNode ())) m NodeId
  buildGraph (Node a (PteInstr x nextT)) = newNode . Node () . PgeInstr x =<< buildGraph nextT
  buildGraph (Node a (PteIf ft ifT elseT)) =
    do
      ifG <- buildGraph ifT
      elseG <- buildGraph elseT
      endG <- newNode $ NodeNaturalEnd ()
      newNode $ Node () $ PgeIf ft ifG elseG endG
  buildGraph (Node a (PteCall fIdx nextT)) =
    newNode . Node () . PgeCall fIdx =<< buildGraph nextT
  buildGraph (Node a (PteCallIndirect ft nextT)) =
    newNode . Node () . PgeCallIndirect ft =<< buildGraph nextT
  buildGraph (Node a (PteBr lIdx)) =
    newNode $ Node () $ PgeBr lIdx
  buildGraph (Node a (PteBrIf lIdx nextT)) =
    newNode . Node () . PgeBrIf lIdx =<< buildGraph nextT
  buildGraph (Node a (PteBrTable lIdxs lIdx)) =
    newNode $ Node () $ PgeBrTable lIdxs lIdx
  buildGraph (Node a (PteKeepDrop kv dv nextT)) =
    do
      fail "TODO"
      --newNode $ Node (Just a) $ PgeBlock (Ast.newFuncType (kv ++ dv) [ValType])
      -- undefined
  buildGraph (NodeNaturalEnd a) = newNode $ NodeNaturalEnd ()
  buildGraph (NodeReturn a)     = newNode $ NodeReturn ()
  buildGraph (NodeTrapped a)    = newNode $ NodeTrapped ()
  buildGraph (NodeTerminal a)   = newNode $ NodeTerminal ()
  buildGraph NodeStaticError    = fail "Failed to build graph"
  newNode :: GraphNode () -> StateT (IdList (GraphNode ())) m NodeId
  newNode = S.state . IdList.append

buildTree :: forall a
          .  InstrCtx
          -> [ValType]
          -> ResultType
          -> GraphFrozen a
          -> Tree (Maybe NodeId)
buildTree ctx stack frs (rootI, g, _) =
  -- let s = reverse $ Vector.toList fps
  -- in
  build (FullStack [LabelFrame stack frs NodeStaticError]) rootI Nothing
  where
  build :: FullStack (Tree (Maybe NodeId)) -> NodeId -> Maybe (Tree (Maybe NodeId)) -> Tree (Maybe NodeId)
  build s i t = fromMaybe NodeStaticError (build' s i t)

  build' :: FullStack (Tree (Maybe NodeId)) -> NodeId -> Maybe (Tree (Maybe NodeId)) -> Maybe (Tree (Maybe NodeId))
  build' s i afterT =
    case g !? i of
      Nothing -> Nothing
      Just (Node a (PgeInstr instr nextI)) ->
        do
          effect <- Ast.simpleInstrEffect ctx instr
          s' <- Stack.applyEffect effect s
          return $ Node (Just i) $ PteInstr instr $ build s' nextI afterT
      Just (Node a (PgeIf ft@(FuncType ps rs) ifI elseI nextI)) ->
        do
          s'    <- Stack.applyEffect ([TI32],[]) s
          nextS <- Stack.applyFuncType ft s'
          let nextT = build nextS nextI afterT
          bodyS <- Stack.applyPushLabel ft nextT s'
          let ifT   = build bodyS ifI (Just nextT)
          let elseT = build bodyS elseI (Just nextT)
          ps <- reverse <$> Stack.scopeVals s'
          let ifType = Ast.newFuncType ps (Vector.toList frs)
          return $ Node (Just i) $ PteIf ifType ifT elseT
      Just (Node a (PgeBlock ft bodyI nextI)) ->
        do
          nextS <- Stack.applyFuncType ft s
          let nextT = build nextS nextI afterT
          bodyS <- Stack.applyPushLabel ft nextT s
          build' bodyS bodyI (Just nextT)
      Just (Node a (PgeLoop ft bodyI nextI)) ->
        do
          nextS <- Stack.applyFuncType ft s
          let nextT = build nextS nextI afterT

          -- Tie the knot
          (bodyT, _) <- Stack.applyPushLabelFix (Ast.FuncType (Ast.ftParams ft) (Ast.ftParams ft)) (\bodyS -> build bodyS bodyI (Just nextT)) s
          return bodyT
      Just (Node a (PgeBr lIdx)) ->
        return NodeStaticError
      Just (Node a (PgeBrIf lIdx nextI)) ->
        do
          s' <- Stack.applyEffect ([TI32],[]) s

          (droppedStacks, keptVals, ifT, _) <- Stack.popLabel lIdx s'
          let keptValsStack = reverse $ Vector.toList keptVals
          droppedVals <- dropEqPrefix keptValsStack (concat droppedStacks)
          
          let ifT' =
                case droppedVals of
                  [] -> ifT
                  (x:xs) -> Node Nothing $ PteKeepDrop keptValsStack (x:|xs) ifT
          let elseT = build s' nextI afterT

          ps <- reverse <$> Stack.scopeVals s'
          let ifType = Ast.newFuncType ps (Vector.toList frs)
          return $ Node (Just i) $ PteIf ifType ifT' elseT
      Just (Node a (PgeBrTable lIdxs lIdx)) ->
        return NodeStaticError
      Just (Node a (PgeCall fIdx nextI)) ->
        do
          ft <- Ast.itxFuncType ctx fIdx
          nextS <- Stack.applyFuncType ft s
          let nextT = build nextS nextI afterT
          return $ Node (Just i) $ PteCall fIdx nextT
      Just (Node a (PgeCallIndirect ft nextI)) ->
        do
          nextS <- Stack.applyFuncType ft =<< Stack.applyEffect ([TI32],[]) s
          let nextT = build nextS nextI afterT
          return $ Node (Just i) $ PteCallIndirect ft nextT
      Just (NodeNaturalEnd a) ->
        return $ fromMaybe (NodeNaturalEnd $ Just i) afterT
      Just (NodeReturn a) ->
        return $ NodeReturn (Just i)
      Just (NodeTrapped a) ->
        return $ NodeTrapped (Just i)
      Just (NodeTerminal a) ->
        return $ NodeTerminal (Just i)
      Just NodeStaticError ->
        return NodeStaticError
