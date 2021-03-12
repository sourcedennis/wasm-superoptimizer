{-# LANGUAGE DeriveGeneric, KindSignatures, ScopedTypeVariables, FlexibleInstances,
  TypeSynonymInstances
#-}

module Lang.Wasm.Process.Structures
  ( -- * Data Structures
    -- ** Graph
    GraphEdge (..)
  , NodeId
  , Node (..)
  , GraphNode (..)
  , GraphMut
  , GraphFrozen
    -- ** Tree
  , Tree (..)
  , TreeEdge (..)
  , mapTree
  -- , toTree
    -- ** Helpers
  -- , DroppedVals
  -- , DroppedValsNE
  -- , KeptVals
  -- , KeepDrop (..)
  -- , SingleId
  -- , SingleEdge
  -- , SingleNode
  -- , SingleGraph
  -- , SingleGraphFrozen
  -- , MultiId
  -- , MultiEdge
  -- , MultiNode
  -- , MultiGraph
  -- , MultiGraphFrozen
  --  -- ** Helpers
  -- , KeptVals
  -- , DroppedValsNE
  -- , DroppedVals
  -- , DropKeep (..)
   -- * Construction
  -- , newKeepDrop
  -- , freezeMulti
  -- , freezeSingle
    -- * Helpers
  , freezeGraph
  , unfreezeGraph
  -- , isNodeTerminal
  -- , isNodeTrapped
  -- , viewDropKeep
  -- , applyDropKeep
  -- , isEqType
  , showTreeLines
  , showGraphLines
  , loopEntries
  ) where

import Melude
-- Stdlib imports
import           Control.Monad.Fail ( MonadFail )
import qualified Data.List.NonEmpty as NE
import           GHC.Generics ( Generic )
import           Control.DeepSeq ( NFData )
import           Data.List.NonEmpty ( NonEmpty ((:|)) )
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
-- Extra stdlib imports
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
-- External library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( SimpleInstr, FuncIdx, FuncType (..), ValType (..), PVal (..), LabelIdx (..)
  , InstrCtx, TypeStack
  )


-- # Data Structures #

-- ## General ##

data Node edge a
  = Node !a edge -- keep edge lazy!
    -- | Naturally reach the end of a block
  | NodeNaturalEnd !a
    -- | /Explicitly/ return from the function (terminal node)
  | NodeReturn !a
    -- | Abort execution (usually after an `unreachable` instruction)
  | NodeTrapped !a
  | NodeTerminal !a
    -- When this is reachable, discard the tree/graph. (Avoids `error`)
  | NodeStaticError
  deriving (Show, Generic)


-- ## Graph ##

type NodeId = Int

-- | A sub-graph is just represented by its root node. It is contained within
-- the total graph to guarantee unique identifiers for every node.
type SubGraph = NodeId

-- | An edge in a process graph or tree. It represents a /state transition/
-- from an source state to an destination state.
data GraphEdge
  = PgeInstr !SimpleInstr !NodeId
  | PgeIf !FuncType !SubGraph !SubGraph !NodeId -- if, else, next
  | PgeBlock !FuncType !SubGraph !NodeId -- block, next
  | PgeLoop !FuncType !SubGraph !NodeId -- loop, next
  | PgeBr !LabelIdx
  | PgeBrIf !LabelIdx !NodeId
    -- | Table branching. It pops a i32-value which indexes in the
    -- branch table. If it's out of bounds it goes to the default.
    -- Corresponds roughly to the `br_table` in the WASM spec.
  | PgeBrTable ![LabelIdx] !LabelIdx
    -- | The `NodeId` is the ID of the node /after returning/ from the
    -- function.
  | PgeCall !FuncIdx !NodeId
    -- | The indirect call is mainly used for dynamic linking, which can thus
    -- not be statically determined. In that case, drop all knowledge about
    -- memory upon returning from such a call. Nothing is known about the
    -- results.
  | PgeCallIndirect !FuncType !NodeId
  deriving (Show, Generic)

type GraphNode a = Node GraphEdge a

-- | This graph must strictly be /acyclic/. Cycles are introduced within the
-- `PeLoop` edge.
type Graph (vec :: * -> *) a = (NodeId, vec (GraphNode a), NodeId)

-- | :: (NodeId, IdList (Node (GraphEdge _ _) a))
type GraphMut a = Graph IdList a

type GraphFrozen a = Graph Vector a


-- ## Tree ##

type Tree a = Node (TreeEdge a) a

-- |
--
-- `br_table` is omitted.
data TreeEdge a
  = PteInstr !SimpleInstr !(Tree a)
  | PteIf !FuncType !(Tree a) !(Tree a)
  | PteCall !FuncIdx !(Tree a)
  | PteCallIndirect !FuncType !(Tree a)
  | PteBr !LabelIdx
  | PteBrIf !LabelIdx !(Tree a)
  | PteBrTable ![LabelIdx] LabelIdx
  | PteKeepDrop !KeptVals !DroppedValsNE !(Tree a)
  deriving (Show, Generic)

type Visited = IntSet

type LoopEntries = IntSet

loopEntries :: MonadFail m => GraphFrozen a -> m LoopEntries
loopEntries (startI, g, endI) = visit startI IntSet.empty
  where
  visit :: MonadFail m => NodeId -> LoopEntries -> m LoopEntries
  visit i =
    case g !? i of
      Nothing -> fail "Invalid graph"
      Just (Node _ (PgeInstr _ j)) ->
        visit j
      Just (Node _ (PgeIf _ ifI elseI j)) ->
        visit j <=< visit elseI <=< visit ifI
      Just (Node _ (PgeBlock _ bodyI j)) ->
        visit j <=< visit bodyI
      Just (Node _ (PgeLoop ft bodyI j)) ->
        visit j <=< visit bodyI . IntSet.insert bodyI
      Just (Node _ (PgeBr _))        -> pure
      Just (Node _ (PgeBrIf _ j))    -> visit j
      Just (Node _ (PgeBrTable _ _)) -> pure
      Just (Node _ (PgeCall _ j))    -> visit j
      Just (Node _ (PgeCallIndirect _ j)) -> visit j
      Just _ -> pure

-- data LabelStack a =
--   LabelStack {
--     lsStack      :: !TypeStack
--   , lsSignature  :: !KeptVals
--   , lsVal        :: !(Tree a)
--   }
--   deriving (Show, Generic)

-- | Grows to the left
-- data FullStack a = FullStack [LabelStack a] TypeStack
--   deriving (Show, Generic)

-- -- | Converts the graph into a (possibly infinite) process tree.
-- toTree :: InstrCtx -> FuncType -> GraphFrozen a -> a -> Tree a
-- toTree ctx (FuncType fps frs) g endVal = undefined
-- --   toTree' (FullStack [] (reverse $ Vector.toList fps)) g (NodeNaturalEnd endVal)
--   where
--   toTree' :: FullStack a -> GraphFrozen a -> Tree a -> Tree a
--   toTree' s (nodeI, g) afterT =
--     case g !? nodeI of
--       Just (Node a (PgeInstr instr nextI)) ->
--         let effect = fromJust $ Ast.simpleInstrEffect ctx instr
--             s' = applyEffect' effect s
--         in Node a $ PteInstr instr $ toTree' s' (nextI, g) afterT
--       Just (Node a (PgeIf ft@(FuncType ps rs) ifG elseG nextI)) ->
--         let s'  = applyEffect' ([TI32],[]) s
--             nextS = applyFuncType' ft s'
--             nextT = toTree' nextS (nextI, g) afterT
--             bodyS = applyLabel (reverse $ Vector.toList ps) (reverse $ Vector.toList rs, nextT) s'
--             ifT   = toTree' bodyS (ifG, g) nextT
--             elseT = toTree' bodyS (elseG, g) nextT
--         in Node a $ PteIf (FuncType (Vector.fromList $ reverse $ fullStack bodyS) frs) ifT elseT
--       Just (Node a (PgeBlock ft@(FuncType ps rs) blockG nextI)) ->
--         let nextS = applyFuncType' ft s
--             nextT = toTree' nextS (nextI, g) afterT
--             bodyS = applyLabel (reverse $ Vector.toList ps) (reverse $ Vector.toList rs, nextT) s
--         in toTree' bodyS (blockG, g) nextT
--       Just (Node a (PgeLoop ft@(FuncType ps rs) loopG nextI)) ->
--         let nextS = applyFuncType' ft s
--             nextT = toTree' nextS (nextI, g) afterT
--             bodyS = applyLabel (reverse $ Vector.toList ps) (reverse $ Vector.toList ps, loopT) s
--             loopT = toTree' bodyS (loopG, g) afterT
--         in loopT
--       Just (Node a (PgeBr lIdx)) ->
--         undefined
--       Just (Node a (PgeBrIf lIdx i)) ->
--         let s' = applyEffect' ([TI32],[]) s
--         in undefined
--       Just (Node a (PgeBrTable lIdxs lIdx)) ->
--         -- Traces with `br_table` cannot generally be reconstructed into a graph.
--         -- For now, regard these trees entirely.
--         Node a PteStaticError
--       Just (Node a (PgeCall fIdx nextI)) ->
--         let ft = fromJust $ Ast.itxFuncType ctx fIdx
--             nextS = applyFuncType' ft s
--             nextT = toTree' nextS (nextI, g) afterT
--         in Node a $ PteCall fIdx nextT
--       Just (Node a (PgeCallIndirect ft nextI)) ->
--         let nextS = applyFuncType' ft $ applyEffect' ([TI32],[]) s
--             nextT = toTree' nextS (nextI, g) afterT
--         in Node a $ PteCallIndirect ft nextT
--       Just (NodeNaturalEnd a) -> NodeNaturalEnd a
--       Just (NodeReturn a)     -> NodeReturn a
--       Just (NodeTrapped a)    -> NodeTrapped a
--       Nothing -> error "Statically-invalid graph"
  -- LabelStack {
  --   lsStack      :: !TypeStack
  -- , lsSignature  :: !KeptVals
  -- , lsVal        :: !(Tree a)
  -- }
  -- findLabel :: LabelIdx -> FullStack a -> Tree a -> Either LabelIdx ([ValType], TypeStack, Tree a)
  -- findLabel (LabelIdx 0)    (FullStack [] zs) afterT =
  --   let (keptVals, droppedVals) = fromJust $ splitEqPrefix (reverse $ Vector.toList rs) zs
  --   in
  --   if null droppedVals then
  --     Right (KDKeepAll, afterT)
  --   else
  --     Left (LabelIdx 0)
  -- findLabel (LabelIdx lIdx) (FullStack [] zs)     afterT = Left (LabelIdx lIdx)
  -- findLabel (LabelIdx 0)    (FullStack (x:xs) zs) afterT =
  --   let (keptVals, droppedVals) = fromJust $ splitEqPrefix (lsSignature x) zs
  --   in
  --   if null droppedVals then
  --     Right (KDKeepAll, afterT)
  --   else
  --     Left (LabelIdx 0)
  
  -- fullStack :: FullStack a -> TypeStack
  -- fullStack (FullStack xs zs) = concatMap lsStack xs ++ zs

  -- applyLabel :: TypeStack -> (KeptVals, Tree a) -> FullStack a -> FullStack a
  -- applyLabel s (v, t) fs =
  --   let (FullStack ys zs) = applyEffect' (reverse s,[]) fs
  --   in FullStack (LabelStack s v t:ys) zs

  -- applyFuncType' :: FuncType -> FullStack a -> FullStack a
  -- applyFuncType' (FuncType xs ys) = applyEffect' (Vector.toList xs, Vector.toList ys)

  -- applyEffect' :: ([ValType],[ValType]) -> FullStack a -> FullStack a
  -- applyEffect' xs (FullStack (y:ys) zs) =
  --   FullStack (y { lsStack = fromJust $ Ast.applyEffect xs (lsStack y) }:ys) zs
  -- applyEffect' xs (FullStack [] zs) =
  --   FullStack [] (fromJust $ Ast.applyEffect xs zs)

freezeGraph :: GraphMut a -> GraphFrozen a
freezeGraph (rootI, g, terminalI) =
  (rootI, Vector.fromList $ IdList.toList g, terminalI)

unfreezeGraph :: GraphFrozen a -> GraphMut a
unfreezeGraph (rootI, g, terminalI) =
  (rootI, IdList.fromList $ Vector.toList g, terminalI)


-- ## Helper Structures ##

-- type NumDrop = Int
-- type NumKeep = Int

-- -- | Top is at the head
-- type DroppedVals = [ValType]
-- -- | Top is at the head
type DroppedValsNE = NonEmpty ValType
-- -- | Top is at the head
type KeptVals = [ValType]

type DList a = [a] -> [a]

dcons :: a -> DList a
dcons x = (x:)

type Indent = Int

-- |
--
-- if (I32)->(I32)
-- | instr (i32.const 10)
-- | instr (i32.const 4)
-- else
-- | instr (i32.const 4)
-- end
showTreeLines :: Show a => Tree a -> [String]
showTreeLines t = showTree' 0 t []
  where
  showTree' :: Show a => Indent -> Tree a -> DList String
  showTree' i (Node a edge)      = showEdge a i edge
  showTree' i (NodeNaturalEnd a) = dcons (replicateShow i (ß"| ") . ß"End # " . shows a $ "")
  showTree' i (NodeReturn a)     = dcons (replicateShow i (ß"| ") . ß"Return # " . shows a $ "")
  showTree' i (NodeTrapped a)    = dcons (replicateShow i (ß"| ") . ß"Trapped # " . shows a $ "")
  showTree' i (NodeTerminal a)   = dcons (replicateShow i (ß"| ") . ß"Terminal # " . shows a $ "")
  showTree' i NodeStaticError    = dcons (replicateShow i (ß"| ") . ß"Error" $ "")

  showEdge :: Show a => a -> Indent -> TreeEdge a -> DList String
  showEdge a i (PteInstr x n) =
    dcons (replicateShow i (ß"| ") . shows x . ß" # " . shows a $ "") . showTree' i n
  showEdge a i (PteIf ft x y) =
    dcons (showIndent i . ß"if " . shows ft . ß" # " . shows a $ "")
    . showTree' (i+1) x
    . dcons (showIndent i . ß"else" $ "")
    . showTree' (i+1) y
    . dcons (showIndent i . ß"end" $ "")

showGraphLines :: Show a => GraphFrozen a -> [String]
showGraphLines (rootI, g, endI) = showGraph 1 rootI $ showGraph 0 endI []
  where
  showGraph :: Indent -> NodeId -> DList String
  showGraph indent nodeI =
    case g !? nodeI of
      Nothing ->
        dcons (showIndent indent . ß"ERROR (" . shows nodeI . ß")" $ "")
      Just (Node a (PgeInstr instr j)) ->
        dcons (showIndent indent . shows instr . ß" (" . shows nodeI . ß") # " . shows a $ "")
        . showGraph indent j
      Just (Node a (PgeIf ft ifG elseG j)) ->
        dcons (showIndent indent . ß"if " . shows ft . ß" (" . shows nodeI . ß") # " . shows a $ "")
        . showGraph (indent+1) ifG
        . dcons (showIndent indent . ß"else" $ "")
        . showGraph (indent+1) elseG
        . showGraph indent j
      Just (Node a (PgeBlock ft bodyG j)) ->
        dcons (showIndent indent . ß"block " . shows ft . ß" (" . shows nodeI . ß") # " . shows a $ "")
        . showGraph (indent+1) bodyG
        . showGraph indent j
      Just (Node a (PgeLoop ft bodyG j)) ->
        dcons (showIndent indent . ß"loop " . shows ft . ß" (" . shows nodeI . ß") # " . shows a $ "")
        . showGraph (indent+1) bodyG
        . showGraph indent j
      Just (Node a (PgeBr lIdx)) ->
        dcons (showIndent indent . ß"Br " . shows lIdx . ß" (" . shows nodeI . ß") # " . shows a $ "")
      Just (Node a (PgeBrIf lIdx j)) ->
        dcons (showIndent indent . ß"BrIf " . shows lIdx . ß" (" . shows nodeI . ß") # " . shows a $ "")
        . showGraph indent j
      Just (Node a (PgeBrTable lIdxs lIdx)) ->
        dcons (showIndent indent . ß"BrTable " . shows lIdxs . ß" " . shows lIdx . ß" (" . shows nodeI . ß") # " . shows a $ "")
      Just (Node a (PgeCall fIdx j)) ->
        dcons (showIndent indent . ß"Call " . shows fIdx . ß" (" . shows nodeI . ß") # " . shows a $ "")
        . showGraph indent j
      Just (Node a (PgeCallIndirect ft j)) ->
        dcons (showIndent indent . ß"CallIndirect " . shows ft . ß" (" . shows nodeI . ß") # " . shows a $ "")
        . showGraph indent j
      Just (NodeNaturalEnd a) ->
        dcons (showIndent indent . ß"End (" . shows nodeI . ß") # " . shows a $ "")
      Just (NodeReturn a) ->
        dcons (showIndent indent . ß"Return (" . shows nodeI . ß") # " . shows a $ "")
      Just (NodeTrapped a) ->
        dcons (showIndent indent . ß"Trapped (" . shows nodeI . ß") # " . shows a $ "")
      Just (NodeTerminal a) ->
        dcons (showIndent indent . ß"Terminal (" . shows nodeI . ß") # " . shows a $ "")
      Just NodeStaticError ->
        dcons (showIndent indent . ß"ERROR" $ "")

showIndent :: Indent -> ShowS
showIndent i = replicateShow i (ß"| ")

-- |
--
-- Elements within the WebAssembly AST cause some lower values to be dropped
-- from the stack (e.g., exiting a block). As the graph contains no syntactic
-- structures such as blocks, these modifications are modeled through this
-- -- structure.
-- data KeepDrop
--   = KDKeep KeptVals DroppedValsNE
--   | KDKeepAll
--   deriving (Eq, Show, Generic)


-- -- # Construction #

-- newKeepDrop :: KeptVals -> DroppedVals -> KeepDrop
-- newKeepDrop _        []     = KDKeepAll
-- newKeepDrop keptVals (x:xs) = KDKeep keptVals (x :| xs)

-- freezeSingle :: SingleGraph a -> SingleGraphFrozen a
-- freezeSingle = Vector.fromList . IdList.elems

-- freezeMulti :: MultiGraph a -> MultiGraphFrozen a
-- freezeMulti = Vector.fromList . IdList.elems


-- -- # Helpers #

-- isNodeTerminal :: GraphNode edge a -> Bool
-- isNodeTerminal (GraphTerminal _) = True
-- isNodeTerminal _ = False

-- isNodeTrapped :: GraphNode edge a -> Bool
-- isNodeTrapped (GraphTrapped _) = True
-- isNodeTrapped _ = False

-- viewDropKeep :: DropKeep -> (KeptVals, DroppedVals)
-- viewDropKeep DKKeepAll = ([],[])
-- viewDropKeep (DKKeep kv (x:|xs)) = (kv, x:xs)

-- applyDropKeep :: DropKeep -> [PVal i32 i64 f32 f64] -> Maybe [PVal i32 i64 f32 f64]
-- applyDropKeep DKKeepAll      xs = Just xs
-- applyDropKeep (DKKeep kv dv) xs =
--   do
--     (keptVals, xs') <- splitPrefix (map isEqType kv) xs
--     xs'' <- dropPrefix (map isEqType (NE.toList dv)) xs'
--     Just (keptVals ++ xs'')

-- isEqType :: ValType -> PVal i32 i64 f32 f64 -> Bool
-- isEqType vt v = vt == Ast.valType v


-- # Instances #

-- instance NFData DropKeep
-- instance NFData node => NFData (Edge node)
-- instance (NFData edge, NFData a) => NFData (GraphNode edge a)

instance (NFData edge, NFData a) => NFData (Node edge a)
instance NFData GraphEdge

-- instance Functor Edge where
--   fmap f (PeInstr i a)           = PeInstr i (f a)
--   fmap f (PeIf (dks, a) b)       = PeIf (dks, f a) (f b)
--   fmap f (PeTable nk ts n)       = PeTable nk (map (mapSnd f) ts) (mapSnd f n)
--   fmap f (PeCall fIdx n)         = PeCall fIdx (f n)
--   fmap f (PeCallIndirect ft n)   = PeCallIndirect ft (f n)
--   fmap f (PeDropMany nk xs n)    = PeDropMany nk xs (f n)

-- instance Functor Tree where
--   fmap f (TreeNode a e)   = TreeNode (f a) (fmap (fmap f) e)
--   fmap f (TreeTerminal a) = TreeTerminal (f a)
--   fmap f (TreeForever a)  = TreeForever (f a)
--   fmap f (TreeTrapped a)  = TreeTrapped (f a)

-- Cannot define `Functor` over type aliases.
mapTree :: ( a -> b ) -> Tree a -> Tree b
mapTree f (Node a edge)      = Node (f a) (f <$> edge)
mapTree f (NodeNaturalEnd a) = NodeNaturalEnd (f a)
mapTree f (NodeReturn a)     = NodeReturn (f a)
mapTree f (NodeTrapped a)    = NodeTrapped (f a)
mapTree f (NodeTerminal a)   = NodeTerminal (f a)
mapTree f NodeStaticError    = NodeStaticError

instance Functor TreeEdge where
  fmap f (PteInstr x t)          = PteInstr x (mapTree f t)
  fmap f (PteIf ft ifT elseT)    = PteIf ft (mapTree f ifT) (mapTree f elseT)
  fmap f (PteCall fIdx t)        = PteCall fIdx (mapTree f t)
  fmap f (PteCallIndirect ft t)  = PteCallIndirect ft (mapTree f t)
  fmap f (PteBr lIdx)            = PteBr lIdx
  fmap f (PteBrIf lIdx t)        = PteBrIf lIdx (mapTree f t)
  fmap f (PteBrTable lIdxs lIdx) = PteBrTable lIdxs lIdx
  fmap f (PteKeepDrop kv dv t)   = PteKeepDrop kv dv (mapTree f t)


-- -- ## Show ##

-- instance Show node => Show (Edge node) where
--   showsPrec d (PeInstr instr nextI) =
--     showParen (d > 10) $ showString "I " . showsPrec 11 instr . showString " " . shows nextI
--   showsPrec d (PeIf (dropKeepSet, ifI) elseI) =
--     case showsKeepDrop dropKeepSet of
--       Just fShowDk -> 
--         showParen (d > 10) $ showString "If (" . fShowDk . showString "," . shows ifI . showString ") " . shows elseI
--       Nothing ->
--         showParen (d > 10) $ showString "If " . shows ifI . showString " " . shows elseI
--   showsPrec d (PeTable numKeep lblIs lblI) =
--     showParen (d > 10) $ showString "Table " . maybeShow (showsKeep numKeep `justSuffix` showString " ") . showList lblIs . showString " " . shows lblI
--   showsPrec d (PeCall fIdx nextI) =
--     showParen (d > 10) $ showString "Call " . shows fIdx . showString " " . shows nextI
--   showsPrec d (PeCallIndirect ft nextI) =
--     showParen (d > 10) $ showString "CallIndirect " . showsPrec 11 ft . showString " " . shows nextI
--   showsPrec d (PeDropMany numKeep droppedVals nextI) =
--     showParen (d > 10) $ showString "DropMany " . maybeShow (showsKeepDrop (DKKeep numKeep droppedVals) `justSuffix` showString " ") . shows nextI

-- showsKeepDrop :: DropKeep -> Maybe ShowS
-- showsKeepDrop DKKeepAll = Nothing
-- showsKeepDrop (DKKeep keptVals droppedVals) =
--   Just (maybeShow (showsKeep keptVals `justSuffix` showString " ") . showsDrop droppedVals)

-- showsKeep :: KeptVals -> Maybe ShowS
-- showsKeep xs = Just (showString "Keep=" . showList xs)

-- showsDrop :: DroppedValsNE -> ShowS
-- showsDrop (x :| xs) = showString "Drop=" . showList (x:xs)

-- justSuffix :: Maybe ( b -> c ) -> ( a -> b ) -> Maybe ( a -> c )
-- justSuffix a b = (.b) <$> a

-- justPrefix :: ( b -> c ) -> Maybe ( a -> b ) -> Maybe ( a -> c )
-- justPrefix a b = (a.) <$> b

-- maybeShow :: Maybe ShowS -> ShowS
-- maybeShow Nothing  = id
-- maybeShow (Just s) = s
