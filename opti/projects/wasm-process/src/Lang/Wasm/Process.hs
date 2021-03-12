
module Lang.Wasm.Process
  ( -- * Data Structures
    -- ** General
    -- Edge (..)
    Node (..)
    -- ** Tree
  , Tree (..)
  , TreeEdge (..)
    -- ** Graph
  , GraphEdge (..)
  , NodeId
  , GraphNode (..)
  , GraphMut
  , GraphFrozen
  -- , GraphNode (..)
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
    -- ** Flow
  , FlowEdge (..)
  , JmpTableIdx (..)
    -- ** Helpers
  , KeptVals
  -- , DroppedValsNE
  , DroppedVals

  , freezeGraph
  , unfreezeGraph

  -- , DropKeep (..)
    -- * Construction
  -- , newDropKeep
  -- , freezeMulti
  -- , freezeSingle
  , buildGraph
  , buildTree
  , treeToGraph
  , boundTree
  , graphToAst
  , graphFlow
  , graphFwdFlow
  , mapTree
  -- , buildTree
  -- , buildTreeFrozen
  -- , multiGraphFlow
  -- , singleGraphFlow
  --   -- * Deconstruction
  -- , reconstructSingle
  --   -- * Helpers
  -- , isNodeTerminal
  -- , isNodeTrapped
  -- , viewDropKeep
  -- , applyDropKeep
  -- , isEqType
  --   -- * TEMP
  -- , Instr
  , showTreeLines
  , showGraphLines
  , loopEntries
  ) where

import Lang.Wasm.Process.Structures
import Lang.Wasm.Process.Flow
import Lang.Wasm.Process.GraphConstruction
import Lang.Wasm.Process.TreeConstruction
import Lang.Wasm.Process.Reconstruct.Single

-- TEMP
import Lang.Wasm.Process.Reconstruct.Instr
