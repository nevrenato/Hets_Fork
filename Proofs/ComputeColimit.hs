{- |
Module      :  $Header$
Description :  Heterogeneous colimit of the displayed graph
Copyright   :  (c) Mihai Codescu, and Uni Bremen 2002-2006
License     :  GPLv2 or higher, see LICENSE.txt
Maintainer  :  mcodescu@informatik.uni-bremen.de
Stability   :  provisional
Portability :  non-portable

Computes the colimit and displays the graph after its insertion.
Improvements:

 - error messages when the algorithm fails to compute
 - insert edges just from a subset of nodes in the original graph
-}

module Proofs.ComputeColimit where

import Static.ComputeTheory
import Static.DevGraph
import Static.DgUtils
import Static.GTheory
import Static.History
import Static.WACocone

import Logic.Comorphism (mkIdComorphism)
import Logic.Grothendieck
import Logic.Logic

import Common.ExtSign
import Common.LibName
import Common.Result
import Common.SFKT
import Common.Id
import Common.IRI (simpleIdToIRI)
import Common.Utils (nubOrd)

import qualified Data.Map as Map
import Data.Graph.Inductive.Graph

computeColimit :: LibName -> LibEnv -> Result LibEnv
computeColimit ln le = do
  let dgraph = lookupDGraph ln le
  nextDGraph <- insertColimitInGraph le dgraph
  return $ Map.insert ln nextDGraph le

insertColimitInGraph :: LibEnv -> DGraph -> Result DGraph
insertColimitInGraph le dgraph = do
  let diag = makeDiagram dgraph (nodesDG dgraph) $ labEdgesDG dgraph
  (gth, morFun) <- gWeaklyAmalgamableCocone diag
  let -- a better node name, gn_Signature_Colimit?
      newNode = newInfoNodeLab (makeName $ simpleIdToIRI $ genToken "Colimit")
                               (newNodeInfo DGProof) gth
      newNodeNr = getNewNodeDG dgraph
      edgeList = map (\n -> (n, newNodeNr, globDefLink
                      (morFun Map.! n) SeeTarget))
                   $ nodes $ dgBody dgraph
      changes  = InsertNode (newNodeNr, newNode) : map InsertEdge edgeList
      newDg = changesDGH dgraph changes
      newGraph = changeDGH newDg $ SetNodeLab newNode
        (newNodeNr, newNode
        { globalTheory = computeLabelTheory le newDg (newNodeNr, newNode) })
  return $ groupHistory dgraph (DGRule "Compute-Colimit") newGraph

{- | creates an GDiagram with the signatures of the given nodes as nodes
   and the morphisms of the given edges as edges -}
makeDiagram :: DGraph -> [Node] -> [LEdge DGLinkLab] -> GDiagram
makeDiagram dg nl el = makeDiagramAux empty dg (nubOrd nl) el

{- | auxiliary method for makeDiagram: first translates all nodes then
   all edges, the descriptors of the nodes are kept in order to make
   retranslation easier -}
makeDiagramAux :: GDiagram -> DGraph -> [Node] -> [LEdge DGLinkLab] -> GDiagram
makeDiagramAux diagram _ [] [] = diagram
makeDiagramAux diagram dgraph [] ((src, tgt, labl) : list) =
  makeDiagramAux (insEdge morphEdge diagram) dgraph [] list
    where EdgeId x = dgl_id labl
          morphEdge = if isHidingDef $ dgl_type labl
                      then (tgt,src,(x,dgl_morphism labl))
                      else (src,tgt,(x,dgl_morphism labl))

makeDiagramAux diagram dgraph (node:list) es =
  makeDiagramAux (insNode sigNode diagram) dgraph list es
    where sigNode = (node, dgn_theory $ labDG dgraph node)

-- | weakly amalgamable cocones
gWeaklyAmalgamableCocone :: GDiagram
                         -> Result (G_theory, Map.Map Int GMorphism)
gWeaklyAmalgamableCocone diag =
 if isHomogeneousGDiagram diag then do
  case head $ labNodes diag of
   (_, G_theory lid _ _ _ _) -> do
    graph <- homogeniseGDiagram lid diag
    (sig, mor) <- weakly_amalgamable_colimit lid graph
    let gth = noSensGTheory lid (mkExtSign sig) startSigId
        cid = mkIdComorphism lid (top_sublogic lid)
        morFun = Map.fromList $
         map (\(n, s)->(n, GMorphism cid (mkExtSign s) startSigId
                             (mor Map.! n) startMorId)) $
         labNodes graph
    return (gth, morFun)
 else if not $ isConnected diag then fail "Graph is not connected"
      else if not $ isThin $ removeIdentities diag then
           fail "Graph is not thin" else do
             let funDesc = initDescList diag
             --graph <- observe $ hetWeakAmalgCocone diag funDesc
             allGraphs <- runM Nothing $ hetWeakAmalgCocone diag funDesc
             case allGraphs of
              [] -> fail "could not compute cocone"
              _ -> do
               let graph = head allGraphs
               -- TO DO: modify this function so it would return
               -- all possible answers and offer them as choices to the user
               buildStrMorphisms diag graph
