{- |
Module      :  $Header$
Description :  compute the theory of a node
Copyright   :  (c) Christian Maeder and DFKI GmbH 2009
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(Logic)

compute the theory of a node
-}

module Static.ComputeTheory
    ( computeTheory
    , globalNodeTheory
    , getGlobalTheory
    , theoremsToAxioms
    , computeDGraphTheories
    , computeLibEnvTheories
    , computeLabelTheory
    , updateLabelTheory
    , markHiding
    , markFree
    , renumberDGLinks
    , getImportNames
    ) where

import Logic.Prover

import Static.GTheory
import Static.DevGraph
import Static.DgUtils
import Static.History

import Common.LibName
import Common.Result

import Data.Graph.Inductive.Graph as Graph
import Data.List (sortBy)
import qualified Data.Map as Map
import qualified Data.Set as Set (fromList, toList)
import Data.Ord

-- * nodes with incoming hiding definition links

nodeHasHiding :: DGraph -> Node -> Bool
nodeHasHiding dg = labelHasHiding . labDG dg

nodeHasFree :: DGraph -> Node -> Bool
nodeHasFree dg = labelHasFree . labDG dg

{- | mark all nodes if they have incoming hiding edges.
   Assume reference nodes to other libraries being properly marked already.
-}
markFree :: LibEnv -> DGraph -> DGraph
markFree le dgraph =
  foldl (\ dg (n, lbl) -> let
     ingoingEdges = innDG dg n
     defEdges = filter (liftE isDefEdge) ingoingEdges
     freeDefEdges = filter (liftE isFreeEdge ) defEdges
     in fst $ labelNodeDG (n, lbl { labelHasFree =
            if isDGRef lbl
            then nodeHasFree (lookupDGraph (dgn_libname lbl) le)
                 $ dgn_node lbl
            else not (null freeDefEdges) }) dg)
     dgraph $ topsortedNodes dgraph

markHiding :: LibEnv -> DGraph -> DGraph
markHiding le dgraph =
  foldl (\ dg (n, lbl) -> let
     ingoingEdges = innDG dg n
     defEdges = filter (liftE isDefEdge) ingoingEdges
     hidingDefEdges = filter (liftE isHidingDef ) defEdges
     next = map (\ (s, _, _) -> s) defEdges
     in fst $ labelNodeDG (n, lbl { labelHasHiding =
            if isDGRef lbl
            then nodeHasHiding (lookupDGraph (dgn_libname lbl) le)
                 $ dgn_node lbl
            else not (null hidingDefEdges) || any (nodeHasHiding dg) next }) dg)
     dgraph $ topsortedNodes dgraph

computeTheory :: LibEnv -> LibName -> Node -> Maybe G_theory
computeTheory libEnv ln = globalNodeTheory $ lookupDGraph ln libEnv

theoremsToAxioms :: G_theory -> G_theory
theoremsToAxioms (G_theory lid sign ind1 sens ind2) =
     G_theory lid sign ind1 (markAsAxiom True sens) ind2

getGlobalTheory :: DGNodeLab -> Result G_theory
getGlobalTheory = maybe (fail "no global theory") return . globalTheory

globalNodeTheory :: DGraph -> Node -> Maybe G_theory
globalNodeTheory dg = globalTheory . labDG dg

computeLibEnvTheories :: LibEnv -> LibEnv
computeLibEnvTheories le =
    let lns = getTopsortedLibs le
        upd le' ln = let dg0 = lookupDGraph ln le
                         dg = computeDGraphTheories le' dg0
                     in Map.insert ln dg le'
    in foldl upd Map.empty lns

computeDGraphTheories :: LibEnv -> DGraph -> DGraph
computeDGraphTheories le dgraph =
  let newDg = computeDGraphTheoriesAux le dgraph
  in groupHistory dgraph (DGRule "Compute theory") newDg

recomputeNodeLabel :: LibEnv -> DGraph -> LNode DGNodeLab -> DGNodeLab
recomputeNodeLabel le dg l@(n, lbl) =
  case computeLabelTheory le dg l of
    gTh@(Just th) ->
      let (chg, lbl1) = case globalTheory lbl of
            Nothing -> (False, lbl)
            Just oTh -> (True, lbl
              { dgn_theory = invalidateProofs oTh th $ dgn_theory lbl })
          ngTh = if chg then computeLabelTheory le dg (n, lbl1) else gTh
      in case ngTh of
        Just nth@(G_theory _ _ _ sens _) ->
          (if Map.null sens then markNodeConsistent "ByNoSentences" lbl1
           else lbl1 { dgn_theory = proveLocalSens nth (dgn_theory lbl1) })
           { globalTheory = ngTh }
        Nothing -> lbl1
    Nothing -> lbl

computeDGraphTheoriesAux :: LibEnv -> DGraph -> DGraph
computeDGraphTheoriesAux le dgraph =
  foldl (\ dg l@(n, lbl) -> updatePending dg lbl
    (n, recomputeNodeLabel le dg l))
     dgraph $ topsortedNodes dgraph

computeLabelTheory :: LibEnv -> DGraph -> LNode DGNodeLab -> Maybe G_theory
computeLabelTheory le dg (n, lbl) = let localTh = dgn_theory lbl in
    fmap reduceTheory . maybeResult $ if isDGRef lbl then
        case Map.lookup (dgn_libname lbl) le of
          Nothing -> return localTh -- do not crash here
          Just dg' -> do
            refTh <- getGlobalTheory $ labDG dg' $ dgn_node lbl
            joinG_sentences refTh localTh
    else do
      ths <- mapM (\ (s, _, l) -> do
         th <- let sl = labDG dg s in if isLocalDef $ dgl_type l then
                   return $ dgn_theory sl
               else getGlobalTheory sl
         -- translate theory and turn all imported theorems into axioms
         translateG_theory (dgl_morphism l) $ theoremsToAxioms th)
         $ sortBy
            (flip $ comparing (\ (_, _, l) -> dgl_id l))
            $ getImports dg n
      flatG_sentences localTh ths

getImports :: DGraph -> Node -> [LEdge DGLinkLab]
getImports dg = filter (liftE $ liftOr isGlobalDef isLocalDef) . innDG dg

getImportNames :: DGraph -> Node -> [String]
getImportNames dg = map (\ (s, _, _) -> getDGNodeName $ labDG dg s)
  . getImports dg

reduceTheory :: G_theory -> G_theory
reduceTheory (G_theory lid sig ind sens _) =
  G_theory lid sig ind (reduceSens sens) startThId

updateLabelTheory :: LibEnv -> DGraph -> LNode DGNodeLab -> G_theory -> DGraph
updateLabelTheory le dg (i, l) gth = let
    l' = l { dgn_theory = gth }
    n = (i, l' { globalTheory = computeLabelTheory le dg (i, l') })
    in updatePending dg l n

updatePending :: DGraph
  -> DGNodeLab -- ^ old label
  -> LNode DGNodeLab -- ^ node with new label
  -> DGraph
updatePending dg l n = let
    dg0 = changeDGH dg $ SetNodeLab l n
    dg1 = togglePending dg0 $ changedLocalTheorems dg0 n
    dg2 = togglePending dg1 $ changedPendingEdges dg1
    in groupHistory dg (DGRule "Node proof") dg2

{- | iterate all edgeId entries of the dg and increase all that are equal or
above the old lId (1st param) so they will be above the desired nextLId
(2nd param) -}
renumberDGLinks :: EdgeId -> EdgeId -> DGraph -> DGraph
renumberDGLinks (EdgeId i1) (EdgeId i2) dg = if i1 >= i2 then dg else
  changesDGH dg $ concatMap mkRenumberChange $ labEdgesDG dg where
  needUpd (EdgeId x) = x >= i1
  offset = i2 - i1
  add (EdgeId x) = EdgeId $ x + offset
  mkRenumberChange e@(s, t, l) = let
    ei = dgl_id l
    -- update links own id if required
    (upd, newId) = let b = needUpd ei in (b, if b then add ei else ei)
    -- make updates to links proof basis if required
    (upd', newTp) = let
      pB = getProofBasis l
      (b, pBids) = foldl (\ (bR, eiR) ei' -> let bi = needUpd ei' in
        (bR || bi, if bi then add ei' : eiR else ei' : eiR))
          (False, []) $ Set.toList $ proofBasis pB
      in (b, (if b then flip updThmProofBasis
        . ProofBasis $ Set.fromList pBids else id) $ dgl_type l)
    -- update in dg unless no updates conducted
    in if upd || upd' then [DeleteEdge e, InsertEdge (s, t, l
      { dgl_id = newId, dgl_type = newTp })] else []
