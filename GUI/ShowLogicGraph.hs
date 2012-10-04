{- |
Module      :  $Header$
Copyright   :  (c) Heng Jiang and Till Mossakowski, Uni Bremen 2002-2006
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable (Logic)

display the logic graph
-}

module GUI.ShowLogicGraph (showLogicGraph, showLG) where

import GUI.UDGUtils as UDG
import GUI.HTkUtils

import Comorphisms.LogicGraph
import Comorphisms.HetLogicGraph
import Logic.Grothendieck
import Logic.Logic
import Logic.Comorphism
import Logic.Prover

import qualified Data.Map as Map
import Data.List
import qualified Common.Lib.Rel as Rel

import Data.Typeable

graphParms :: GraphAllConfig graph graphParms node nodeType nodeTypeParms
                              arc arcType arcTypeParms
           => Graph graph graphParms node nodeType nodeTypeParms
                        arc arcType arcTypeParms
           -> String -- ^ title of graph
           -> graphParms
graphParms _ title =
             GraphTitle title $$
             OptimiseLayout True $$
             AllowClose (return True) $$
             emptyGraphParms

makeNodeMenu :: (GraphAllConfig graph graphParms node
                                    nodeType nodeTypeParms
                                     arc arcType arcTypeParms,
                       Typeable value)
                  => Graph graph graphParms node nodeType nodeTypeParms
                        arc arcType arcTypeParms
                  -> (value -> IO String)
                  -- ^ display the value as a String
                  -> LocalMenu value
                  -> String -- ^ color of node
                  -> nodeTypeParms value
makeNodeMenu _ showMyValue logicNodeMenu color =
               logicNodeMenu $$$
               Ellipse $$$
               ValueTitle showMyValue $$$
               Color color $$$
               emptyNodeTypeParms

stableColor, testingColor, unstableColor, experimentalColor,
   proverColor :: String
stableColor = "#00FF00"
testingColor = "#88FF33"
unstableColor = "#CCFF66"
experimentalColor = "white"
proverColor = "lightsteelblue"

normalArcColor :: String
normalArcColor = "black"

inclusionArcColor :: String
inclusionArcColor = "blue"

-- | Test whether a comorphism is an ad-hoc inclusion
isInclComorphism :: AnyComorphism -> Bool
isInclComorphism (Comorphism cid) =
    Logic (sourceLogic cid) == Logic (targetLogic cid) &&
    isProperSublogic (G_sublogics (sourceLogic cid) (sourceSublogic cid))
                      (G_sublogics (targetLogic cid) (targetSublogic cid))

showLogicGraph ::
   GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms
   => Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms
   -> IO ()
showLogicGraph displaySrt = do
           -- disp s tD = debug (s ++ (show tD))
       logicG <- newGraph displaySrt (GlobalMenu (UDG.Menu Nothing [
                Button "Show detailed logic graph" showHSG ]) $$
                                      graphParms displaySrt "Logic Graph"
                                     )
       let logicNodeMenu = LocalMenu (UDG.Menu (Just "Info")
               [Button "Tools" (\ lg -> createTextDisplay
                      ("Parsers, Provers and Cons_Checker of "
                       ++ show lg) (showTools lg) [size (80, 25)]),
                Button "Sublogic" (\ lg -> createTextDisplay ("Sublogics of "
                       ++ show lg) (showSublogic lg) [size (80, 25)]),
                Button "Sublogic Graph" showSubLogicGraph,
                Button "Description" (\ lg -> createTextDisplay
                      ("Description of " ++ show lg) (showDescription lg)
                                      [size (83, 25)])
               ])
           makeLogicNodeMenu = makeNodeMenu displaySrt (return . show)
                            logicNodeMenu
       stableNodeType <- newNodeType logicG $ makeLogicNodeMenu stableColor
       testingNodeType <- newNodeType logicG $ makeLogicNodeMenu testingColor
       unstableNodeType <- newNodeType logicG $ makeLogicNodeMenu unstableColor
       experimentalNodeType <- newNodeType logicG $
                               makeLogicNodeMenu experimentalColor
       proverNodeType <-
           newNodeType logicG $ makeLogicNodeMenu proverColor
       let newNode' logic =
             case logic of
               Logic lid -> if null $ provers lid then let
                   nodeType = case stability lid of
                     Stable -> stableNodeType
                     Testing -> testingNodeType
                     Unstable -> unstableNodeType
                     Experimental -> experimentalNodeType
                   in newNode logicG nodeType logic
                 else newNode logicG proverNodeType logic

       -- production of the nodes (in a list)
       nodeList <- mapM newNode' (Map.elems (logics logicGraph))
       -- build the map with the node's name and the node.
       let namesAndNodes = Map.fromList (zip (Map.keys (logics logicGraph))
                                             nodeList)
           lookupLogi (Logic lid) =
               Map.findWithDefault (error "lookupLogi: Logic not found")
                                   (language_name lid)
                                   namesAndNodes
           {- each edge can also show the informations (the
             description of comorphism and names of
             source/target-Logic as well as the sublogics). -}
           logicArcMenu =
               LocalMenu (Button "Info"
                 (\ c -> createTextDisplay (show c) (showComoDescription c)
                              [size (80, 25)]))
           normalArcTypeParms = logicArcMenu $$$         -- normal comorphism
                                Color normalArcColor $$$
                                ValueTitle (\ c -> case c of
                                    Comorphism cid ->
                                        return $ language_name cid) $$$
                                emptyArcTypeParms
           inclArcTypeParms = logicArcMenu $$$           -- inclusion
                               Color inclusionArcColor $$$
                               emptyArcTypeParms
       normalArcType <- newArcType logicG normalArcTypeParms
       inclArcType <- newArcType logicG inclArcTypeParms
       let insertComo =            -- for cormophism
             (\ c ->
                case c of
                  Comorphism cid ->
                    let sid = Logic (sourceLogic cid)
                        tid = Logic (targetLogic cid)
                    in newArc logicG normalArcType c (lookupLogi sid)
                            (lookupLogi tid))
           insertIncl =            -- for inclusion
             (\ i ->
                case i of
                  Comorphism cid ->
                    let sid = Logic (sourceLogic cid)
                        tid = Logic (targetLogic cid)
                    in newArc logicG inclArcType i (lookupLogi sid)
                            (lookupLogi tid))
       mapM_ insertIncl inclusionList
       mapM_ insertComo $ filter (`notElem` inclusionList) comorphismList
       redraw logicG
       where
        showSubLogicGraph subl =
          case subl of
            Logic sublid ->
              do subLogicG <- newGraph displaySrt
                                     (graphParms displaySrt "SubLogic Graph")
                 let listG_Sublogics = -- map (\sbl -> G_sublogics sublid sbl)
                                       (all_sublogics sublid)
                     subNodeMenu = LocalMenu (UDG.Menu Nothing [])
                     subNodeTypeParms =
                         subNodeMenu $$$
                         Ellipse $$$
                         ValueTitle (return . sublogicName) $$$
                         Color "yellow" $$$
                         emptyNodeTypeParms
                 subNodeType <- newNodeType subLogicG subNodeTypeParms
                 subNodeList <- mapM (newNode subLogicG subNodeType)
                                listG_Sublogics
                 let slAndNodes = Map.fromList $
                                  zip listG_Sublogics subNodeList
                     lookupSublogic g_sl =
                         Map.findWithDefault
                              (error "lookupSublogic: node not found")
                              g_sl slAndNodes
                     subArcMenu = LocalMenu (UDG.Menu Nothing [])
                     subArcTypeParms = subArcMenu $$$
                                       Color "green" $$$
                                       emptyArcTypeParms
                 subArcType <- newArcType subLogicG subArcTypeParms
                 let insertSubArc (node1, node2) =
                           newArc subLogicG subArcType ""
                                  (lookupSublogic node1)
                                  (lookupSublogic node2)
                     subl_nodes =
                         Rel.toList $ Rel.intransKernel $ Rel.fromList
                          [ (g1, g2)
                            | g1 <- listG_Sublogics
                            , g2 <- listG_Sublogics
                            , g1 /= g2
                            , isSubElem g1 g2
                          ]
                 mapM_ insertSubArc subl_nodes
                 redraw subLogicG

showHetSublogicGraph ::
   GraphAllConfig graph graphParms node nodeType nodeTypeParms
      arc arcType arcTypeParms
   => Graph graph graphParms node nodeType nodeTypeParms
         arc arcType arcTypeParms
   -> IO ()
showHetSublogicGraph displaySrt = do
       logicG <- newGraph displaySrt (graphParms displaySrt
                                                "Heterogeneous Sublogic Graph")
       let logicNodeMenu = LocalMenu (UDG.Menu (Just "Info")
               [Button "Tools" (\ lg -> createTextDisplay
                      ("Parsers, Provers and Cons_Checker of "
                       ++ show lg) (showTools $ toAnyLogic lg) [size (80, 25)]),
                Button "Sublogic" (\ lg -> createTextDisplay ("Sublogics of "
                       ++ show lg) (showSublogic $ toAnyLogic lg)
                                   [size (80, 25)]),
                Button "Description" (\ lg -> createTextDisplay
                      ("Description of " ++ show lg)
                        (showDescription $ toAnyLogic lg)
                                      [size (83, 25)])
               ])
           makeLogicNodeMenu = makeNodeMenu displaySrt (return . show)
               logicNodeMenu
       stableNodeType <- newNodeType logicG $ makeLogicNodeMenu stableColor
       testingNodeType <- newNodeType logicG $ makeLogicNodeMenu testingColor
       unstableNodeType <- newNodeType logicG $ makeLogicNodeMenu unstableColor
       experimentalNodeType <-
           newNodeType logicG $ makeLogicNodeMenu experimentalColor
       proverNodeType <-
           newNodeType logicG $ makeLogicNodeMenu proverColor
       let newNode' gsl@(G_sublogics lid _) =
               if not $ null $ provers lid
               then newNode logicG proverNodeType gsl
               else let nodeType = case stability lid of
                                     Stable -> stableNodeType
                                     Testing -> testingNodeType
                                     Unstable -> unstableNodeType
                                     Experimental -> experimentalNodeType
                    in newNode logicG nodeType gsl

       -- production of the nodes (in a list)
       nodeList <- mapM newNode' (Map.elems (sublogicNodes hetSublogicGraph))
       -- build the map with the node's name and the node.
       let namesAndNodes =
               Map.fromList (zip (Map.keys (sublogicNodes hetSublogicGraph))
                                 nodeList)
           lookupLogi gslStr =
               Map.findWithDefault (error "lookupLogi: Logic not found")
                                   gslStr
                                   namesAndNodes
           {- each edge can also show the informations (the
             description of comorphism and names of
             source/target-Logic as well as the sublogics). -}
           logicArcMenu =
               LocalMenu (Button "Info"
                 (\ c -> createTextDisplay (show c) (showComoDescription c)
                  [size (80, 25)]))
           acmName = (\ (Comorphism cid) -> return $ language_name cid)
           normalArcTypeParms = logicArcMenu $$$         -- normal comorphism
                                Color normalArcColor $$$
                                ValueTitle acmName $$$
                                emptyArcTypeParms
           inclArcTypeParms = logicArcMenu $$$           -- inclusion
                               Color inclusionArcColor $$$
                               ValueTitle acmName $$$
                               emptyArcTypeParms
           adhocInclArcTypeParms =
                            Color inclusionArcColor $$$ -- ad-hoc inclusion
                            emptyArcTypeParms
       normalArcType <- newArcType logicG normalArcTypeParms
       inclArcType <- newArcType logicG inclArcTypeParms
       adhocInclArcType <- newArcType logicG adhocInclArcTypeParms
       let insertArcType tp =
             (\ ((src, trg), acm) ->
                  newArc logicG tp acm (lookupLogi src)
                            (lookupLogi trg))
           (inclCom, notInclCom) =
               partition ((`elem` inclusionList) . snd) $
               concatMap (\ (x, ys) -> zip (repeat x) ys) $
                   Map.toList -- [((String,String),[AnyComorphism])]
                          (comorphismEdges hetSublogicGraph)
           (adhocCom, normalCom) =
               partition (isInclComorphism . snd) notInclCom
       mapM_ (insertArcType inclArcType) inclCom
       mapM_ (insertArcType adhocInclArcType) adhocCom
       mapM_ (insertArcType normalArcType) normalCom
       redraw logicG


toAnyLogic :: G_sublogics -> AnyLogic
toAnyLogic (G_sublogics lid _) = Logic lid

showSublogic :: AnyLogic -> String
showSublogic (Logic lid) = unlines (map sublogicName (all_sublogics lid))

showSubTitle :: G_sublogics -> String
showSubTitle (G_sublogics _ lid) = sublogicName lid

showDescription :: AnyLogic -> String
showDescription (Logic lid) =
    description lid ++ "\n\nStability: " ++ show (stability lid)

showComoDescription :: AnyComorphism -> String
showComoDescription (Comorphism cid) =
  let ssid = G_sublogics (sourceLogic cid) (sourceSublogic cid)
      tsid = G_sublogics (targetLogic cid) (targetSublogic cid)
  in description cid ++ "\n\n"
  ++ "source logic:     " ++ language_name (sourceLogic cid) ++ "\n\n"
  ++ "target logic:     " ++ language_name (targetLogic cid) ++ "\n"
  ++ "source sublogic:  " ++ showSubTitle ssid ++ "\n"
  ++ "target sublogic:  " ++ showSubTitle tsid

showTools :: AnyLogic -> String
showTools (Logic li) =
    case parse_basic_spec li of
      Just _ -> "Parser for basic specifications.\n"
      Nothing -> ""
  ++ case parse_symb_items li of
       Just _ -> "Parser for symbol lists.\n"
       Nothing -> ""
  ++ case parse_symb_map_items li of
       Just _ -> "Parser for symbol maps.\n"
       Nothing -> ""
  ++ case basic_analysis li of
       Just _ -> "Analysis of basic specifications.\n"
       Nothing -> ""
  ++ case data_logic li of
       Just _ -> "is a process logic.\n"
       Nothing -> ""
  ++ "\nProvers: "
  ++ case provers li of
       [] -> "None"
       ls -> unlines $ map proverName ls
  ++ "\nConsistency checkers: "
  ++ case cons_checkers li of
       [] -> "None"
       ls -> unlines $ map ccName ls

showHSG :: IO ()
showHSG = showHetSublogicGraph daVinciSort

showLG :: IO ()
showLG = showLogicGraph daVinciSort
