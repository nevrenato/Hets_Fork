{- |
Module      :$Header$
Description : utilitary functions used throughout the CMDL interface
Copyright   : uni-bremen and DFKI
License     : GPLv2 or higher, see LICENSE.txt
Maintainer  : r.pascanu@jacobs-university.de
Stability   : provisional
Portability : portable

CMDL.Utils contains different basic functions that are
used throughout the CMDL interface and could not be found in
Prelude

-}

module CMDL.DataTypesUtils
  ( obtainGoalNodeList
  , getAllGoalNodes
  , getAllGoalEdges
  , getSelectedDGNodes
  , getInputDGNodes
  , getInputNodes
  , getTh
  , baseChannels
  , genErrorMsg
  , genMessage
  , generatePrompter
  , add2hist
  , getIdComorphism
  ) where

import Interfaces.Command (Command (CommentCmd))
import Interfaces.DataTypes
import Interfaces.History (add2history)
import Interfaces.Utils (getAllEdges, getAllNodes)
import CMDL.Utils
import CMDL.DataTypes

import Static.GTheory (G_theory, mapG_theory)
import Static.DevGraph (DGNodeLab, DGLinkLab, lookupDGraph, labDG)

import System.IO (stdout, stdin)

import Proofs.AbstractState (sublogicOfTheory, theoryName)
import Static.ComputeTheory (computeTheory)

import Data.Graph.Inductive.Graph (LNode, LEdge, Node)
import Data.List (find)

import Common.Result (Result (Result))

import Logic.Comorphism (AnyComorphism (..), mkIdComorphism)
import Logic.Grothendieck (G_sublogics (..))

add2hist :: [UndoRedoElem] -> CmdlState -> CmdlState
add2hist descr st
 = let intst = add2history (CommentCmd "") (intState st) descr
   in st { intState = intst }

{- | Given a list of selected theory generate an Id comorphism to the
first selected theory -}
getIdComorphism :: [Int_NodeInfo] -> Maybe AnyComorphism
getIdComorphism ls = case ls of
    [] -> Nothing
    Element st _ : _ ->
       case sublogicOfTheory st of
         G_sublogics lid sub -> Just $ Comorphism (mkIdComorphism lid sub)

-- | Generates the string containing the prompter
generatePrompter :: CmdlState -> String
generatePrompter st = let pst = prompter st in
  (case i_state $ intState st of
    Nothing -> ""
    Just ist ->
     let els = case elements ist of
                [] -> delExtension (fileLoaded pst)
                Element sm _ : r -> theoryName sm ++ if null r then "" else ".."
         cm = case elements ist of
                [] -> ""
                es -> if cComorphism ist /= getIdComorphism es
                       then "*"
                       else ""
     in els ++ cm) ++ prompterHead pst

{- | Given a list of node names and the list of all nodes
the function returns all the nodes that have their name
in the name list but are also goals -}
obtainGoalNodeList :: CmdlState -> [String] -> [LNode DGNodeLab]
                                 -> ([String], [LNode DGNodeLab])
obtainGoalNodeList state input ls
 = let (l1, l2) = obtainNodeList input ls
       l2' = filter (\ (nb, nd) ->
                       let nwth = getTh Dont_translate nb state
                       in case nwth of
                           Nothing -> False
                           Just th -> nodeContainsGoals (nb, nd) th) l2
   in (l1, l2')


{- | Returns the list of all nodes that are goals,
taking care of the up to date status -}
getAllGoalNodes :: CmdlState -> [LNode DGNodeLab]
getAllGoalNodes st
 = case i_state $ intState st of
    Nothing -> []
    Just ist ->
      filter (\ (nb, nd) ->
             let nwth = getTh Dont_translate nb st
             in case nwth of
                 Nothing -> False
                 Just th -> nodeContainsGoals (nb, nd) th) $
                                 getAllNodes ist

{- | Returns the list of all goal edges taking care of the
up to date status -}
getAllGoalEdges :: CmdlState -> [LEdge DGLinkLab]
getAllGoalEdges st
 = case i_state $ intState st of
    Nothing -> []
    Just ist ->
      filter edgeContainsGoals $ getAllEdges ist

-- Returns the selected DGNodes along with a possible error message
getSelectedDGNodes :: IntIState -> (String, [LNode DGNodeLab])
getSelectedDGNodes dgState =
  let nds = map (\ (Element _ n) -> n) $ elements dgState
      dg = lookupDGraph (i_ln dgState) (i_libEnv dgState)
      nds' = zip nds $ map (labDG dg) nds
   in (if null nds' then "No node(s) selected!" else "", nds')

{- Returns the selected DGNodes
or if the selection is empty the DGNodes specified by the input string -}
getInputDGNodes :: String -> IntIState -> (String, [LNode DGNodeLab])
getInputDGNodes input dgState =
  if null input
    then getSelectedDGNodes dgState
    else let
        (nds, _, _, errs) = decomposeIntoGoals input
        tmpErrs = prettyPrintErrList errs
        in case nds of
               [] -> (tmpErrs, [])
               _ -> let
                   lsNodes = getAllNodes dgState
                   (errs', listNodes) = obtainNodeList nds lsNodes
                   tmpErrs' = tmpErrs ++ prettyPrintErrList errs'
                   in (tmpErrs', listNodes)

{- Returns the selected Nodes
or if the selection is empty the Nodes specified by the input string -}
getInputNodes :: String -> IntIState -> (String, [Node])
getInputNodes input dgState =
  let (errors, nodes) = getInputDGNodes input dgState
   in (errors, map fst nodes)

{- local function that computes the theory of a node
that takes into consideration translated theories in
the selection too and returns the theory as a string -}
getTh :: CmdlUseTranslation -> Int -> CmdlState -> Maybe G_theory
getTh useTrans x st
 = let
    -- compute the theory for a given node
       fn n = case i_state $ intState st of
               Nothing -> Nothing
               Just ist -> computeTheory (i_libEnv ist) (i_ln ist) n
   in
    case useTrans of
     Dont_translate -> fn x
     Do_translate ->
      case i_state $ intState st of
       Nothing -> Nothing
       Just ist ->
        case elements ist of
         [] -> fn x
         _ ->
          case find (\ y -> case y of
                          Element _ z -> z == x) $
                  elements ist of
           Nothing -> fn x
           Just _ ->
            case cComorphism ist of
             Nothing -> fn x
             Just cm ->
              case fn x of
               Nothing -> Nothing
               Just sth ->
                case mapG_theory cm sth of
                  Result _ Nothing -> Just sth
                  Result _ (Just sth') -> Just sth'


-- | Generates the base channels to be used (stdin and stdout)
baseChannels :: [CmdlChannel]
baseChannels
 = let ch_in = CmdlChannel {
                  chName = "stdin",
                  chType = ChStdin,
                  chHandler = stdin,
                  chSocket = Nothing,
                  chProperties = ChRead
                  }
       ch_out = CmdlChannel {
                  chName = "stdout",
                  chType = ChStdout,
                  chHandler = stdout,
                  chSocket = Nothing,
                  chProperties = ChWrite
                  }
   in [ch_in, ch_out]


genErrorMsg :: String -> CmdlState -> CmdlState
genErrorMsg msg st
 = st {
      output = CmdlMessage {
         outputMsg = [],
         warningMsg = [],
         errorMsg = msg
         }
     }

genMessage :: String -> String -> CmdlState -> CmdlState
genMessage warnings msg st
 = st {
      output = CmdlMessage {
        outputMsg = msg,
        warningMsg = warnings,
        errorMsg = []
        }
     }
