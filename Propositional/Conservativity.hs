{- |
Module      :  $Header$
Description :  ConservativityChecker for propositional logic
Copyright   :  (c) Dominik Luecke, Uni Bremen 2007
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  luecke@informatik.uni-bremen.de
Stability   :  experimental
Portability :  portable

THe QBF solver sKizzo is used for conservativity checking. This is the
code connecting it to Hets
-}

module Propositional.Conservativity (conserCheck) where

import Common.AS_Annotation
import Common.Consistency
import Common.Id
import Common.Result
import Common.ProverTools
import Common.Utils (getTempFile)

import System.Directory
import System.Exit
import System.Process

import qualified Data.Map as Map
import qualified Data.Set as Set

-- Propositional Stuff
import Propositional.Sign
import Propositional.AS_BASIC_Propositional
import Propositional.Morphism
import Propositional.Conversions
import Propositional.Fold

proverName :: String
proverName = "sKizzo"

defOptions :: String
defOptions = "-timeout 60"

-- | Conservativity Check for Propositional Logic
conserCheck :: (Sign, [Named FORMULA])      -- ^ Initial sign and formulas
           -> Morphism                      -- ^ morhpism between specs
           -> [Named FORMULA]               -- ^ Formulas of extended spec
           -> IO (Result (Maybe (Conservativity, [FORMULA])))
conserCheck (_, inSens) mor cSens = do
      let cForms = getFormulas cSens
          inSig = source mor
          cSig = target mor
      case mapM (mapSentence mor) $ getFormulas inSens of
        Result ds Nothing -> return $ Result ds Nothing
        Result _ (Just inFormsM) -> do
          let checkForm = Implication
                 (Conjunction inFormsM nullRange)
                 (Conjunction cForms nullRange)
                 nullRange
          doConservCheck inSig cSig checkForm

-- | Extraction of needed formulas, removes all Axioms and Annotations
getFormulas :: [Named FORMULA] -> [FORMULA]
getFormulas = foldl (\ out sen -> out ++ [sentence sen | isAxiom sen]) []

doConservCheck :: Sign       -- ^ Initial  Sign
               -> Sign       -- ^ Extended Sign
               -> FORMULA    -- ^ QBF Formula to Prove
               -> IO (Result (Maybe (Conservativity, [FORMULA])))
doConservCheck inSig oSig form = do
             let iMap = createSignMap oSig 1 Map.empty
                 iSym = items inSig
                 eSym = items oSig `Set.difference` iSym
                 qdim = showQDimacs iSym eSym iMap $ getConj $ cnf form
             res <- runSKizzo qdim
             return (return (Just (res, [])))

-- | Printer for QDimacs Format
showQDimacs :: Set.Set Id               -- ^ Symbols of initial  Sign
            -> Set.Set Id               -- ^ New symbols of extended Sign
            -> Map.Map Token Integer    -- ^ Map of Symbols
            -> [FORMULA]          -- ^ Formulas to Translate
            -> String
showQDimacs inSym exSym sigMap fforms =
      let numVars = Set.size inSym + Set.size exSym
          fct sym str = show (Map.findWithDefault 0 (id2SimpleId sym) sigMap)
                        ++ " " ++ str
      in "c Conservativity Problem Created by Hets \n" ++
             "p cnf " ++ show numVars ++ " " ++
             show (length fforms) ++ "\n" ++
             "a " ++ Set.fold fct "" inSym ++ "0\n" ++
             "e " ++ Set.fold fct "" exSym ++ "0\n" ++
             case fforms of
                    [] -> ""
                    _ -> "c Axioms\n" ++ concatMap (`mapClause` sigMap)
                          fforms

-- | Runs sKizzo that has to reside in your path
runSKizzo :: String                  -- ^ File in qdimacs syntax
          -> IO Conservativity
runSKizzo qd = do
      noProg <- missingExecutableInPath proverName
      if noProg then
        return $ Unknown (proverName ++ " not found in your $PATH$")
        else do
              path <- getTempFile qd "sKizzoTemp.qdimacs"
              (exCode, _, _) <- readProcessWithExitCode proverName
                (words defOptions ++ [path]) ""
              removeFile path
              return $ case exCode of
                ExitFailure n -> case n of
                  10 -> Cons
                  20 -> Inconsistent
                  30 -> Unknown "Timeout"
                  40 -> Unknown "Cannot solve"
                  250 -> Unknown "Out of memory"
                  251 -> Unknown "sKizzo crashed"
                  254 -> Unknown "File not found"
                  -4 -> Unknown "Parse error"
                  -5 -> Unknown "sKizzo crashed"
                  -6 -> Unknown "Out of memory"
                  _ -> Unknown $ "Unkown, exit was: " ++ show n
                _ -> Unknown $ "Unkown, exit was: " ++ show exCode
