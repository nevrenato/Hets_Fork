{-# LANGUAGE ExistentialQuantification #-}

{- |
Module      :  $Header$
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  nevrenato@gmail.com 
Stability   :  provisional
Portability :  portable
Description  : This is the list of logics that can be hybridized
-}

module TopHybrid.UnderLogicList where

import Logic.Logic
-- Import of logics
import CASL.Logic_CASL
import Propositional.Logic_Propositional
import CoCASL.Logic_CoCASL
-- End of import of logics
import qualified Data.Map as M
import TopHybrid.Utilities

-- Logics supported
underlogicList :: [(String, AnyLogic)]
underlogicList = [ (show CASL, Logic CASL), (show Propositional, Logic Propositional),(show CoCASL, Logic CoCASL)]

-- Build of the underlogics map
underlogics :: M.Map String AnyLogic
underlogics =  buildMapFromList underlogicList
 
-- Tries to get a logic, if it fails, then
-- gives an error saying that the logic in question doesn't exist 
getLogic :: String -> AnyLogic
getLogic s = maybeE 3 $ M.lookup s underlogics
