{-# LANGUAGE ExistentialQuantification #-}
{- |
Module      :  $Header$
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  nevrenato@gmail.com 
Stability   :  provisional
Portability :  portable


Description  :
List of logics that can be under TopHybrid
-}

module TopHybrid.UnderLogicList where

-- To access AnyLogic datatype
import Logic.Logic
-- Logics 
import Modal.Logic_Modal
import Hybrid.Logic_Hybrid
import CASL.Logic_CASL
-- Others
import qualified Data.Map as M
import Data.Maybe

-- Under logics supported
underlogics :: M.Map String AnyLogic
underlogics =  M.insert (show CASL) (Logic CASL)
               $ M.insert (show Hybrid) (Logic Hybrid) 
               $ M.insert (show Modal) (Logic Modal) M.empty

getLogic :: String -> AnyLogic
getLogic s = let x = M.lookup s underlogics in
        if isNothing x then error $ "hey, the logic " ++ s ++ " doesn't exist or isn't available for that kind of use"
        else fromJust x
