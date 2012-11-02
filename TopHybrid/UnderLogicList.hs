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

-- Import of Logics
import CASL.Logic_CASL

import qualified Data.Map as M
import Data.Maybe

-- Logics supported
underlogicList :: [(String, AnyLogic)]
underlogicList = [ (show CASL, Logic CASL) ]

underlogics :: M.Map String AnyLogic
underlogics =  buildMapFromList underlogicList
 
buildMapFromList :: (Ord a) => [(a,b)] -> (M.Map a b)
buildMapFromList = foldl (\y (x,x') -> M.insert x x' y) M.empty

getLogic :: String -> AnyLogic
getLogic s = let x = M.lookup s underlogics in
        if isNothing x then error $ "hey, the logic " ++ 
        s ++ 
        " doesn't exist or isn't available for that kind of use"
        else fromJust x
