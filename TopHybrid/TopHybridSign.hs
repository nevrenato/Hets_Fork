{-# LANGUAGE ExistentialQuantification, StandaloneDeriving, DeriveDataTypeable #-}
{- |
Module      :  $Header$
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  nevrenato@gmail.com 
Stability   :  experimental
Portability :  portable

Descrption  :
Signature for hybrid logic with an arbitrary logic below
-}

module TopHybrid.TopHybridSign where

import TopHybrid.AS_TopHybrid
import Common.Id
import ATerm.Lib
import Data.Typeable

data Sign_Wrapper = forall s. (Show s, GetRange s, ShATermConvertible s) 
                        => Sign_Wrapper (THybridSign s)

data THybridSign s = THybridSign
  { 
    modies :: [MODALITY]
  , nomies :: [NOMINAL]
  , extended :: s
  } deriving (Show, Eq, Ord) 


emptyTHybridSign :: Sign_Wrapper
emptyTHybridSign = Sign_Wrapper s
        where s = THybridSign [] [] ()

isSubTHybridSign :: Sign_Wrapper -> Sign_Wrapper -> Bool
isSubTHybridSign (Sign_Wrapper s) (Sign_Wrapper s') = False
        













-- Boring instances needed for a valid progam, that DriFT cannot generate
deriving instance Show Sign_Wrapper
deriving instance Typeable Sign_Wrapper
instance Eq Sign_Wrapper where
        (==) = undefined
instance Ord Sign_Wrapper where
        compare = undefined  
----------------


