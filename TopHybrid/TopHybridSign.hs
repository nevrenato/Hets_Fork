{-# LANGUAGE ExistentialQuantification, StandaloneDeriving, DeriveDataTypeable #-}
{- |
Module      :  $Header$
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  nevrenato@gmail.com 
Stability   :  experimental
Portability :  portable

Description  :
Signature for an hybridized logic. Its constituted by
the declaration of nominals and modalities, and the signature
of the logic below
-}

module TopHybrid.TopHybridSign where

import TopHybrid.AS_TopHybrid
import Logic.Logic
import Data.Typeable
import Data.Set
import Unsafe.Coerce

-- Prototype; a datatype that wraps the signature and the base logic
-- so that we can know which is the underlying logic, by only looking
-- to the signature
-- We have a total different constructor to represent the empty signagture;
-- we can't compute the an empty signature, without knowing the logic below,
-- hence we use the different constructor for that, which doesn't have the lid. 
data Sgn_Wrap = forall l sub bs f s sm sign mo sy rw pf.
                        (Logic l sub bs f s sm sign mo sy rw pf) =>
                        Sgn_Wrap l (THybridSign sign)
                | EmptySign

data THybridSign s = THybridSign
  { 
    modies :: [MODALITY]
  , nomies :: [NOMINAL]
  , extended :: s
  } deriving (Show, Eq, Ord) 

emptyHybridSign :: Sgn_Wrap
emptyHybridSign = EmptySign 

-- Unfortunately, the typechecker can't know that if (l == l') then
-- the extended signatures are the same. Thus, we will have to use unsafe
-- functions here. Also unfortunately the datatype Lids can't be directly compared
-- so we have to use their string representation, which is bijective, hence we can
-- compare in this way. 
isSubHybridSign :: Sgn_Wrap -> Sgn_Wrap -> Bool
isSubHybridSign (Sgn_Wrap l s) (Sgn_Wrap l' s') = final
         where sAMod = fromList $ modies s
               sBMod = fromList $ modies s' 
               sANom = fromList $ nomies s
               sBNom = fromList $ nomies s' 
               resExt = if (show l == (show l')) 
                           then is_subsig l (extended s) (unsafeCoerce $ extended s')
                           else False
               final = (isSubsetOf sAMod sBMod) &&
                       (isSubsetOf sANom sBNom) && 
                       resExt
-- An empty set is always contained in any other set
isSubHybridSign EmptySign _ = True
-- A non empty set is never contained in an empty set
isSubHybridSign _ EmptySign = False

----- instances needed
deriving instance Show Sgn_Wrap
deriving instance Typeable Sgn_Wrap

instance Eq Sgn_Wrap where
        (==) a b = (isSubHybridSign a b) && (isSubHybridSign b a) 
instance Ord Sgn_Wrap where
        compare a b = if a == b then EQ else GT 
