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
import Common.Result
import Common.Id
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
    modies :: Set MODALITY
  , nomies :: Set NOMINAL
  , extended :: s
  } deriving (Show, Eq, Ord) 

emptyHybridSign :: Sgn_Wrap
emptyHybridSign = EmptySign 

-- Unfortunately, the typechecker can't know that if (l == l') then
-- the extended signatures are the same. Thus, we will have to use unsafe
-- functions here. Also unfortunately the datatype Lids can't be directly compared
-- so we have to use their string representation, which is bijective, hence we can
-- compare in this way. 
isSubTHybSgn :: Sgn_Wrap -> Sgn_Wrap -> Bool
isSubTHybSgn (Sgn_Wrap l s) (Sgn_Wrap l' s') = final
               where 
               resExt =  if (show l == (show l'))         
                         then is_subsig l (extended s) (unsafeCoerce $ extended s')
                         else False
               final = (isSubsetOf (modies s) (modies s')) &&
                       (isSubsetOf (nomies s) (nomies s')) && 
                       resExt
-- An empty set is always contained in any other set
isSubTHybSgn EmptySign _ = True
-- A non empty set is never contained in an empty set
isSubTHybSgn _ EmptySign = False

-- Computes the difference between two signatures. If they belong to
-- different logics then throw an error
sgnDiff :: Sgn_Wrap -> Sgn_Wrap -> Result Sgn_Wrap
-- The difference between an emptySign and any other Sig results in an emptySig
sgnDiff EmptySign _ = return EmptySign
-- The difference between any sig and a empty sig results in the original sig
sgnDiff s EmptySign = return s

sgnDiff (Sgn_Wrap l s) (Sgn_Wrap l' s') = 
                if (show l) /= (show l') 
                then Result [Diag Error "signatures belong to different logics" nullRange] Nothing 
                else ds >>= (return . (Sgn_Wrap l)) 
                where
                        dn = difference (nomies s) $ nomies s'
                        dm = difference (modies s) $ modies s'
                        ds = (signatureDiff l (extended s) $ unsafeCoerce $ extended s') >>= (return . (THybridSign dm dn)) 

----- instances needed
deriving instance Show Sgn_Wrap
deriving instance Typeable Sgn_Wrap

instance Eq Sgn_Wrap where
        (==) a b = (isSubTHybSgn a b) && (isSubTHybSgn b a) 
instance Ord Sgn_Wrap where
        compare a b = if a == b then EQ else GT 
