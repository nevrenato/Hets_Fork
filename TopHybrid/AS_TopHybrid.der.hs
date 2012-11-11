{-# LANGUAGE StandaloneDeriving, ExistentialQuantification, DeriveDataTypeable #-}
{- |
Module      :  $Header$
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  nevrenato@gmail.com
Stability   :  experimental
Portability :  portable

Description :
Abstract syntax for hybrid logic with an arbitrary logic
below. 
-}

module TopHybrid.AS_TopHybrid where

import Common.Id
import Data.Typeable
import ATerm.Lib
-- DrIFT command
{-! global: GetRange !-}

data TH_BSPEC s = Bspec [TH_BASIC_ITEM] s deriving Show

data TH_BASIC_ITEM = Simple_mod_decl [MODALITY] Range
                   | Simple_nom_decl [NOMINAL] Range 
                     deriving Show

type MODALITY = SIMPLE_ID
type NOMINAL = SIMPLE_ID

data TH_FORMULA f = At NOMINAL (TH_FORMULA f) Range 
                  | Box MODALITY (TH_FORMULA f) Range
                  | Dia MODALITY (TH_FORMULA f) Range
                  | UnderLogic f
                  | Conjunction (TH_FORMULA f) (TH_FORMULA f)
                  | Disjunction (TH_FORMULA f) (TH_FORMULA f)
                  | Implication (TH_FORMULA f) (TH_FORMULA f)
                  | BiImplication (TH_FORMULA f) (TH_FORMULA f) -- ??
                  | Here NOMINAL f Range  
                    deriving (Show, Eq, Ord)

data Form_Wrapper = forall f. (Show f, GetRange f, ShATermConvertible f) 
                                => Form_Wrapper (TH_FORMULA f)

data Spec_Wrapper = forall s. (Show s, GetRange s, ShATermConvertible s) 
                                => Spec_Wrapper (TH_BSPEC s) [Form_Wrapper]

data Mor = Mor 
deriving instance Ord Mor
deriving instance Eq Mor
deriving instance Show Mor

----- Boring instances needed for a valid program, that DriFT cannot generate
deriving instance Show Form_Wrapper
deriving instance Show Spec_Wrapper
deriving instance Typeable Form_Wrapper
deriving instance Typeable Spec_Wrapper

instance Ord Form_Wrapper where
       compare _ _ = EQ 
instance Eq Form_Wrapper where
       (==) _ _ = False 

instance Ord Spec_Wrapper where
        compare _ _ = EQ 
instance Eq Spec_Wrapper where
       (==) _ _= False 

instance GetRange Form_Wrapper where
        getRange (Form_Wrapper f) = getRange f
        rangeSpan (Form_Wrapper f) = rangeSpan f

instance GetRange Spec_Wrapper where
        getRange (Spec_Wrapper s _) = getRange s
        rangeSpan (Spec_Wrapper s _) = rangeSpan s
