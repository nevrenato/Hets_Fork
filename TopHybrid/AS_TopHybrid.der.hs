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
import Logic.Logic
import Common.DocUtils
-- DrIFT command
{-! global: GetRange !-}

data TH_BSPEC s = Bspec { bitems :: [TH_BASIC_ITEM],  und :: s } deriving Show

data TH_BASIC_ITEM = Simple_mod_decl [MODALITY] 
                   | Simple_nom_decl [NOMINAL] 
                     deriving Show

type MODALITY = SIMPLE_ID
type NOMINAL = SIMPLE_ID

data TH_FORMULA f = At NOMINAL (TH_FORMULA f) 
                  | Box MODALITY (TH_FORMULA f) 
                  | Dia MODALITY (TH_FORMULA f) 
                  | UnderLogic f
                  | Conjunction (TH_FORMULA f) (TH_FORMULA f)
                  | Disjunction (TH_FORMULA f) (TH_FORMULA f)
                  | Implication (TH_FORMULA f) (TH_FORMULA f)
                  | BiImplication (TH_FORMULA f) (TH_FORMULA f) 
                  | Here NOMINAL
                  | Neg (TH_FORMULA f) 
                    deriving (Show, Eq, Ord)

data Form_Wrapper = forall f. (Pretty f, GetRange f, ShATermConvertible f) 
                                => Form_Wrapper (TH_FORMULA f)


data Spec_Wrapper = forall s. (Pretty s, GetRange s, ShATermConvertible s) 
                                => Spec_Wrapper AnyLogic (TH_BSPEC s) [Form_Wrapper]

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
        compare _ _  = EQ 
instance Eq Spec_Wrapper where
       (==) _ _  = False 

instance GetRange Form_Wrapper where
        getRange (Form_Wrapper f) = getRange f
        rangeSpan (Form_Wrapper f) = rangeSpan f

instance GetRange Spec_Wrapper where
        getRange (Spec_Wrapper _ s _) = getRange s
        rangeSpan (Spec_Wrapper _ s _) = rangeSpan s
