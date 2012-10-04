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


data TH_BSPEC s = Bspec TH_BASIC_ITEM 
                | ExtSpec s
                  deriving Show 

data TH_BASIC_ITEM = Simple_mod_decl [MODALITY] Range
                   | Simple_nom_decl [NOMINAL] Range 
                     deriving Show

type MODALITY = SIMPLE_ID
type NOMINAL = SIMPLE_ID

data TH_FORMULA f = At NOMINAL (TH_FORMULA f) Range 
                  | Box MODALITY (TH_FORMULA f) Range
                  | Dia MODALITY (TH_FORMULA f) Range
                  | UnderLogic f
                  | Here NOMINAL Range  
                    deriving (Show, Eq, Ord)

data Form_Wrapper = forall f. (Show f, GetRange f, ShATermConvertible f) 
                                => Form_Wrapper (TH_FORMULA f)

-- this spec will need to have a list of TH_BSPEC s
data Spec_Wrapper = forall s. (Show s, GetRange s, ShATermConvertible s) 
                                => Spec_Wrapper (TH_BSPEC s)

----- Boring instances needed for a valid program, that DriFT cannot generate
deriving instance Show Form_Wrapper
deriving instance Show Spec_Wrapper
deriving instance Typeable Form_Wrapper
deriving instance Typeable Spec_Wrapper

instance Eq Form_Wrapper where
       (==) = undefined 
instance Eq Spec_Wrapper where
       (==) = undefined
instance Ord Form_Wrapper where
       compare = undefined
instance Ord Spec_Wrapper where
        compare = undefined 

instance GetRange Form_Wrapper where
        getRange (Form_Wrapper f) = getRange f
        rangeSpan (Form_Wrapper f) = rangeSpan f

instance GetRange Spec_Wrapper where
        getRange (Spec_Wrapper s) = getRange s
        rangeSpan (Spec_Wrapper s) = rangeSpan s


