{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, ExistentialQuantification, DeriveDataTypeable, StandaloneDeriving #-}
{- |
Module      :  $Header$
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  : nevrenato@gmail.com
Stability   : experimental
Portability : portable

Description :
Instance of class Logic for hybrid logic with
an arbitrary logic under.
-}

module TopHybrid.Logic_TopHybrid where

import Logic.Logic
import ATerm.Lib
import TopHybrid.AS_TopHybrid
import TopHybrid.TopHybridSign
import TopHybrid.ATC_TopHybrid
import TopHybrid.Parse_AS
import TopHybrid.Print_AS ()
import TopHybrid.StatAna
import CASL.Morphism
import CASL.AS_Basic_CASL
import CASL.Logic_CASL ()
import CASL.SymbolParser
import CASL.Sign
import Data.Typeable
data TopHybrid = TopHybrid deriving Show

instance Language TopHybrid where
 description _ = "Hybridization of a logic"

instance Category Sign_Wrapper Mor where

instance SignExtension Sign_Wrapper where
        isSubSignExtension = isSubTHybridSign

instance Syntax TopHybrid Spec_Wrapper SYMB_ITEMS SYMB_MAP_ITEMS where
        parse_basic_spec TopHybrid = Just thBasic
        parse_symb_items TopHybrid = Just $ symbItems []
        parse_symb_map_items TopHybrid = Just $ symbMapItems []

instance Sentences TopHybrid Form_Wrapper Sign_Wrapper Mor Symbol where
 --       print_named TopHybrid = undefined
  
instance StaticAnalysis TopHybrid Spec_Wrapper Form_Wrapper SYMB_ITEMS SYMB_MAP_ITEMS
          Sign_Wrapper Mor Symbol RawSymbol where 
                basic_analysis TopHybrid = Just thAna 
                empty_signature TopHybrid = emptyTHybridSign

instance Logic TopHybrid () Spec_Wrapper Form_Wrapper SYMB_ITEMS SYMB_MAP_ITEMS
               Sign_Wrapper Mor Symbol RawSymbol () where
                stability TopHybrid = Experimental


