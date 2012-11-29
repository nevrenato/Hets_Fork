{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, ExistentialQuantification, DeriveDataTypeable, StandaloneDeriving #-}
{- |
Module      :  $Header$
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  : nevrenato@gmail.com
Stability   : experimental
Portability : portable

Description :
Instance of class Logic for hybridized logics 
with an arbitrary logic under.
-}
module TopHybrid.Logic_TopHybrid where

import Logic.Logic
import TopHybrid.AS_TopHybrid
import TopHybrid.TopHybridSign
import TopHybrid.Parse_AS
import TopHybrid.Print_AS ()
import TopHybrid.StatAna
import CASL.Morphism
import CASL.AS_Basic_CASL
import CASL.Logic_CASL ()
import CASL.SymbolParser
import CASL.Sign

data Hybridize = Hybridize deriving Show

instance Language Hybridize where
 description _ = "Hybridization of an arbitrary logic"

instance Category Sgn_Wrap Mor where

instance SignExtension Sgn_Wrap where

instance Syntax Hybridize Spc_Wrap SYMB_ITEMS SYMB_MAP_ITEMS where
        parse_basic_spec Hybridize = Just thBasic
        parse_symb_items Hybridize = Just $ symbItems []
        parse_symb_map_items Hybridize = Just $ symbMapItems []

instance Sentences Hybridize Frm_Wrap Sgn_Wrap Mor Symbol where
  
instance StaticAnalysis Hybridize Spc_Wrap Frm_Wrap SYMB_ITEMS SYMB_MAP_ITEMS
          Sgn_Wrap Mor Symbol RawSymbol where 
                basic_analysis Hybridize = Just thAna 
                empty_signature Hybridize = emptyHybridSign

instance Logic Hybridize () Spc_Wrap Frm_Wrap SYMB_ITEMS SYMB_MAP_ITEMS
               Sgn_Wrap Mor Symbol RawSymbol () where
                stability Hybridize = Experimental
