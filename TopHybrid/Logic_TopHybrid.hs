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
--a
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

data TopHybrid = TopHybrid deriving Show

instance Language TopHybrid where
 description _ = "Hybridization of an arbitrary logic"

instance Category Sgn_Wrap Mor where

instance SignExtension Sgn_Wrap where

instance Syntax TopHybrid Spc_Wrap SYMB_ITEMS SYMB_MAP_ITEMS where
        parse_basic_spec TopHybrid = Just thBasic
        parse_symb_items TopHybrid = Just $ symbItems []
        parse_symb_map_items TopHybrid = Just $ symbMapItems []

instance Sentences TopHybrid Frm_Wrap Sgn_Wrap Mor Symbol where
  
instance StaticAnalysis TopHybrid Spc_Wrap Frm_Wrap SYMB_ITEMS SYMB_MAP_ITEMS
          Sgn_Wrap Mor Symbol RawSymbol where 
                basic_analysis TopHybrid = Just thAna 
                empty_signature TopHybrid = emptyHybridSign

instance Logic TopHybrid () Spc_Wrap Frm_Wrap SYMB_ITEMS SYMB_MAP_ITEMS
               Sgn_Wrap Mor Symbol RawSymbol () where
                stability TopHybrid = Experimental
