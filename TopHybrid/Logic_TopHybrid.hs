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
 
instance StaticAnalysis TopHybrid Spec_Wrapper Form_Wrapper SYMB_ITEMS SYMB_MAP_ITEMS
          Sign_Wrapper Mor Symbol RawSymbol where 
                basic_analysis TopHybrid = Just thAna 
                empty_signature TopHybrid = emptyTHybridSign

instance Logic TopHybrid () Spec_Wrapper Form_Wrapper SYMB_ITEMS SYMB_MAP_ITEMS
               Sign_Wrapper Mor Symbol RawSymbol () where
                stability TopHybrid = Experimental

-- Boring instances needed for a valid program, that DriFT cannot generate
instance  ShATermConvertible Sign_Wrapper where
         toShATermAux att (Sign_Wrapper s) = toShATermAux att s
--         fromShATermAux a b = mapSnd Sign_Wrapper $ fromShATermAux a b
--                 where mapSnd f (a,b) = (a, f b)
         fromShATermAux a b = error "I entered here"

instance ShATermConvertible Spec_Wrapper where
         toShATermAux att (Spec_Wrapper s _) = toShATermAux att s 
--         fromShATermAux a b = mapSnd Spec_Wrapper $ fromShATermAux a b
--                 where mapSnd f (x,y) = (x, f y)
         fromShATermAux a b = error "I entered here"

instance ShATermConvertible Form_Wrapper where
        toShATermAux att (Form_Wrapper f) = toShATermAux att f
        fromShATermAux a b = error "I entered here"
