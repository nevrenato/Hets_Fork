{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, ExistentialQuantification #-}
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

import CASL.Sign
import CASL.Morphism
import CASL.AS_Basic_CASL
import CASL.Logic_CASL ()
import CASL.SymbolParser

data TopHybrid = TopHybrid deriving Show

instance Language TopHybrid where
 description _ = "Hybrid Logic\n" ++ 
                 "Extends an abitrary logic with at/modal operators."

type THSign = Sign Form_Wrapper Sign_Wrapper 
type THybridMor = Morphism Form_Wrapper Sign_Wrapper (DefMorExt Sign_Wrapper)


instance SignExtension Sign_Wrapper where
        isSubSignExtension = isSubTHybridSign

instance Syntax TopHybrid Spec_Wrapper SYMB_ITEMS SYMB_MAP_ITEMS where
        parse_basic_spec TopHybrid = Just thBasic
        parse_symb_items TopHybrid = Just $ symbItems []
        parse_symb_map_items TopHybrid = Just $ symbMapItems []

instance Sentences TopHybrid Form_Wrapper THSign THybridMor Symbol where
 

instance StaticAnalysis TopHybrid Spec_Wrapper Form_Wrapper SYMB_ITEMS SYMB_MAP_ITEMS
          THSign THybridMor Symbol RawSymbol where 

instance Logic TopHybrid () Spec_Wrapper Form_Wrapper SYMB_ITEMS SYMB_MAP_ITEMS
               THSign THybridMor Symbol RawSymbol () where
                stability TopHybrid = Experimental

----- Boring instances needed for a valid program, that DriFT cannot generate
instance  ShATermConvertible Spec_Wrapper where
         toShATermAux att (Spec_Wrapper s) = toShATermAux att s
--         fromShATermAux a b = mapSnd Spec_Wrapper $ fromShATermAux a b
--                 where mapSnd f (x,y) = (x, f y)

instance  ShATermConvertible Form_Wrapper where
         toShATermAux att (Form_Wrapper f) = toShATermAux att f
--         fromShATermAux a b = mapSnd Form_Wrapper $ fromShATermAux a b
--                 where mapSnd f (a,b) = (a, f b)

instance  ShATermConvertible Sign_Wrapper where
         toShATermAux att (Sign_Wrapper s) = toShATermAux att s
--         fromShATermAux a b = mapSnd Sign_Wrapper $ fromShATermAux a b
--                 where mapSnd f (a,b) = (a, f b)

-----------
