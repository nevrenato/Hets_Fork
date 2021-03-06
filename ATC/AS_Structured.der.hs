{-# OPTIONS -w -O0 #-}
{- |
Module      :  ATC/AS_Structured.der.hs
Description :  generated Typeable, ShATermLG instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermLG
  for the type(s):
'Syntax.AS_Structured.SPEC'
'Syntax.AS_Structured.RENAMING'
'Syntax.AS_Structured.RESTRICTION'
'Syntax.AS_Structured.G_mapping'
'Syntax.AS_Structured.G_hiding'
'Syntax.AS_Structured.FIT_ARG'
'Syntax.AS_Structured.Logic_code'
'Syntax.AS_Structured.Logic_name'
'Syntax.AS_Structured.CORRESPONDENCE'
'Syntax.AS_Structured.TERM_OR_ENTITY_REF'
'Syntax.AS_Structured.RELATION_REF'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Syntax/AS_Structured.der.hs
-}

module ATC.AS_Structured () where

import ATC.Grothendieck
import ATerm.Lib
import Common.AS_Annotation
import Common.IRI
import Common.Id
import Data.Typeable
import Logic.Grothendieck (G_basic_spec
    , G_symb_items_list
    , G_symb_map_items_list
    , LogicGraph
    , setCurLogic )
import Logic.Logic (AnyLogic)
import Syntax.AS_Structured

{-! for Syntax.AS_Structured.SPEC derive : Typeable !-}
{-! for Syntax.AS_Structured.RENAMING derive : Typeable !-}
{-! for Syntax.AS_Structured.RESTRICTION derive : Typeable !-}
{-! for Syntax.AS_Structured.G_mapping derive : Typeable !-}
{-! for Syntax.AS_Structured.G_hiding derive : Typeable !-}
{-! for Syntax.AS_Structured.FIT_ARG derive : Typeable !-}
{-! for Syntax.AS_Structured.Logic_code derive : Typeable !-}
{-! for Syntax.AS_Structured.Logic_name derive : Typeable !-}
{-! for Syntax.AS_Structured.CORRESPONDENCE derive : Typeable !-}
{-! for Syntax.AS_Structured.TERM_OR_ENTITY_REF derive : Typeable !-}
{-! for Syntax.AS_Structured.RELATION_REF derive : Typeable !-}

{-! for Syntax.AS_Structured.SPEC derive : ShATermLG !-}
{-! for Syntax.AS_Structured.RENAMING derive : ShATermLG !-}
{-! for Syntax.AS_Structured.RESTRICTION derive : ShATermLG !-}
{-! for Syntax.AS_Structured.G_mapping derive : ShATermLG !-}
{-! for Syntax.AS_Structured.G_hiding derive : ShATermLG !-}
{-! for Syntax.AS_Structured.FIT_ARG derive : ShATermLG !-}
{-! for Syntax.AS_Structured.Logic_code derive : ShATermLG !-}
{-! for Syntax.AS_Structured.Logic_name derive : ShATermLG !-}
{-! for Syntax.AS_Structured.CORRESPONDENCE derive : ShATermLG !-}
{-! for Syntax.AS_Structured.TERM_OR_ENTITY_REF derive : ShATermLG !-}
{-! for Syntax.AS_Structured.RELATION_REF derive : ShATermLG !-}
