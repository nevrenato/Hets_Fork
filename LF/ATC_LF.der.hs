{-# OPTIONS -w -O0 #-}
{- |
Module      :  LF/ATC_LF.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'LF.Sign.Symbol'
'LF.Sign.EXP'
'LF.Sign.DEF'
'LF.Sign.Sign'
'LF.Morphism.MorphType'
'LF.Morphism.Morphism'
'LF.AS.BASIC_SPEC'
'LF.AS.BASIC_ITEM'
'LF.AS.SYMB_ITEMS'
'LF.AS.SYMB_MAP_ITEMS'
'LF.AS.SYMB_OR_MAP'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
LF/Sign.hs
LF/Morphism.hs
LF/AS.hs
-}

module LF.ATC_LF () where

import ATC.AS_Annotation
import ATerm.Lib
import Common.AS_Annotation
import Common.Doc
import Common.Doc hiding (space)
import Common.DocUtils
import Common.Id
import Common.Keywords
import Common.Result
import Data.List
import Data.Maybe
import Data.Typeable
import LF.AS
import LF.Morphism
import LF.Sign
import qualified Data.Map as Map
import qualified Data.Set as Set

{-! for LF.Sign.Symbol derive : Typeable !-}
{-! for LF.Sign.EXP derive : Typeable !-}
{-! for LF.Sign.DEF derive : Typeable !-}
{-! for LF.Sign.Sign derive : Typeable !-}
{-! for LF.Morphism.MorphType derive : Typeable !-}
{-! for LF.Morphism.Morphism derive : Typeable !-}
{-! for LF.AS.BASIC_SPEC derive : Typeable !-}
{-! for LF.AS.BASIC_ITEM derive : Typeable !-}
{-! for LF.AS.SYMB_ITEMS derive : Typeable !-}
{-! for LF.AS.SYMB_MAP_ITEMS derive : Typeable !-}
{-! for LF.AS.SYMB_OR_MAP derive : Typeable !-}

{-! for LF.Sign.Symbol derive : ShATermConvertible !-}
{-! for LF.Sign.EXP derive : ShATermConvertible !-}
{-! for LF.Sign.DEF derive : ShATermConvertible !-}
{-! for LF.Sign.Sign derive : ShATermConvertible !-}
{-! for LF.Morphism.MorphType derive : ShATermConvertible !-}
{-! for LF.Morphism.Morphism derive : ShATermConvertible !-}
{-! for LF.AS.BASIC_SPEC derive : ShATermConvertible !-}
{-! for LF.AS.BASIC_ITEM derive : ShATermConvertible !-}
{-! for LF.AS.SYMB_ITEMS derive : ShATermConvertible !-}
{-! for LF.AS.SYMB_MAP_ITEMS derive : ShATermConvertible !-}
{-! for LF.AS.SYMB_OR_MAP derive : ShATermConvertible !-}
