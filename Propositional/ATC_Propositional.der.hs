{-# OPTIONS -w -O0 #-}
{- |
Module      :  Propositional/ATC_Propositional.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'Propositional.Sign.Sign'
'Propositional.Morphism.Morphism'
'Propositional.AS_BASIC_Propositional.PRED_ITEM'
'Propositional.AS_BASIC_Propositional.BASIC_SPEC'
'Propositional.AS_BASIC_Propositional.BASIC_ITEMS'
'Propositional.AS_BASIC_Propositional.FORMULA'
'Propositional.AS_BASIC_Propositional.SYMB_ITEMS'
'Propositional.AS_BASIC_Propositional.SYMB'
'Propositional.AS_BASIC_Propositional.SYMB_MAP_ITEMS'
'Propositional.AS_BASIC_Propositional.SYMB_OR_MAP'
'Propositional.Symbol.Symbol'
'Propositional.Sublogic.PropFormulae'
'Propositional.Sublogic.PropSL'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Propositional/Sign.hs
Propositional/Morphism.hs
Propositional/AS_BASIC_Propositional.hs
Propositional/Symbol.hs
Propositional/Sublogic.hs
-}

module Propositional.ATC_Propositional () where

import ATC.AS_Annotation
import ATerm.Lib
import Common.AS_Annotation as AS_Anno
import Common.Doc
import Common.DocUtils
import Common.Id
import Common.Id as Id
import Common.Keywords
import Common.Result
import Data.Typeable
import Propositional.AS_BASIC_Propositional
import Propositional.Morphism
import Propositional.Morphism as Morphism
import Propositional.Sign
import Propositional.Sign as Sign
import Propositional.Sublogic
import Propositional.Symbol
import qualified Common.AS_Annotation as AS_Anno
import qualified Common.Id as Id
import qualified Common.Lib.State as State
import qualified Common.Result as Result
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Propositional.AS_BASIC_Propositional as AS_BASIC
import qualified Propositional.Morphism as Morphism
import qualified Propositional.Sign as Sign
import qualified Propositional.Symbol as Symbol
import qualified Propositional.Tools as Tools

{-! for Propositional.Sign.Sign derive : Typeable !-}
{-! for Propositional.Morphism.Morphism derive : Typeable !-}
{-! for Propositional.AS_BASIC_Propositional.PRED_ITEM derive : Typeable !-}
{-! for Propositional.AS_BASIC_Propositional.BASIC_SPEC derive : Typeable !-}
{-! for Propositional.AS_BASIC_Propositional.BASIC_ITEMS derive : Typeable !-}
{-! for Propositional.AS_BASIC_Propositional.FORMULA derive : Typeable !-}
{-! for Propositional.AS_BASIC_Propositional.SYMB_ITEMS derive : Typeable !-}
{-! for Propositional.AS_BASIC_Propositional.SYMB derive : Typeable !-}
{-! for Propositional.AS_BASIC_Propositional.SYMB_MAP_ITEMS derive : Typeable !-}
{-! for Propositional.AS_BASIC_Propositional.SYMB_OR_MAP derive : Typeable !-}
{-! for Propositional.Symbol.Symbol derive : Typeable !-}
{-! for Propositional.Sublogic.PropFormulae derive : Typeable !-}
{-! for Propositional.Sublogic.PropSL derive : Typeable !-}

{-! for Propositional.Sign.Sign derive : ShATermConvertible !-}
{-! for Propositional.Morphism.Morphism derive : ShATermConvertible !-}
{-! for Propositional.AS_BASIC_Propositional.PRED_ITEM derive : ShATermConvertible !-}
{-! for Propositional.AS_BASIC_Propositional.BASIC_SPEC derive : ShATermConvertible !-}
{-! for Propositional.AS_BASIC_Propositional.BASIC_ITEMS derive : ShATermConvertible !-}
{-! for Propositional.AS_BASIC_Propositional.FORMULA derive : ShATermConvertible !-}
{-! for Propositional.AS_BASIC_Propositional.SYMB_ITEMS derive : ShATermConvertible !-}
{-! for Propositional.AS_BASIC_Propositional.SYMB derive : ShATermConvertible !-}
{-! for Propositional.AS_BASIC_Propositional.SYMB_MAP_ITEMS derive : ShATermConvertible !-}
{-! for Propositional.AS_BASIC_Propositional.SYMB_OR_MAP derive : ShATermConvertible !-}
{-! for Propositional.Symbol.Symbol derive : ShATermConvertible !-}
{-! for Propositional.Sublogic.PropFormulae derive : ShATermConvertible !-}
{-! for Propositional.Sublogic.PropSL derive : ShATermConvertible !-}