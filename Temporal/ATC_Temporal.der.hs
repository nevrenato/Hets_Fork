{-# OPTIONS -w -O0 #-}
{- |
Module      :  Temporal/ATC_Temporal.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'Temporal.AS_BASIC_Temporal.BASIC_SPEC'
'Temporal.AS_BASIC_Temporal.FORMULA'
'Temporal.AS_BASIC_Temporal.SYMB_ITEMS'
'Temporal.AS_BASIC_Temporal.SYMB'
'Temporal.AS_BASIC_Temporal.SYMB_MAP_ITEMS'
'Temporal.Sign.Sign'
'Temporal.Symbol.Symbol'
'Temporal.Morphism.Morphism'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Temporal/AS_BASIC_Temporal.hs
Temporal/Sign.hs
Temporal/Symbol.hs
Temporal/Morphism.hs
-}

module Temporal.ATC_Temporal () where

import ATerm.Lib
import CASL.ATC_CASL
import Common.Doc
import Common.DocUtils
import Common.Id
import Common.Id as Id
import Common.Result
import Data.Typeable
import Temporal.AS_BASIC_Temporal
import Temporal.Morphism
import Temporal.Sign
import Temporal.Sign as Sign
import Temporal.Symbol
import qualified Common.Id as Id
import qualified Common.Result as Result
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Temporal.AS_BASIC_Temporal as AS_BASIC
import qualified Temporal.Morphism as Morphism
import qualified Temporal.Sign as Sign

{-! for Temporal.AS_BASIC_Temporal.BASIC_SPEC derive : Typeable !-}
{-! for Temporal.AS_BASIC_Temporal.FORMULA derive : Typeable !-}
{-! for Temporal.AS_BASIC_Temporal.SYMB_ITEMS derive : Typeable !-}
{-! for Temporal.AS_BASIC_Temporal.SYMB derive : Typeable !-}
{-! for Temporal.AS_BASIC_Temporal.SYMB_MAP_ITEMS derive : Typeable !-}
{-! for Temporal.Sign.Sign derive : Typeable !-}
{-! for Temporal.Symbol.Symbol derive : Typeable !-}
{-! for Temporal.Morphism.Morphism derive : Typeable !-}

{-! for Temporal.AS_BASIC_Temporal.BASIC_SPEC derive : ShATermConvertible !-}
{-! for Temporal.AS_BASIC_Temporal.FORMULA derive : ShATermConvertible !-}
{-! for Temporal.AS_BASIC_Temporal.SYMB_ITEMS derive : ShATermConvertible !-}
{-! for Temporal.AS_BASIC_Temporal.SYMB derive : ShATermConvertible !-}
{-! for Temporal.AS_BASIC_Temporal.SYMB_MAP_ITEMS derive : ShATermConvertible !-}
{-! for Temporal.Sign.Sign derive : ShATermConvertible !-}
{-! for Temporal.Symbol.Symbol derive : ShATermConvertible !-}
{-! for Temporal.Morphism.Morphism derive : ShATermConvertible !-}