{-# OPTIONS -w -O0 #-}
{- |
Module      :  CoCASL/ATC_CoCASL.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'CoCASL.AS_CoCASL.C_BASIC_ITEM'
'CoCASL.AS_CoCASL.C_SIG_ITEM'
'CoCASL.AS_CoCASL.CODATATYPE_DECL'
'CoCASL.AS_CoCASL.COALTERNATIVE'
'CoCASL.AS_CoCASL.COCOMPONENTS'
'CoCASL.AS_CoCASL.MODALITY'
'CoCASL.AS_CoCASL.C_FORMULA'
'CoCASL.CoCASLSign.CoCASLSign'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
CoCASL/AS_CoCASL.hs
CoCASL/CoCASLSign.hs
-}

module CoCASL.ATC_CoCASL () where

import ATerm.Lib
import CASL.AS_Basic_CASL
import CASL.AS_Basic_CASL (SORT)
import CASL.ATC_CASL
import CASL.Sign
import CoCASL.AS_CoCASL
import CoCASL.CoCASLSign
import Common.AS_Annotation
import Common.Id
import Data.Typeable
import qualified Common.Lib.MapSet as MapSet
import qualified Common.Lib.Rel as Rel

{-! for CoCASL.AS_CoCASL.C_BASIC_ITEM derive : Typeable !-}
{-! for CoCASL.AS_CoCASL.C_SIG_ITEM derive : Typeable !-}
{-! for CoCASL.AS_CoCASL.CODATATYPE_DECL derive : Typeable !-}
{-! for CoCASL.AS_CoCASL.COALTERNATIVE derive : Typeable !-}
{-! for CoCASL.AS_CoCASL.COCOMPONENTS derive : Typeable !-}
{-! for CoCASL.AS_CoCASL.MODALITY derive : Typeable !-}
{-! for CoCASL.AS_CoCASL.C_FORMULA derive : Typeable !-}
{-! for CoCASL.CoCASLSign.CoCASLSign derive : Typeable !-}

{-! for CoCASL.AS_CoCASL.C_BASIC_ITEM derive : ShATermConvertible !-}
{-! for CoCASL.AS_CoCASL.C_SIG_ITEM derive : ShATermConvertible !-}
{-! for CoCASL.AS_CoCASL.CODATATYPE_DECL derive : ShATermConvertible !-}
{-! for CoCASL.AS_CoCASL.COALTERNATIVE derive : ShATermConvertible !-}
{-! for CoCASL.AS_CoCASL.COCOMPONENTS derive : ShATermConvertible !-}
{-! for CoCASL.AS_CoCASL.MODALITY derive : ShATermConvertible !-}
{-! for CoCASL.AS_CoCASL.C_FORMULA derive : ShATermConvertible !-}
{-! for CoCASL.CoCASLSign.CoCASLSign derive : ShATermConvertible !-}
