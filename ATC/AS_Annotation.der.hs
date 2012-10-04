{-# OPTIONS -w -O0 #-}
{- |
Module      :  ATC/AS_Annotation.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'Common.AS_Annotation.Annote_word'
'Common.AS_Annotation.Annote_text'
'Common.AS_Annotation.Display_format'
'Common.AS_Annotation.PrecRel'
'Common.AS_Annotation.AssocEither'
'Common.AS_Annotation.Semantic_anno'
'Common.AS_Annotation.Annotation'
'Common.AS_Annotation.Annoted'
'Common.AS_Annotation.SenAttr'
'Common.AS_Annotation.Name'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Common/AS_Annotation.der.hs
-}

module ATC.AS_Annotation () where

import ATC.IRI
import ATerm.Lib
import Common.AS_Annotation
import Common.ATerm.ConvInstances
import Common.IRI (IRI)
import Common.Id
import Data.Maybe
import Data.Typeable

{-! for Common.AS_Annotation.Annote_word derive : Typeable !-}
{-! for Common.AS_Annotation.Annote_text derive : Typeable !-}
{-! for Common.AS_Annotation.Display_format derive : Typeable !-}
{-! for Common.AS_Annotation.PrecRel derive : Typeable !-}
{-! for Common.AS_Annotation.AssocEither derive : Typeable !-}
{-! for Common.AS_Annotation.Semantic_anno derive : Typeable !-}
{-! for Common.AS_Annotation.Annotation derive : Typeable !-}
{-! for Common.AS_Annotation.Annoted derive : Typeable !-}
{-! for Common.AS_Annotation.SenAttr derive : Typeable !-}
{-! for Common.AS_Annotation.Name derive : Typeable !-}

{-! for Common.AS_Annotation.Annote_word derive : ShATermConvertible !-}
{-! for Common.AS_Annotation.Annote_text derive : ShATermConvertible !-}
{-! for Common.AS_Annotation.Display_format derive : ShATermConvertible !-}
{-! for Common.AS_Annotation.PrecRel derive : ShATermConvertible !-}
{-! for Common.AS_Annotation.AssocEither derive : ShATermConvertible !-}
{-! for Common.AS_Annotation.Semantic_anno derive : ShATermConvertible !-}
{-! for Common.AS_Annotation.Annotation derive : ShATermConvertible !-}
{-! for Common.AS_Annotation.Annoted derive : ShATermConvertible !-}
{-! for Common.AS_Annotation.SenAttr derive : ShATermConvertible !-}
{-! for Common.AS_Annotation.Name derive : ShATermConvertible !-}
