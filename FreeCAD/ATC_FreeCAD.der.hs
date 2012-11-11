{-# OPTIONS -w -O0 #-}
{- |
Module      :  FreeCAD/ATC_FreeCAD.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'FreeCAD.As.Vector3'
'FreeCAD.As.Matrix33'
'FreeCAD.As.Vector4'
'FreeCAD.As.Placement'
'FreeCAD.As.BaseObject'
'FreeCAD.As.Object'
'FreeCAD.As.ExtendedObject'
'FreeCAD.As.PlacedObject'
'FreeCAD.As.NamedObject'
'FreeCAD.As.Sign'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
FreeCAD/As.hs
-}

module FreeCAD.ATC_FreeCAD () where

import ATerm.Lib
import Common.ATerm.ConvInstances
import Data.Typeable
import FreeCAD.As
import qualified Data.Set as Set

{-! for FreeCAD.As.Vector3 derive : Typeable !-}
{-! for FreeCAD.As.Matrix33 derive : Typeable !-}
{-! for FreeCAD.As.Vector4 derive : Typeable !-}
{-! for FreeCAD.As.Placement derive : Typeable !-}
{-! for FreeCAD.As.BaseObject derive : Typeable !-}
{-! for FreeCAD.As.Object derive : Typeable !-}
{-! for FreeCAD.As.ExtendedObject derive : Typeable !-}
{-! for FreeCAD.As.PlacedObject derive : Typeable !-}
{-! for FreeCAD.As.NamedObject derive : Typeable !-}
{-! for FreeCAD.As.Sign derive : Typeable !-}

{-! for FreeCAD.As.Vector3 derive : ShATermConvertible !-}
{-! for FreeCAD.As.Matrix33 derive : ShATermConvertible !-}
{-! for FreeCAD.As.Vector4 derive : ShATermConvertible !-}
{-! for FreeCAD.As.Placement derive : ShATermConvertible !-}
{-! for FreeCAD.As.BaseObject derive : ShATermConvertible !-}
{-! for FreeCAD.As.Object derive : ShATermConvertible !-}
{-! for FreeCAD.As.ExtendedObject derive : ShATermConvertible !-}
{-! for FreeCAD.As.PlacedObject derive : ShATermConvertible !-}
{-! for FreeCAD.As.NamedObject derive : ShATermConvertible !-}
{-! for FreeCAD.As.Sign derive : ShATermConvertible !-}
