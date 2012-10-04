{-# OPTIONS -w -O0 #-}
{- |
Module      :  ATC/LibName.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'Common.LibName.LibName'
'Common.LibName.LibId'
'Common.LibName.VersionNumber'
'Common.LibName.LinkPath'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Common/LibName.hs
-}

module ATC.LibName () where

import ATC.Id
import ATerm.Lib
import Common.ATerm.ConvInstances
import Common.Doc
import Common.DocUtils
import Common.Id
import Common.Keywords
import Common.LibName
import Common.Utils
import Data.Char
import Data.Graph.Inductive.Graph
import Data.List
import Data.Ord
import Data.Typeable
import System.FilePath
import System.Time

{-! for Common.LibName.LibName derive : Typeable !-}
{-! for Common.LibName.LibId derive : Typeable !-}
{-! for Common.LibName.VersionNumber derive : Typeable !-}
{-! for Common.LibName.LinkPath derive : Typeable !-}

{-! for Common.LibName.LibName derive : ShATermConvertible !-}
{-! for Common.LibName.LibId derive : ShATermConvertible !-}
{-! for Common.LibName.VersionNumber derive : ShATermConvertible !-}
{-! for Common.LibName.LinkPath derive : ShATermConvertible !-}
