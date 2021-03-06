{-# OPTIONS -w -O0 #-}
{- |
Module      :  ATC/Graph.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'Common.Lib.Graph.Gr'
'Common.Lib.Graph.GrContext'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Common/Lib/Graph.hs
-}

module ATC.Graph () where

import ATerm.Lib
import Common.Lib.Graph
import Data.Graph.Inductive.Graph as Graph
import Data.List
import Data.Typeable
import qualified Data.IntMap as Map

{-! for Common.Lib.Graph.Gr derive : Typeable !-}
{-! for Common.Lib.Graph.GrContext derive : Typeable !-}

{-! for Common.Lib.Graph.Gr derive : ShATermConvertible !-}
{-! for Common.Lib.Graph.GrContext derive : ShATermConvertible !-}

-- Generated by DrIFT, look but don't touch!

_tcGrTc :: TyCon
_tcGrTc = mkTyCon "Common.Lib.Graph.Gr"
instance Typeable2 Gr where
    typeOf2 _ = mkTyConApp _tcGrTc []

_tcGrContextTc :: TyCon
_tcGrContextTc = mkTyCon "Common.Lib.Graph.GrContext"
instance Typeable2 GrContext where
    typeOf2 _ = mkTyConApp _tcGrContextTc []

instance (ShATermConvertible a,
          ShATermConvertible b) => ShATermConvertible (Gr a b) where
  toShATermAux att0 xv = case xv of
    Gr a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Gr" [a'] []) att1
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Gr" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Gr a') }
    u -> fromShATermError "Gr" u

instance (ShATermConvertible a,
          ShATermConvertible b) => ShATermConvertible (GrContext a b) where
  toShATermAux att0 xv = case xv of
    GrContext a b c d -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      return $ addATerm (ShAAppl "GrContext" [a', b', c', d'] []) att4
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "GrContext" [a, b, c, d] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      case fromShATerm' d att3 of
      { (att4, d') ->
      (att4, GrContext a' b' c' d') }}}}
    u -> fromShATermError "GrContext" u
