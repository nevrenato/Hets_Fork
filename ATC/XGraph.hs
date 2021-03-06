{-# OPTIONS -w -O0 #-}
{- |
Module      :  ATC/XGraph.der.hs
Description :  generated Typeable, ShATermLG instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermLG
  for the type(s):
'Static.XGraph.XGraph'
'Static.XGraph.XNode'
'Static.XGraph.XLink'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Static/XGraph.hs
-}

module ATC.XGraph () where

import ATC.DgUtils
import ATC.Grothendieck
import ATerm.Lib
import Common.AnalyseAnnos (getGlobalAnnos)
import Common.Consistency (Conservativity (..))
import Common.GlobalAnnotations (GlobalAnnos, emptyGlobalAnnos)
import Common.LibName
import Common.Result (Result (..))
import Common.Utils (readMaybe)
import Common.XUpdate (getAttrVal, readAttrVal)
import Control.Monad
import Data.List
import Data.Maybe (fromMaybe)
import Data.Typeable
import Static.DgUtils
import Static.XGraph
import Text.XML.Light
import qualified Data.Map as Map
import qualified Data.Set as Set

{-! for Static.XGraph.XGraph derive : Typeable !-}
{-! for Static.XGraph.XNode derive : Typeable !-}
{-! for Static.XGraph.XLink derive : Typeable !-}

{-! for Static.XGraph.XGraph derive : ShATermLG !-}
{-! for Static.XGraph.XNode derive : ShATermLG !-}
{-! for Static.XGraph.XLink derive : ShATermLG !-}

-- Generated by DrIFT, look but don't touch!

_tcXGraphTc :: TyCon
_tcXGraphTc = mkTyCon "Static.XGraph.XGraph"
instance Typeable XGraph where
    typeOf _ = mkTyConApp _tcXGraphTc []

_tcXNodeTc :: TyCon
_tcXNodeTc = mkTyCon "Static.XGraph.XNode"
instance Typeable XNode where
    typeOf _ = mkTyConApp _tcXNodeTc []

_tcXLinkTc :: TyCon
_tcXLinkTc = mkTyCon "Static.XGraph.XLink"
instance Typeable XLink where
    typeOf _ = mkTyConApp _tcXLinkTc []

instance ShATermLG XGraph where
  toShATermLG att0 xv = case xv of
    XGraph a b c d e f -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      (att4, d') <- toShATermLG' att3 d
      (att5, e') <- toShATermLG' att4 e
      (att6, f') <- toShATermLG' att5 f
      return $ addATerm (ShAAppl "XGraph" [a', b', c', d', e',
                                           f'] []) att6
  fromShATermLG lg ix att0 = case getShATerm ix att0 of
    ShAAppl "XGraph" [a, b, c, d, e, f] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      case fromShATermLG' lg d att3 of
      { (att4, d') ->
      case fromShATermLG' lg e att4 of
      { (att5, e') ->
      case fromShATermLG' lg f att5 of
      { (att6, f') ->
      (att6, XGraph a' b' c' d' e' f') }}}}}}
    u -> fromShATermError "XGraph" u

instance ShATermLG XNode where
  toShATermLG att0 xv = case xv of
    XNode a b c d e -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      (att4, d') <- toShATermLG' att3 d
      (att5, e') <- toShATermLG' att4 e
      return $ addATerm (ShAAppl "XNode" [a', b', c', d', e'] []) att5
    XRef a b c d -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      (att4, d') <- toShATermLG' att3 d
      return $ addATerm (ShAAppl "XRef" [a', b', c', d'] []) att4
  fromShATermLG lg ix att0 = case getShATerm ix att0 of
    ShAAppl "XNode" [a, b, c, d, e] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      case fromShATermLG' lg d att3 of
      { (att4, d') ->
      case fromShATermLG' lg e att4 of
      { (att5, e') ->
      (att5, XNode a' b' c' d' e') }}}}}
    ShAAppl "XRef" [a, b, c, d] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      case fromShATermLG' lg d att3 of
      { (att4, d') ->
      (att4, XRef a' b' c' d') }}}}
    u -> fromShATermError "XNode" u

instance ShATermLG XLink where
  toShATermLG att0 xv = case xv of
    XLink a b c d e f g h i j -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      (att4, d') <- toShATermLG' att3 d
      (att5, e') <- toShATermLG' att4 e
      (att6, f') <- toShATermLG' att5 f
      (att7, g') <- toShATermLG' att6 g
      (att8, h') <- toShATermLG' att7 h
      (att9, i') <- toShATermLG' att8 i
      (att10, j') <- toShATermLG' att9 j
      return $ addATerm (ShAAppl "XLink" [a', b', c', d', e', f', g', h',
                                          i', j'] []) att10
  fromShATermLG lg ix att0 = case getShATerm ix att0 of
    ShAAppl "XLink" [a, b, c, d, e, f, g, h, i, j] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      case fromShATermLG' lg d att3 of
      { (att4, d') ->
      case fromShATermLG' lg e att4 of
      { (att5, e') ->
      case fromShATermLG' lg f att5 of
      { (att6, f') ->
      case fromShATermLG' lg g att6 of
      { (att7, g') ->
      case fromShATermLG' lg h att7 of
      { (att8, h') ->
      case fromShATermLG' lg i att8 of
      { (att9, i') ->
      case fromShATermLG' lg j att9 of
      { (att10, j') ->
      (att10, XLink a' b' c' d' e' f' g' h' i' j') }}}}}}}}}}
    u -> fromShATermError "XLink" u
