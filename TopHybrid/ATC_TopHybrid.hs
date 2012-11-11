{-# OPTIONS -w -O0 #-}
{- |
Module      :  TopHybrid/ATC_TopHybrid.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'TopHybrid.AS_TopHybrid.TH_BSPEC'
'TopHybrid.AS_TopHybrid.TH_BASIC_ITEM'
'TopHybrid.AS_TopHybrid.TH_FORMULA'
'TopHybrid.AS_TopHybrid.Mor'
'TopHybrid.TopHybridSign.THybridSign'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
TopHybrid/AS_TopHybrid.hs
TopHybrid/TopHybridSign.hs
-}

module TopHybrid.ATC_TopHybrid () where

import ATC.AS_Annotation
import ATerm.Lib
import Common.Id
import Data.Typeable
import TopHybrid.AS_TopHybrid
import TopHybrid.TopHybridSign

{-! for TopHybrid.AS_TopHybrid.TH_BSPEC derive : Typeable !-}
{-! for TopHybrid.AS_TopHybrid.TH_BASIC_ITEM derive : Typeable !-}
{-! for TopHybrid.AS_TopHybrid.TH_FORMULA derive : Typeable !-}
{-! for TopHybrid.AS_TopHybrid.Mor derive : Typeable !-}
{-! for TopHybrid.TopHybridSign.THybridSign derive : Typeable !-}

{-! for TopHybrid.AS_TopHybrid.TH_BSPEC derive : ShATermConvertible !-}
{-! for TopHybrid.AS_TopHybrid.TH_BASIC_ITEM derive : ShATermConvertible !-}
{-! for TopHybrid.AS_TopHybrid.TH_FORMULA derive : ShATermConvertible !-}
{-! for TopHybrid.AS_TopHybrid.Mor derive : ShATermConvertible !-}
{-! for TopHybrid.TopHybridSign.THybridSign derive : ShATermConvertible !-}

-- Generated by DrIFT, look but don't touch!

_tcTH_BSPECTc :: TyCon
_tcTH_BSPECTc = mkTyCon "TopHybrid.AS_TopHybrid.TH_BSPEC"
instance Typeable1 TH_BSPEC where
    typeOf1 _ = mkTyConApp _tcTH_BSPECTc []

_tcTH_BASIC_ITEMTc :: TyCon
_tcTH_BASIC_ITEMTc = mkTyCon "TopHybrid.AS_TopHybrid.TH_BASIC_ITEM"
instance Typeable TH_BASIC_ITEM where
    typeOf _ = mkTyConApp _tcTH_BASIC_ITEMTc []

_tcTH_FORMULATc :: TyCon
_tcTH_FORMULATc = mkTyCon "TopHybrid.AS_TopHybrid.TH_FORMULA"
instance Typeable1 TH_FORMULA where
    typeOf1 _ = mkTyConApp _tcTH_FORMULATc []

_tcMorTc :: TyCon
_tcMorTc = mkTyCon "TopHybrid.AS_TopHybrid.Mor"
instance Typeable Mor where
    typeOf _ = mkTyConApp _tcMorTc []

instance ShATermConvertible s => ShATermConvertible (TH_BSPEC s) where
  toShATermAux att0 xv = case xv of
    Bspec a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Bspec" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Bspec" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Bspec a' b') }}
    u -> fromShATermError "TH_BSPEC" u

instance ShATermConvertible TH_BASIC_ITEM where
  toShATermAux att0 xv = case xv of
    Simple_mod_decl a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Simple_mod_decl" [a', b'] []) att2
    Simple_nom_decl a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Simple_nom_decl" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Simple_mod_decl" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Simple_mod_decl a' b') }}
    ShAAppl "Simple_nom_decl" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Simple_nom_decl a' b') }}
    u -> fromShATermError "TH_BASIC_ITEM" u

instance ShATermConvertible f => ShATermConvertible (TH_FORMULA f) where
  toShATermAux att0 xv = case xv of
    At a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "At" [a', b', c'] []) att3
    Box a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Box" [a', b', c'] []) att3
    Dia a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Dia" [a', b', c'] []) att3
    UnderLogic a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "UnderLogic" [a'] []) att1
    Conjunction a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Conjunction" [a', b'] []) att2
    Disjunction a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Disjunction" [a', b'] []) att2
    Implication a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Implication" [a', b'] []) att2
    BiImplication a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "BiImplication" [a', b'] []) att2
    Here a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Here" [a', b', c'] []) att3
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "At" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, At a' b' c') }}}
    ShAAppl "Box" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Box a' b' c') }}}
    ShAAppl "Dia" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Dia a' b' c') }}}
    ShAAppl "UnderLogic" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, UnderLogic a') }
    ShAAppl "Conjunction" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Conjunction a' b') }}
    ShAAppl "Disjunction" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Disjunction a' b') }}
    ShAAppl "Implication" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Implication a' b') }}
    ShAAppl "BiImplication" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, BiImplication a' b') }}
    ShAAppl "Here" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Here a' b' c') }}}
    u -> fromShATermError "TH_FORMULA" u

instance ShATermConvertible Mor where
  toShATermAux att0 xv = case xv of
    Mor -> return $ addATerm (ShAAppl "Mor" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Mor" [] _ -> (att0, Mor)
    u -> fromShATermError "Mor" u

instance ShATermConvertible s => ShATermConvertible (THybridSign s) where
  toShATermAux att0 xv = case xv of
    THybridSign a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "THybridSign" [a', b', c'] []) att3
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "THybridSign" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, THybridSign a' b' c') }}}
    u -> fromShATermError "THybridSign" u

_tcTHybridSignTc :: TyCon
_tcTHybridSignTc = mkTyCon "TopHybrid.TopHybridSign.THybridSign"
instance Typeable1 THybridSign where
    typeOf1 _ = mkTyConApp _tcTHybridSignTc []