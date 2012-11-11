{-# OPTIONS -w -O0 #-}
{- |
Module      :  Modal/ATC_Modal.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'Modal.AS_Modal.M_BASIC_ITEM'
'Modal.AS_Modal.RIGOR'
'Modal.AS_Modal.M_SIG_ITEM'
'Modal.AS_Modal.MODALITY'
'Modal.AS_Modal.M_FORMULA'
'Modal.ModalSign.ModalSign'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Modal/AS_Modal.hs
Modal/ModalSign.hs
-}

module Modal.ATC_Modal () where

import ATerm.Lib
import CASL.AS_Basic_CASL
import CASL.ATC_CASL
import CASL.Sign
import Common.AS_Annotation
import Common.Id
import Data.Typeable
import Modal.AS_Modal
import Modal.ModalSign
import qualified Common.Lib.MapSet as MapSet
import qualified Data.List as List
import qualified Data.Map as Map

{-! for Modal.AS_Modal.M_BASIC_ITEM derive : Typeable !-}
{-! for Modal.AS_Modal.RIGOR derive : Typeable !-}
{-! for Modal.AS_Modal.M_SIG_ITEM derive : Typeable !-}
{-! for Modal.AS_Modal.MODALITY derive : Typeable !-}
{-! for Modal.AS_Modal.M_FORMULA derive : Typeable !-}
{-! for Modal.ModalSign.ModalSign derive : Typeable !-}

{-! for Modal.AS_Modal.M_BASIC_ITEM derive : ShATermConvertible !-}
{-! for Modal.AS_Modal.RIGOR derive : ShATermConvertible !-}
{-! for Modal.AS_Modal.M_SIG_ITEM derive : ShATermConvertible !-}
{-! for Modal.AS_Modal.MODALITY derive : ShATermConvertible !-}
{-! for Modal.AS_Modal.M_FORMULA derive : ShATermConvertible !-}
{-! for Modal.ModalSign.ModalSign derive : ShATermConvertible !-}

-- Generated by DrIFT, look but don't touch!

instance ShATermConvertible M_FORMULA where
  toShATermAux att0 xv = case xv of
    BoxOrDiamond a b c d -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      return $ addATerm (ShAAppl "BoxOrDiamond" [a', b', c', d'] []) att4
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "BoxOrDiamond" [a, b, c, d] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      case fromShATerm' d att3 of
      { (att4, d') ->
      (att4, BoxOrDiamond a' b' c' d') }}}}
    u -> fromShATermError "M_FORMULA" u

instance ShATermConvertible MODALITY where
  toShATermAux att0 xv = case xv of
    Simple_mod a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Simple_mod" [a'] []) att1
    Term_mod a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Term_mod" [a'] []) att1
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Simple_mod" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Simple_mod a') }
    ShAAppl "Term_mod" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Term_mod a') }
    u -> fromShATermError "MODALITY" u

instance ShATermConvertible M_SIG_ITEM where
  toShATermAux att0 xv = case xv of
    Rigid_op_items a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Rigid_op_items" [a', b', c'] []) att3
    Rigid_pred_items a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Rigid_pred_items" [a', b', c'] []) att3
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Rigid_op_items" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Rigid_op_items a' b' c') }}}
    ShAAppl "Rigid_pred_items" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Rigid_pred_items a' b' c') }}}
    u -> fromShATermError "M_SIG_ITEM" u

instance ShATermConvertible RIGOR where
  toShATermAux att0 xv = case xv of
    Rigid -> return $ addATerm (ShAAppl "Rigid" [] []) att0
    Flexible -> return $ addATerm (ShAAppl "Flexible" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Rigid" [] _ -> (att0, Rigid)
    ShAAppl "Flexible" [] _ -> (att0, Flexible)
    u -> fromShATermError "RIGOR" u

instance ShATermConvertible M_BASIC_ITEM where
  toShATermAux att0 xv = case xv of
    Simple_mod_decl a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Simple_mod_decl" [a', b', c'] []) att3
    Term_mod_decl a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Term_mod_decl" [a', b', c'] []) att3
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Simple_mod_decl" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Simple_mod_decl a' b' c') }}}
    ShAAppl "Term_mod_decl" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Term_mod_decl a' b' c') }}}
    u -> fromShATermError "M_BASIC_ITEM" u

_tcM_FORMULATc :: TyCon
_tcM_FORMULATc = mkTyCon "Modal.AS_Modal.M_FORMULA"
instance Typeable M_FORMULA where
    typeOf _ = mkTyConApp _tcM_FORMULATc []

_tcMODALITYTc :: TyCon
_tcMODALITYTc = mkTyCon "Modal.AS_Modal.MODALITY"
instance Typeable MODALITY where
    typeOf _ = mkTyConApp _tcMODALITYTc []

_tcM_SIG_ITEMTc :: TyCon
_tcM_SIG_ITEMTc = mkTyCon "Modal.AS_Modal.M_SIG_ITEM"
instance Typeable M_SIG_ITEM where
    typeOf _ = mkTyConApp _tcM_SIG_ITEMTc []

_tcRIGORTc :: TyCon
_tcRIGORTc = mkTyCon "Modal.AS_Modal.RIGOR"
instance Typeable RIGOR where
    typeOf _ = mkTyConApp _tcRIGORTc []

_tcM_BASIC_ITEMTc :: TyCon
_tcM_BASIC_ITEMTc = mkTyCon "Modal.AS_Modal.M_BASIC_ITEM"
instance Typeable M_BASIC_ITEM where
    typeOf _ = mkTyConApp _tcM_BASIC_ITEMTc []

_tcModalSignTc :: TyCon
_tcModalSignTc = mkTyCon "Modal.ModalSign.ModalSign"
instance Typeable ModalSign where
    typeOf _ = mkTyConApp _tcModalSignTc []

instance ShATermConvertible ModalSign where
  toShATermAux att0 xv = case xv of
    ModalSign a b c d -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      return $ addATerm (ShAAppl "ModalSign" [a', b', c', d'] []) att4
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "ModalSign" [a, b, c, d] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      case fromShATerm' d att3 of
      { (att4, d') ->
      (att4, ModalSign a' b' c' d') }}}}
    u -> fromShATermError "ModalSign" u
