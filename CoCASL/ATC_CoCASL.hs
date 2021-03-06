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

-- Generated by DrIFT, look but don't touch!

_tcC_BASIC_ITEMTc :: TyCon
_tcC_BASIC_ITEMTc = mkTyCon "CoCASL.AS_CoCASL.C_BASIC_ITEM"
instance Typeable C_BASIC_ITEM where
    typeOf _ = mkTyConApp _tcC_BASIC_ITEMTc []

_tcC_SIG_ITEMTc :: TyCon
_tcC_SIG_ITEMTc = mkTyCon "CoCASL.AS_CoCASL.C_SIG_ITEM"
instance Typeable C_SIG_ITEM where
    typeOf _ = mkTyConApp _tcC_SIG_ITEMTc []

_tcCODATATYPE_DECLTc :: TyCon
_tcCODATATYPE_DECLTc = mkTyCon "CoCASL.AS_CoCASL.CODATATYPE_DECL"
instance Typeable CODATATYPE_DECL where
    typeOf _ = mkTyConApp _tcCODATATYPE_DECLTc []

_tcCOALTERNATIVETc :: TyCon
_tcCOALTERNATIVETc = mkTyCon "CoCASL.AS_CoCASL.COALTERNATIVE"
instance Typeable COALTERNATIVE where
    typeOf _ = mkTyConApp _tcCOALTERNATIVETc []

_tcCOCOMPONENTSTc :: TyCon
_tcCOCOMPONENTSTc = mkTyCon "CoCASL.AS_CoCASL.COCOMPONENTS"
instance Typeable COCOMPONENTS where
    typeOf _ = mkTyConApp _tcCOCOMPONENTSTc []

_tcMODALITYTc :: TyCon
_tcMODALITYTc = mkTyCon "CoCASL.AS_CoCASL.MODALITY"
instance Typeable MODALITY where
    typeOf _ = mkTyConApp _tcMODALITYTc []

_tcC_FORMULATc :: TyCon
_tcC_FORMULATc = mkTyCon "CoCASL.AS_CoCASL.C_FORMULA"
instance Typeable C_FORMULA where
    typeOf _ = mkTyConApp _tcC_FORMULATc []

instance ShATermConvertible C_BASIC_ITEM where
  toShATermAux att0 xv = case xv of
    CoFree_datatype a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "CoFree_datatype" [a', b'] []) att2
    CoSort_gen a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "CoSort_gen" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "CoFree_datatype" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, CoFree_datatype a' b') }}
    ShAAppl "CoSort_gen" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, CoSort_gen a' b') }}
    u -> fromShATermError "C_BASIC_ITEM" u

instance ShATermConvertible C_SIG_ITEM where
  toShATermAux att0 xv = case xv of
    CoDatatype_items a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "CoDatatype_items" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "CoDatatype_items" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, CoDatatype_items a' b') }}
    u -> fromShATermError "C_SIG_ITEM" u

instance ShATermConvertible CODATATYPE_DECL where
  toShATermAux att0 xv = case xv of
    CoDatatype_decl a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "CoDatatype_decl" [a', b', c'] []) att3
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "CoDatatype_decl" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, CoDatatype_decl a' b' c') }}}
    u -> fromShATermError "CODATATYPE_DECL" u

instance ShATermConvertible COALTERNATIVE where
  toShATermAux att0 xv = case xv of
    Co_construct a b c d -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      return $ addATerm (ShAAppl "Co_construct" [a', b', c', d'] []) att4
    CoSubsorts a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "CoSubsorts" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Co_construct" [a, b, c, d] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      case fromShATerm' d att3 of
      { (att4, d') ->
      (att4, Co_construct a' b' c' d') }}}}
    ShAAppl "CoSubsorts" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, CoSubsorts a' b') }}
    u -> fromShATermError "COALTERNATIVE" u

instance ShATermConvertible COCOMPONENTS where
  toShATermAux att0 xv = case xv of
    CoSelect a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "CoSelect" [a', b', c'] []) att3
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "CoSelect" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, CoSelect a' b' c') }}}
    u -> fromShATermError "COCOMPONENTS" u

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

instance ShATermConvertible C_FORMULA where
  toShATermAux att0 xv = case xv of
    BoxOrDiamond a b c d -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      return $ addATerm (ShAAppl "BoxOrDiamond" [a', b', c', d'] []) att4
    CoSort_gen_ax a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "CoSort_gen_ax" [a', b', c'] []) att3
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
    ShAAppl "CoSort_gen_ax" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, CoSort_gen_ax a' b' c') }}}
    u -> fromShATermError "C_FORMULA" u

instance ShATermConvertible CoCASLSign where
  toShATermAux att0 xv = case xv of
    CoCASLSign a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "CoCASLSign" [a', b', c'] []) att3
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "CoCASLSign" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, CoCASLSign a' b' c') }}}
    u -> fromShATermError "CoCASLSign" u

_tcCoCASLSignTc :: TyCon
_tcCoCASLSignTc = mkTyCon "CoCASL.CoCASLSign.CoCASLSign"
instance Typeable CoCASLSign where
    typeOf _ = mkTyConApp _tcCoCASLSignTc []
