{-# OPTIONS -w -O0 #-}
{- |
Module      :  ATC/AS_Architecture.der.hs
Description :  generated Typeable, ShATermLG instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermLG
  for the type(s):
'Syntax.AS_Architecture.ARCH_SPEC'
'Syntax.AS_Architecture.UNIT_DECL_DEFN'
'Syntax.AS_Architecture.UNIT_SPEC'
'Syntax.AS_Architecture.REF_SPEC'
'Syntax.AS_Architecture.UNIT_REF'
'Syntax.AS_Architecture.UNIT_EXPRESSION'
'Syntax.AS_Architecture.UNIT_BINDING'
'Syntax.AS_Architecture.UNIT_TERM'
'Syntax.AS_Architecture.FIT_ARG_UNIT'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Syntax/AS_Architecture.der.hs
-}

module ATC.AS_Architecture () where

import ATC.AS_Structured
import ATC.Grothendieck
import ATerm.Lib
import Common.AS_Annotation
import Common.IRI
import Common.Id
import Data.Typeable
import Syntax.AS_Architecture
import Syntax.AS_Structured

{-! for Syntax.AS_Architecture.ARCH_SPEC derive : Typeable !-}
{-! for Syntax.AS_Architecture.UNIT_DECL_DEFN derive : Typeable !-}
{-! for Syntax.AS_Architecture.UNIT_SPEC derive : Typeable !-}
{-! for Syntax.AS_Architecture.REF_SPEC derive : Typeable !-}
{-! for Syntax.AS_Architecture.UNIT_REF derive : Typeable !-}
{-! for Syntax.AS_Architecture.UNIT_EXPRESSION derive : Typeable !-}
{-! for Syntax.AS_Architecture.UNIT_BINDING derive : Typeable !-}
{-! for Syntax.AS_Architecture.UNIT_TERM derive : Typeable !-}
{-! for Syntax.AS_Architecture.FIT_ARG_UNIT derive : Typeable !-}

{-! for Syntax.AS_Architecture.ARCH_SPEC derive : ShATermLG !-}
{-! for Syntax.AS_Architecture.UNIT_DECL_DEFN derive : ShATermLG !-}
{-! for Syntax.AS_Architecture.UNIT_SPEC derive : ShATermLG !-}
{-! for Syntax.AS_Architecture.REF_SPEC derive : ShATermLG !-}
{-! for Syntax.AS_Architecture.UNIT_REF derive : ShATermLG !-}
{-! for Syntax.AS_Architecture.UNIT_EXPRESSION derive : ShATermLG !-}
{-! for Syntax.AS_Architecture.UNIT_BINDING derive : ShATermLG !-}
{-! for Syntax.AS_Architecture.UNIT_TERM derive : ShATermLG !-}
{-! for Syntax.AS_Architecture.FIT_ARG_UNIT derive : ShATermLG !-}

-- Generated by DrIFT, look but don't touch!

instance ShATermLG FIT_ARG_UNIT where
  toShATermLG att0 xv = case xv of
    Fit_arg_unit a b c -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      return $ addATerm (ShAAppl "Fit_arg_unit" [a', b', c'] []) att3
  fromShATermLG lg ix att0 = case getShATerm ix att0 of
    ShAAppl "Fit_arg_unit" [a, b, c] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      (att3, Fit_arg_unit a' b' c') }}}
    u -> fromShATermError "FIT_ARG_UNIT" u

instance ShATermLG UNIT_TERM where
  toShATermLG att0 xv = case xv of
    Unit_reduction a b -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      return $ addATerm (ShAAppl "Unit_reduction" [a', b'] []) att2
    Unit_translation a b -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      return $ addATerm (ShAAppl "Unit_translation" [a', b'] []) att2
    Amalgamation a b -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      return $ addATerm (ShAAppl "Amalgamation" [a', b'] []) att2
    Local_unit a b c -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      return $ addATerm (ShAAppl "Local_unit" [a', b', c'] []) att3
    Unit_appl a b c -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      return $ addATerm (ShAAppl "Unit_appl" [a', b', c'] []) att3
    Group_unit_term a b -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      return $ addATerm (ShAAppl "Group_unit_term" [a', b'] []) att2
  fromShATermLG lg ix att0 = case getShATerm ix att0 of
    ShAAppl "Unit_reduction" [a, b] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      (att2, Unit_reduction a' b') }}
    ShAAppl "Unit_translation" [a, b] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      (att2, Unit_translation a' b') }}
    ShAAppl "Amalgamation" [a, b] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      (att2, Amalgamation a' b') }}
    ShAAppl "Local_unit" [a, b, c] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      (att3, Local_unit a' b' c') }}}
    ShAAppl "Unit_appl" [a, b, c] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      (att3, Unit_appl a' b' c') }}}
    ShAAppl "Group_unit_term" [a, b] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      (att2, Group_unit_term a' b') }}
    u -> fromShATermError "UNIT_TERM" u

instance ShATermLG UNIT_BINDING where
  toShATermLG att0 xv = case xv of
    Unit_binding a b c -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      return $ addATerm (ShAAppl "Unit_binding" [a', b', c'] []) att3
  fromShATermLG lg ix att0 = case getShATerm ix att0 of
    ShAAppl "Unit_binding" [a, b, c] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      (att3, Unit_binding a' b' c') }}}
    u -> fromShATermError "UNIT_BINDING" u

instance ShATermLG UNIT_EXPRESSION where
  toShATermLG att0 xv = case xv of
    Unit_expression a b c -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      return $ addATerm (ShAAppl "Unit_expression" [a', b', c'] []) att3
  fromShATermLG lg ix att0 = case getShATerm ix att0 of
    ShAAppl "Unit_expression" [a, b, c] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      (att3, Unit_expression a' b' c') }}}
    u -> fromShATermError "UNIT_EXPRESSION" u

instance ShATermLG UNIT_REF where
  toShATermLG att0 xv = case xv of
    Unit_ref a b c -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      return $ addATerm (ShAAppl "Unit_ref" [a', b', c'] []) att3
  fromShATermLG lg ix att0 = case getShATerm ix att0 of
    ShAAppl "Unit_ref" [a, b, c] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      (att3, Unit_ref a' b' c') }}}
    u -> fromShATermError "UNIT_REF" u

instance ShATermLG REF_SPEC where
  toShATermLG att0 xv = case xv of
    Unit_spec a -> do
      (att1, a') <- toShATermLG' att0 a
      return $ addATerm (ShAAppl "Unit_spec" [a'] []) att1
    Refinement a b c d e -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      (att4, d') <- toShATermLG' att3 d
      (att5, e') <- toShATermLG' att4 e
      return $ addATerm (ShAAppl "Refinement" [a', b', c', d',
                                               e'] []) att5
    Arch_unit_spec a b -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      return $ addATerm (ShAAppl "Arch_unit_spec" [a', b'] []) att2
    Compose_ref a b -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      return $ addATerm (ShAAppl "Compose_ref" [a', b'] []) att2
    Component_ref a b -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      return $ addATerm (ShAAppl "Component_ref" [a', b'] []) att2
  fromShATermLG lg ix att0 = case getShATerm ix att0 of
    ShAAppl "Unit_spec" [a] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      (att1, Unit_spec a') }
    ShAAppl "Refinement" [a, b, c, d, e] _ ->
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
      (att5, Refinement a' b' c' d' e') }}}}}
    ShAAppl "Arch_unit_spec" [a, b] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      (att2, Arch_unit_spec a' b') }}
    ShAAppl "Compose_ref" [a, b] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      (att2, Compose_ref a' b') }}
    ShAAppl "Component_ref" [a, b] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      (att2, Component_ref a' b') }}
    u -> fromShATermError "REF_SPEC" u

instance ShATermLG UNIT_SPEC where
  toShATermLG att0 xv = case xv of
    Unit_type a b c -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      return $ addATerm (ShAAppl "Unit_type" [a', b', c'] []) att3
    Spec_name a -> do
      (att1, a') <- toShATermLG' att0 a
      return $ addATerm (ShAAppl "Spec_name" [a'] []) att1
    Closed_unit_spec a b -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      return $ addATerm (ShAAppl "Closed_unit_spec" [a', b'] []) att2
  fromShATermLG lg ix att0 = case getShATerm ix att0 of
    ShAAppl "Unit_type" [a, b, c] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      (att3, Unit_type a' b' c') }}}
    ShAAppl "Spec_name" [a] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      (att1, Spec_name a') }
    ShAAppl "Closed_unit_spec" [a, b] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      (att2, Closed_unit_spec a' b') }}
    u -> fromShATermError "UNIT_SPEC" u

instance ShATermLG UNIT_DECL_DEFN where
  toShATermLG att0 xv = case xv of
    Unit_decl a b c d -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      (att4, d') <- toShATermLG' att3 d
      return $ addATerm (ShAAppl "Unit_decl" [a', b', c', d'] []) att4
    Unit_defn a b c -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      return $ addATerm (ShAAppl "Unit_defn" [a', b', c'] []) att3
  fromShATermLG lg ix att0 = case getShATerm ix att0 of
    ShAAppl "Unit_decl" [a, b, c, d] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      case fromShATermLG' lg d att3 of
      { (att4, d') ->
      (att4, Unit_decl a' b' c' d') }}}}
    ShAAppl "Unit_defn" [a, b, c] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      (att3, Unit_defn a' b' c') }}}
    u -> fromShATermError "UNIT_DECL_DEFN" u

instance ShATermLG ARCH_SPEC where
  toShATermLG att0 xv = case xv of
    Basic_arch_spec a b c -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      (att3, c') <- toShATermLG' att2 c
      return $ addATerm (ShAAppl "Basic_arch_spec" [a', b', c'] []) att3
    Arch_spec_name a -> do
      (att1, a') <- toShATermLG' att0 a
      return $ addATerm (ShAAppl "Arch_spec_name" [a'] []) att1
    Group_arch_spec a b -> do
      (att1, a') <- toShATermLG' att0 a
      (att2, b') <- toShATermLG' att1 b
      return $ addATerm (ShAAppl "Group_arch_spec" [a', b'] []) att2
  fromShATermLG lg ix att0 = case getShATerm ix att0 of
    ShAAppl "Basic_arch_spec" [a, b, c] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      case fromShATermLG' lg c att2 of
      { (att3, c') ->
      (att3, Basic_arch_spec a' b' c') }}}
    ShAAppl "Arch_spec_name" [a] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      (att1, Arch_spec_name a') }
    ShAAppl "Group_arch_spec" [a, b] _ ->
      case fromShATermLG' lg a att0 of
      { (att1, a') ->
      case fromShATermLG' lg b att1 of
      { (att2, b') ->
      (att2, Group_arch_spec a' b') }}
    u -> fromShATermError "ARCH_SPEC" u

_tcFIT_ARG_UNITTc :: TyCon
_tcFIT_ARG_UNITTc = mkTyCon "Syntax.AS_Architecture.FIT_ARG_UNIT"
instance Typeable FIT_ARG_UNIT where
    typeOf _ = mkTyConApp _tcFIT_ARG_UNITTc []

_tcUNIT_TERMTc :: TyCon
_tcUNIT_TERMTc = mkTyCon "Syntax.AS_Architecture.UNIT_TERM"
instance Typeable UNIT_TERM where
    typeOf _ = mkTyConApp _tcUNIT_TERMTc []

_tcUNIT_BINDINGTc :: TyCon
_tcUNIT_BINDINGTc = mkTyCon "Syntax.AS_Architecture.UNIT_BINDING"
instance Typeable UNIT_BINDING where
    typeOf _ = mkTyConApp _tcUNIT_BINDINGTc []

_tcUNIT_EXPRESSIONTc :: TyCon
_tcUNIT_EXPRESSIONTc = mkTyCon "Syntax.AS_Architecture.UNIT_EXPRESSION"
instance Typeable UNIT_EXPRESSION where
    typeOf _ = mkTyConApp _tcUNIT_EXPRESSIONTc []

_tcUNIT_REFTc :: TyCon
_tcUNIT_REFTc = mkTyCon "Syntax.AS_Architecture.UNIT_REF"
instance Typeable UNIT_REF where
    typeOf _ = mkTyConApp _tcUNIT_REFTc []

_tcREF_SPECTc :: TyCon
_tcREF_SPECTc = mkTyCon "Syntax.AS_Architecture.REF_SPEC"
instance Typeable REF_SPEC where
    typeOf _ = mkTyConApp _tcREF_SPECTc []

_tcUNIT_SPECTc :: TyCon
_tcUNIT_SPECTc = mkTyCon "Syntax.AS_Architecture.UNIT_SPEC"
instance Typeable UNIT_SPEC where
    typeOf _ = mkTyConApp _tcUNIT_SPECTc []

_tcUNIT_DECL_DEFNTc :: TyCon
_tcUNIT_DECL_DEFNTc = mkTyCon "Syntax.AS_Architecture.UNIT_DECL_DEFN"
instance Typeable UNIT_DECL_DEFN where
    typeOf _ = mkTyConApp _tcUNIT_DECL_DEFNTc []

_tcARCH_SPECTc :: TyCon
_tcARCH_SPECTc = mkTyCon "Syntax.AS_Architecture.ARCH_SPEC"
instance Typeable ARCH_SPEC where
    typeOf _ = mkTyConApp _tcARCH_SPECTc []
