{-# OPTIONS -w -O0 #-}
{- |
Module      :  Temporal/ATC_Temporal.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'Temporal.AS_BASIC_Temporal.BASIC_SPEC'
'Temporal.AS_BASIC_Temporal.FORMULA'
'Temporal.AS_BASIC_Temporal.SYMB_ITEMS'
'Temporal.AS_BASIC_Temporal.SYMB'
'Temporal.AS_BASIC_Temporal.SYMB_MAP_ITEMS'
'Temporal.Sign.Sign'
'Temporal.Symbol.Symbol'
'Temporal.Morphism.Morphism'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Temporal/AS_BASIC_Temporal.hs
Temporal/Sign.hs
Temporal/Symbol.hs
Temporal/Morphism.hs
-}

module Temporal.ATC_Temporal () where

import ATerm.Lib
import CASL.ATC_CASL
import Common.Doc
import Common.DocUtils
import Common.Id
import Common.Id as Id
import Common.Result
import Data.Typeable
import Temporal.AS_BASIC_Temporal
import Temporal.Morphism
import Temporal.Sign
import Temporal.Sign as Sign
import Temporal.Symbol
import qualified Common.Id as Id
import qualified Common.Result as Result
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Temporal.AS_BASIC_Temporal as AS_BASIC
import qualified Temporal.Morphism as Morphism
import qualified Temporal.Sign as Sign

{-! for Temporal.AS_BASIC_Temporal.BASIC_SPEC derive : Typeable !-}
{-! for Temporal.AS_BASIC_Temporal.FORMULA derive : Typeable !-}
{-! for Temporal.AS_BASIC_Temporal.SYMB_ITEMS derive : Typeable !-}
{-! for Temporal.AS_BASIC_Temporal.SYMB derive : Typeable !-}
{-! for Temporal.AS_BASIC_Temporal.SYMB_MAP_ITEMS derive : Typeable !-}
{-! for Temporal.Sign.Sign derive : Typeable !-}
{-! for Temporal.Symbol.Symbol derive : Typeable !-}
{-! for Temporal.Morphism.Morphism derive : Typeable !-}

{-! for Temporal.AS_BASIC_Temporal.BASIC_SPEC derive : ShATermConvertible !-}
{-! for Temporal.AS_BASIC_Temporal.FORMULA derive : ShATermConvertible !-}
{-! for Temporal.AS_BASIC_Temporal.SYMB_ITEMS derive : ShATermConvertible !-}
{-! for Temporal.AS_BASIC_Temporal.SYMB derive : ShATermConvertible !-}
{-! for Temporal.AS_BASIC_Temporal.SYMB_MAP_ITEMS derive : ShATermConvertible !-}
{-! for Temporal.Sign.Sign derive : ShATermConvertible !-}
{-! for Temporal.Symbol.Symbol derive : ShATermConvertible !-}
{-! for Temporal.Morphism.Morphism derive : ShATermConvertible !-}

-- Generated by DrIFT, look but don't touch!

_tcBASIC_SPECTc :: TyCon
_tcBASIC_SPECTc = mkTyCon "Temporal.AS_BASIC_Temporal.BASIC_SPEC"
instance Typeable BASIC_SPEC where
    typeOf _ = mkTyConApp _tcBASIC_SPECTc []

_tcFORMULATc :: TyCon
_tcFORMULATc = mkTyCon "Temporal.AS_BASIC_Temporal.FORMULA"
instance Typeable FORMULA where
    typeOf _ = mkTyConApp _tcFORMULATc []

_tcSYMB_ITEMSTc :: TyCon
_tcSYMB_ITEMSTc = mkTyCon "Temporal.AS_BASIC_Temporal.SYMB_ITEMS"
instance Typeable SYMB_ITEMS where
    typeOf _ = mkTyConApp _tcSYMB_ITEMSTc []

_tcSYMBTc :: TyCon
_tcSYMBTc = mkTyCon "Temporal.AS_BASIC_Temporal.SYMB"
instance Typeable SYMB where
    typeOf _ = mkTyConApp _tcSYMBTc []

_tcSYMB_MAP_ITEMSTc :: TyCon
_tcSYMB_MAP_ITEMSTc = mkTyCon "Temporal.AS_BASIC_Temporal.SYMB_MAP_ITEMS"
instance Typeable SYMB_MAP_ITEMS where
    typeOf _ = mkTyConApp _tcSYMB_MAP_ITEMSTc []

instance ShATermConvertible BASIC_SPEC where
  toShATermAux att0 xv = case xv of
    Basic_spec -> return $ addATerm (ShAAppl "Basic_spec" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Basic_spec" [] _ -> (att0, Basic_spec)
    u -> fromShATermError "BASIC_SPEC" u

instance ShATermConvertible FORMULA where
  toShATermAux att0 xv = case xv of
    Formula -> return $ addATerm (ShAAppl "Formula" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Formula" [] _ -> (att0, Formula)
    u -> fromShATermError "FORMULA" u

instance ShATermConvertible SYMB_ITEMS where
  toShATermAux att0 xv = case xv of
    Symb_items -> return $ addATerm (ShAAppl "Symb_items" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Symb_items" [] _ -> (att0, Symb_items)
    u -> fromShATermError "SYMB_ITEMS" u

instance ShATermConvertible SYMB where
  toShATermAux att0 xv = case xv of
    Symb_id -> return $ addATerm (ShAAppl "Symb_id" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Symb_id" [] _ -> (att0, Symb_id)
    u -> fromShATermError "SYMB" u

instance ShATermConvertible SYMB_MAP_ITEMS where
  toShATermAux att0 xv = case xv of
    Symb_map_items -> return $ addATerm (ShAAppl "Symb_map_items" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Symb_map_items" [] _ -> (att0, Symb_map_items)
    u -> fromShATermError "SYMB_MAP_ITEMS" u

instance ShATermConvertible Morphism where
  toShATermAux att0 xv = case xv of
    Morphism a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Morphism" [a', b', c'] []) att3
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Morphism" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Morphism a' b' c') }}}
    u -> fromShATermError "Morphism" u

_tcMorphismTc :: TyCon
_tcMorphismTc = mkTyCon "Temporal.Morphism.Morphism"
instance Typeable Morphism where
    typeOf _ = mkTyConApp _tcMorphismTc []

_tcSignTc :: TyCon
_tcSignTc = mkTyCon "Temporal.Sign.Sign"
instance Typeable Sign where
    typeOf _ = mkTyConApp _tcSignTc []

instance ShATermConvertible Sign where
  toShATermAux att0 xv = case xv of
    Sign a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Sign" [a'] []) att1
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Sign" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Sign a') }
    u -> fromShATermError "Sign" u

_tcSymbolTc :: TyCon
_tcSymbolTc = mkTyCon "Temporal.Symbol.Symbol"
instance Typeable Symbol where
    typeOf _ = mkTyConApp _tcSymbolTc []

instance ShATermConvertible Symbol where
  toShATermAux att0 xv = case xv of
    Symbol a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Symbol" [a'] []) att1
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Symbol" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Symbol a') }
    u -> fromShATermError "Symbol" u
