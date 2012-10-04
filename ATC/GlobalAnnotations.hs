{-# OPTIONS -w -O0 #-}
{- |
Module      :  ATC/GlobalAnnotations.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'Common.GlobalAnnotations.GlobalAnnos'
'Common.GlobalAnnotations.LiteralAnnos'
'Common.GlobalAnnotations.LiteralType'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Common/GlobalAnnotations.hs
-}

module ATC.GlobalAnnotations () where

import ATC.AS_Annotation
import ATC.Result
import ATerm.Lib
import Common.AS_Annotation
import Common.GlobalAnnotations
import Common.IRI (IRI)
import Common.Id
import Data.Typeable
import qualified Common.Lib.Rel as Rel
import qualified Data.Map as Map

{-! for Common.GlobalAnnotations.GlobalAnnos derive : Typeable !-}
{-! for Common.GlobalAnnotations.LiteralAnnos derive : Typeable !-}
{-! for Common.GlobalAnnotations.LiteralType derive : Typeable !-}

{-! for Common.GlobalAnnotations.GlobalAnnos derive : ShATermConvertible !-}
{-! for Common.GlobalAnnotations.LiteralAnnos derive : ShATermConvertible !-}
{-! for Common.GlobalAnnotations.LiteralType derive : ShATermConvertible !-}

-- Generated by DrIFT, look but don't touch!

instance ShATermConvertible LiteralType where
  toShATermAux att0 xv = case xv of
    StringCons a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "StringCons" [a'] []) att1
    StringNull -> return $ addATerm (ShAAppl "StringNull" [] []) att0
    ListCons a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "ListCons" [a', b'] []) att2
    ListNull a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "ListNull" [a'] []) att1
    Number -> return $ addATerm (ShAAppl "Number" [] []) att0
    Fraction -> return $ addATerm (ShAAppl "Fraction" [] []) att0
    Floating -> return $ addATerm (ShAAppl "Floating" [] []) att0
    NoLiteral -> return $ addATerm (ShAAppl "NoLiteral" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "StringCons" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, StringCons a') }
    ShAAppl "StringNull" [] _ -> (att0, StringNull)
    ShAAppl "ListCons" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, ListCons a' b') }}
    ShAAppl "ListNull" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, ListNull a') }
    ShAAppl "Number" [] _ -> (att0, Number)
    ShAAppl "Fraction" [] _ -> (att0, Fraction)
    ShAAppl "Floating" [] _ -> (att0, Floating)
    ShAAppl "NoLiteral" [] _ -> (att0, NoLiteral)
    u -> fromShATermError "LiteralType" u

instance ShATermConvertible LiteralAnnos where
  toShATermAux att0 xv = case xv of
    LA a b c d -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      return $ addATerm (ShAAppl "LA" [a', b', c', d'] []) att4
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "LA" [a, b, c, d] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      case fromShATerm' d att3 of
      { (att4, d') ->
      (att4, LA a' b' c' d') }}}}
    u -> fromShATermError "LiteralAnnos" u

instance ShATermConvertible GlobalAnnos where
  toShATermAux att0 xv = case xv of
    GA a b c d e f -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      (att5, e') <- toShATerm' att4 e
      (att6, f') <- toShATerm' att5 f
      return $ addATerm (ShAAppl "GA" [a', b', c', d', e', f'] []) att6
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "GA" [a, b, c, d, e, f] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      case fromShATerm' d att3 of
      { (att4, d') ->
      case fromShATerm' e att4 of
      { (att5, e') ->
      case fromShATerm' f att5 of
      { (att6, f') ->
      (att6, GA a' b' c' d' e' f') }}}}}}
    u -> fromShATermError "GlobalAnnos" u

_tcLiteralTypeTc :: TyCon
_tcLiteralTypeTc = mkTyCon "Common.GlobalAnnotations.LiteralType"
instance Typeable LiteralType where
    typeOf _ = mkTyConApp _tcLiteralTypeTc []

_tcLiteralAnnosTc :: TyCon
_tcLiteralAnnosTc = mkTyCon "Common.GlobalAnnotations.LiteralAnnos"
instance Typeable LiteralAnnos where
    typeOf _ = mkTyConApp _tcLiteralAnnosTc []

_tcGlobalAnnosTc :: TyCon
_tcGlobalAnnosTc = mkTyCon "Common.GlobalAnnotations.GlobalAnnos"
instance Typeable GlobalAnnos where
    typeOf _ = mkTyConApp _tcGlobalAnnosTc []
