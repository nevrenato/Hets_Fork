{-# OPTIONS -w -O0 #-}
{- |
Module      :  Isabelle/ATC_Isabelle.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'Isabelle.IsaSign.VName'
'Isabelle.IsaSign.AltSyntax'
'Isabelle.IsaSign.Indexname'
'Isabelle.IsaSign.IsaClass'
'Isabelle.IsaSign.Typ'
'Isabelle.IsaSign.Continuity'
'Isabelle.IsaSign.TAttr'
'Isabelle.IsaSign.DTyp'
'Isabelle.IsaSign.Term'
'Isabelle.IsaSign.Sentence'
'Isabelle.IsaSign.SetDecl'
'Isabelle.IsaSign.MetaTerm'
'Isabelle.IsaSign.TypeSig'
'Isabelle.IsaSign.BaseSig'
'Isabelle.IsaSign.Sign'
'Isabelle.IsaSign.IsaProof'
'Isabelle.IsaSign.ProofCommand'
'Isabelle.IsaSign.ProofEnd'
'Isabelle.IsaSign.Modifier'
'Isabelle.IsaSign.ProofMethod'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Isabelle/IsaSign.hs
-}

module Isabelle.ATC_Isabelle () where

import ATerm.Lib
import Data.List
import Data.Typeable
import Isabelle.IsaSign
import qualified Data.Map as Map

{-! for Isabelle.IsaSign.VName derive : Typeable !-}
{-! for Isabelle.IsaSign.AltSyntax derive : Typeable !-}
{-! for Isabelle.IsaSign.Indexname derive : Typeable !-}
{-! for Isabelle.IsaSign.IsaClass derive : Typeable !-}
{-! for Isabelle.IsaSign.Typ derive : Typeable !-}
{-! for Isabelle.IsaSign.Continuity derive : Typeable !-}
{-! for Isabelle.IsaSign.TAttr derive : Typeable !-}
{-! for Isabelle.IsaSign.DTyp derive : Typeable !-}
{-! for Isabelle.IsaSign.Term derive : Typeable !-}
{-! for Isabelle.IsaSign.Sentence derive : Typeable !-}
{-! for Isabelle.IsaSign.SetDecl derive : Typeable !-}
{-! for Isabelle.IsaSign.MetaTerm derive : Typeable !-}
{-! for Isabelle.IsaSign.TypeSig derive : Typeable !-}
{-! for Isabelle.IsaSign.BaseSig derive : Typeable !-}
{-! for Isabelle.IsaSign.Sign derive : Typeable !-}
{-! for Isabelle.IsaSign.IsaProof derive : Typeable !-}
{-! for Isabelle.IsaSign.ProofCommand derive : Typeable !-}
{-! for Isabelle.IsaSign.ProofEnd derive : Typeable !-}
{-! for Isabelle.IsaSign.Modifier derive : Typeable !-}
{-! for Isabelle.IsaSign.ProofMethod derive : Typeable !-}

{-! for Isabelle.IsaSign.VName derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.AltSyntax derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.Indexname derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.IsaClass derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.Typ derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.Continuity derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.TAttr derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.DTyp derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.Term derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.Sentence derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.SetDecl derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.MetaTerm derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.TypeSig derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.BaseSig derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.Sign derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.IsaProof derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.ProofCommand derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.ProofEnd derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.Modifier derive : ShATermConvertible !-}
{-! for Isabelle.IsaSign.ProofMethod derive : ShATermConvertible !-}

-- Generated by DrIFT, look but don't touch!

_tcVNameTc :: TyCon
_tcVNameTc = mkTyCon "Isabelle.IsaSign.VName"
instance Typeable VName where
    typeOf _ = mkTyConApp _tcVNameTc []

_tcAltSyntaxTc :: TyCon
_tcAltSyntaxTc = mkTyCon "Isabelle.IsaSign.AltSyntax"
instance Typeable AltSyntax where
    typeOf _ = mkTyConApp _tcAltSyntaxTc []

_tcIndexnameTc :: TyCon
_tcIndexnameTc = mkTyCon "Isabelle.IsaSign.Indexname"
instance Typeable Indexname where
    typeOf _ = mkTyConApp _tcIndexnameTc []

_tcIsaClassTc :: TyCon
_tcIsaClassTc = mkTyCon "Isabelle.IsaSign.IsaClass"
instance Typeable IsaClass where
    typeOf _ = mkTyConApp _tcIsaClassTc []

_tcTypTc :: TyCon
_tcTypTc = mkTyCon "Isabelle.IsaSign.Typ"
instance Typeable Typ where
    typeOf _ = mkTyConApp _tcTypTc []

_tcContinuityTc :: TyCon
_tcContinuityTc = mkTyCon "Isabelle.IsaSign.Continuity"
instance Typeable Continuity where
    typeOf _ = mkTyConApp _tcContinuityTc []

_tcTAttrTc :: TyCon
_tcTAttrTc = mkTyCon "Isabelle.IsaSign.TAttr"
instance Typeable TAttr where
    typeOf _ = mkTyConApp _tcTAttrTc []

_tcDTypTc :: TyCon
_tcDTypTc = mkTyCon "Isabelle.IsaSign.DTyp"
instance Typeable DTyp where
    typeOf _ = mkTyConApp _tcDTypTc []

_tcTermTc :: TyCon
_tcTermTc = mkTyCon "Isabelle.IsaSign.Term"
instance Typeable Term where
    typeOf _ = mkTyConApp _tcTermTc []

_tcSentenceTc :: TyCon
_tcSentenceTc = mkTyCon "Isabelle.IsaSign.Sentence"
instance Typeable Sentence where
    typeOf _ = mkTyConApp _tcSentenceTc []

_tcSetDeclTc :: TyCon
_tcSetDeclTc = mkTyCon "Isabelle.IsaSign.SetDecl"
instance Typeable SetDecl where
    typeOf _ = mkTyConApp _tcSetDeclTc []

_tcMetaTermTc :: TyCon
_tcMetaTermTc = mkTyCon "Isabelle.IsaSign.MetaTerm"
instance Typeable MetaTerm where
    typeOf _ = mkTyConApp _tcMetaTermTc []

_tcTypeSigTc :: TyCon
_tcTypeSigTc = mkTyCon "Isabelle.IsaSign.TypeSig"
instance Typeable TypeSig where
    typeOf _ = mkTyConApp _tcTypeSigTc []

_tcBaseSigTc :: TyCon
_tcBaseSigTc = mkTyCon "Isabelle.IsaSign.BaseSig"
instance Typeable BaseSig where
    typeOf _ = mkTyConApp _tcBaseSigTc []

_tcSignTc :: TyCon
_tcSignTc = mkTyCon "Isabelle.IsaSign.Sign"
instance Typeable Sign where
    typeOf _ = mkTyConApp _tcSignTc []

_tcIsaProofTc :: TyCon
_tcIsaProofTc = mkTyCon "Isabelle.IsaSign.IsaProof"
instance Typeable IsaProof where
    typeOf _ = mkTyConApp _tcIsaProofTc []

_tcProofCommandTc :: TyCon
_tcProofCommandTc = mkTyCon "Isabelle.IsaSign.ProofCommand"
instance Typeable ProofCommand where
    typeOf _ = mkTyConApp _tcProofCommandTc []

_tcProofEndTc :: TyCon
_tcProofEndTc = mkTyCon "Isabelle.IsaSign.ProofEnd"
instance Typeable ProofEnd where
    typeOf _ = mkTyConApp _tcProofEndTc []

_tcModifierTc :: TyCon
_tcModifierTc = mkTyCon "Isabelle.IsaSign.Modifier"
instance Typeable Modifier where
    typeOf _ = mkTyConApp _tcModifierTc []

_tcProofMethodTc :: TyCon
_tcProofMethodTc = mkTyCon "Isabelle.IsaSign.ProofMethod"
instance Typeable ProofMethod where
    typeOf _ = mkTyConApp _tcProofMethodTc []

instance ShATermConvertible VName where
  toShATermAux att0 xv = case xv of
    VName a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "VName" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "VName" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, VName a' b') }}
    u -> fromShATermError "VName" u

instance ShATermConvertible AltSyntax where
  toShATermAux att0 xv = case xv of
    AltSyntax a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "AltSyntax" [a', b', c'] []) att3
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "AltSyntax" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, AltSyntax a' b' c') }}}
    u -> fromShATermError "AltSyntax" u

instance ShATermConvertible Indexname where
  toShATermAux att0 xv = case xv of
    Indexname a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Indexname" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Indexname" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Indexname a' b') }}
    u -> fromShATermError "Indexname" u

instance ShATermConvertible IsaClass where
  toShATermAux att0 xv = case xv of
    IsaClass a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "IsaClass" [a'] []) att1
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "IsaClass" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, IsaClass a') }
    u -> fromShATermError "IsaClass" u

instance ShATermConvertible Typ where
  toShATermAux att0 xv = case xv of
    Type a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Type" [a', b', c'] []) att3
    TFree a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "TFree" [a', b'] []) att2
    TVar a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "TVar" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Type" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Type a' b' c') }}}
    ShAAppl "TFree" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, TFree a' b') }}
    ShAAppl "TVar" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, TVar a' b') }}
    u -> fromShATermError "Typ" u

instance ShATermConvertible Continuity where
  toShATermAux att0 xv = case xv of
    IsCont a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "IsCont" [a'] []) att1
    NotCont -> return $ addATerm (ShAAppl "NotCont" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "IsCont" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, IsCont a') }
    ShAAppl "NotCont" [] _ -> (att0, NotCont)
    u -> fromShATermError "Continuity" u

instance ShATermConvertible TAttr where
  toShATermAux att0 xv = case xv of
    TFun -> return $ addATerm (ShAAppl "TFun" [] []) att0
    TMet -> return $ addATerm (ShAAppl "TMet" [] []) att0
    TCon -> return $ addATerm (ShAAppl "TCon" [] []) att0
    NA -> return $ addATerm (ShAAppl "NA" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "TFun" [] _ -> (att0, TFun)
    ShAAppl "TMet" [] _ -> (att0, TMet)
    ShAAppl "TCon" [] _ -> (att0, TCon)
    ShAAppl "NA" [] _ -> (att0, NA)
    u -> fromShATermError "TAttr" u

instance ShATermConvertible DTyp where
  toShATermAux att0 xv = case xv of
    Hide a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Hide" [a', b', c'] []) att3
    Disp a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Disp" [a', b', c'] []) att3
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Hide" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Hide a' b' c') }}}
    ShAAppl "Disp" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Disp a' b' c') }}}
    u -> fromShATermError "DTyp" u

instance ShATermConvertible Term where
  toShATermAux att0 xv = case xv of
    Const a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Const" [a', b'] []) att2
    Free a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Free" [a'] []) att1
    Abs a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Abs" [a', b', c'] []) att3
    App a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "App" [a', b', c'] []) att3
    If a b c d -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      return $ addATerm (ShAAppl "If" [a', b', c', d'] []) att4
    Case a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Case" [a', b'] []) att2
    Let a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Let" [a', b'] []) att2
    IsaEq a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "IsaEq" [a', b'] []) att2
    Tuplex a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Tuplex" [a', b'] []) att2
    Set a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Set" [a'] []) att1
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Const" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Const a' b') }}
    ShAAppl "Free" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Free a') }
    ShAAppl "Abs" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Abs a' b' c') }}}
    ShAAppl "App" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, App a' b' c') }}}
    ShAAppl "If" [a, b, c, d] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      case fromShATerm' d att3 of
      { (att4, d') ->
      (att4, If a' b' c' d') }}}}
    ShAAppl "Case" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Case a' b') }}
    ShAAppl "Let" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Let a' b') }}
    ShAAppl "IsaEq" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, IsaEq a' b') }}
    ShAAppl "Tuplex" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Tuplex a' b') }}
    ShAAppl "Set" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Set a') }
    u -> fromShATermError "Term" u

instance ShATermConvertible Sentence where
  toShATermAux att0 xv = case xv of
    Sentence a b c d -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      return $ addATerm (ShAAppl "Sentence" [a', b', c', d'] []) att4
    Instance a b c d e -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      (att5, e') <- toShATerm' att4 e
      return $ addATerm (ShAAppl "Instance" [a', b', c', d', e'] []) att5
    ConstDef a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "ConstDef" [a'] []) att1
    RecDef a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "RecDef" [a', b'] []) att2
    PrimRecDef a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "PrimRecDef" [a', b', c'] []) att3
    TypeDef a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "TypeDef" [a', b', c'] []) att3
    Lemmas a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Lemmas" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Sentence" [a, b, c, d] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      case fromShATerm' d att3 of
      { (att4, d') ->
      (att4, Sentence a' b' c' d') }}}}
    ShAAppl "Instance" [a, b, c, d, e] _ ->
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
      (att5, Instance a' b' c' d' e') }}}}}
    ShAAppl "ConstDef" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, ConstDef a') }
    ShAAppl "RecDef" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, RecDef a' b') }}
    ShAAppl "PrimRecDef" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, PrimRecDef a' b' c') }}}
    ShAAppl "TypeDef" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, TypeDef a' b' c') }}}
    ShAAppl "Lemmas" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Lemmas a' b') }}
    u -> fromShATermError "Sentence" u

instance ShATermConvertible SetDecl where
  toShATermAux att0 xv = case xv of
    SubSet a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "SubSet" [a', b', c'] []) att3
    FixedSet a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "FixedSet" [a'] []) att1
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "SubSet" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, SubSet a' b' c') }}}
    ShAAppl "FixedSet" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, FixedSet a') }
    u -> fromShATermError "SetDecl" u

instance ShATermConvertible MetaTerm where
  toShATermAux att0 xv = case xv of
    Term a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Term" [a'] []) att1
    Conditional a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Conditional" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Term" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Term a') }
    ShAAppl "Conditional" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Conditional a' b') }}
    u -> fromShATermError "MetaTerm" u

instance ShATermConvertible TypeSig where
  toShATermAux att0 xv = case xv of
    TySg a b c d e f -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      (att5, e') <- toShATerm' att4 e
      (att6, f') <- toShATerm' att5 f
      return $ addATerm (ShAAppl "TySg" [a', b', c', d', e', f'] []) att6
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "TySg" [a, b, c, d, e, f] _ ->
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
      (att6, TySg a' b' c' d' e' f') }}}}}}
    u -> fromShATermError "TypeSig" u

instance ShATermConvertible BaseSig where
  toShATermAux att0 xv = case xv of
    Main_thy -> return $ addATerm (ShAAppl "Main_thy" [] []) att0
    MainHC_thy -> return $ addATerm (ShAAppl "MainHC_thy" [] []) att0
    MainHCPairs_thy -> return $ addATerm (ShAAppl "MainHCPairs_thy" [] []) att0
    HOLCF_thy -> return $ addATerm (ShAAppl "HOLCF_thy" [] []) att0
    HsHOLCF_thy -> return $ addATerm (ShAAppl "HsHOLCF_thy" [] []) att0
    HsHOL_thy -> return $ addATerm (ShAAppl "HsHOL_thy" [] []) att0
    MHsHOL_thy -> return $ addATerm (ShAAppl "MHsHOL_thy" [] []) att0
    MHsHOLCF_thy -> return $ addATerm (ShAAppl "MHsHOLCF_thy" [] []) att0
    CspHOLComplex_thy ->
      return $ addATerm (ShAAppl "CspHOLComplex_thy" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Main_thy" [] _ -> (att0, Main_thy)
    ShAAppl "MainHC_thy" [] _ -> (att0, MainHC_thy)
    ShAAppl "MainHCPairs_thy" [] _ -> (att0, MainHCPairs_thy)
    ShAAppl "HOLCF_thy" [] _ -> (att0, HOLCF_thy)
    ShAAppl "HsHOLCF_thy" [] _ -> (att0, HsHOLCF_thy)
    ShAAppl "HsHOL_thy" [] _ -> (att0, HsHOL_thy)
    ShAAppl "MHsHOL_thy" [] _ -> (att0, MHsHOL_thy)
    ShAAppl "MHsHOLCF_thy" [] _ -> (att0, MHsHOLCF_thy)
    ShAAppl "CspHOLComplex_thy" [] _ -> (att0, CspHOLComplex_thy)
    u -> fromShATermError "BaseSig" u

instance ShATermConvertible Sign where
  toShATermAux att0 xv = case xv of
    Sign a b c d e f g -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      (att5, e') <- toShATerm' att4 e
      (att6, f') <- toShATerm' att5 f
      (att7, g') <- toShATerm' att6 g
      return $ addATerm (ShAAppl "Sign" [a', b', c', d', e', f',
                                         g'] []) att7
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Sign" [a, b, c, d, e, f, g] _ ->
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
      case fromShATerm' g att6 of
      { (att7, g') ->
      (att7, Sign a' b' c' d' e' f' g') }}}}}}}
    u -> fromShATermError "Sign" u

instance ShATermConvertible IsaProof where
  toShATermAux att0 xv = case xv of
    IsaProof a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "IsaProof" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "IsaProof" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, IsaProof a' b') }}
    u -> fromShATermError "IsaProof" u

instance ShATermConvertible ProofCommand where
  toShATermAux att0 xv = case xv of
    Apply a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Apply" [a', b'] []) att2
    Using a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Using" [a'] []) att1
    Back -> return $ addATerm (ShAAppl "Back" [] []) att0
    Defer a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Defer" [a'] []) att1
    Prefer a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Prefer" [a'] []) att1
    Refute -> return $ addATerm (ShAAppl "Refute" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Apply" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Apply a' b') }}
    ShAAppl "Using" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Using a') }
    ShAAppl "Back" [] _ -> (att0, Back)
    ShAAppl "Defer" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Defer a') }
    ShAAppl "Prefer" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Prefer a') }
    ShAAppl "Refute" [] _ -> (att0, Refute)
    u -> fromShATermError "ProofCommand" u

instance ShATermConvertible ProofEnd where
  toShATermAux att0 xv = case xv of
    By a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "By" [a'] []) att1
    DotDot -> return $ addATerm (ShAAppl "DotDot" [] []) att0
    Done -> return $ addATerm (ShAAppl "Done" [] []) att0
    Oops -> return $ addATerm (ShAAppl "Oops" [] []) att0
    Sorry -> return $ addATerm (ShAAppl "Sorry" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "By" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, By a') }
    ShAAppl "DotDot" [] _ -> (att0, DotDot)
    ShAAppl "Done" [] _ -> (att0, Done)
    ShAAppl "Oops" [] _ -> (att0, Oops)
    ShAAppl "Sorry" [] _ -> (att0, Sorry)
    u -> fromShATermError "ProofEnd" u

instance ShATermConvertible Modifier where
  toShATermAux att0 xv = case xv of
    No_asm -> return $ addATerm (ShAAppl "No_asm" [] []) att0
    No_asm_simp -> return $ addATerm (ShAAppl "No_asm_simp" [] []) att0
    No_asm_use -> return $ addATerm (ShAAppl "No_asm_use" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "No_asm" [] _ -> (att0, No_asm)
    ShAAppl "No_asm_simp" [] _ -> (att0, No_asm_simp)
    ShAAppl "No_asm_use" [] _ -> (att0, No_asm_use)
    u -> fromShATermError "Modifier" u

instance ShATermConvertible ProofMethod where
  toShATermAux att0 xv = case xv of
    Auto -> return $ addATerm (ShAAppl "Auto" [] []) att0
    Simp -> return $ addATerm (ShAAppl "Simp" [] []) att0
    AutoSimpAdd a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "AutoSimpAdd" [a', b'] []) att2
    SimpAdd a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "SimpAdd" [a', b'] []) att2
    Induct a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Induct" [a'] []) att1
    CaseTac a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "CaseTac" [a'] []) att1
    SubgoalTac a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "SubgoalTac" [a'] []) att1
    Insert a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Insert" [a'] []) att1
    Other a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Other" [a'] []) att1
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Auto" [] _ -> (att0, Auto)
    ShAAppl "Simp" [] _ -> (att0, Simp)
    ShAAppl "AutoSimpAdd" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, AutoSimpAdd a' b') }}
    ShAAppl "SimpAdd" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, SimpAdd a' b') }}
    ShAAppl "Induct" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Induct a') }
    ShAAppl "CaseTac" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, CaseTac a') }
    ShAAppl "SubgoalTac" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, SubgoalTac a') }
    ShAAppl "Insert" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Insert a') }
    ShAAppl "Other" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Other a') }
    u -> fromShATermError "ProofMethod" u
