{-# OPTIONS -w -O0 #-}
{- |
Module      :  VSE/ATC_VSE.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'VSE.As.Paramkind'
'VSE.As.Procparam'
'VSE.As.Profile'
'VSE.As.Sigentry'
'VSE.As.Procdecls'
'VSE.As.Ranged'
'VSE.As.PlainProgram'
'VSE.As.VarDecl'
'VSE.As.VSEforms'
'VSE.As.BoxOrDiamond'
'VSE.As.ProcKind'
'VSE.As.Defproc'
'VSE.As.Procs'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
VSE/As.hs
-}

module VSE.ATC_VSE () where

import ATerm.Lib
import CASL.AS_Basic_CASL
import CASL.ATC_CASL
import CASL.ToDoc
import Common.AS_Annotation
import Common.Doc
import Common.DocUtils
import Common.Id
import Common.LibName
import Common.Result
import Common.Utils (number)
import Control.Monad (foldM)
import Data.Char
import Data.Typeable
import VSE.As
import qualified Data.Map as Map

{-! for VSE.As.Paramkind derive : Typeable !-}
{-! for VSE.As.Procparam derive : Typeable !-}
{-! for VSE.As.Profile derive : Typeable !-}
{-! for VSE.As.Sigentry derive : Typeable !-}
{-! for VSE.As.Procdecls derive : Typeable !-}
{-! for VSE.As.Ranged derive : Typeable !-}
{-! for VSE.As.PlainProgram derive : Typeable !-}
{-! for VSE.As.VarDecl derive : Typeable !-}
{-! for VSE.As.VSEforms derive : Typeable !-}
{-! for VSE.As.BoxOrDiamond derive : Typeable !-}
{-! for VSE.As.ProcKind derive : Typeable !-}
{-! for VSE.As.Defproc derive : Typeable !-}
{-! for VSE.As.Procs derive : Typeable !-}

{-! for VSE.As.Paramkind derive : ShATermConvertible !-}
{-! for VSE.As.Procparam derive : ShATermConvertible !-}
{-! for VSE.As.Profile derive : ShATermConvertible !-}
{-! for VSE.As.Sigentry derive : ShATermConvertible !-}
{-! for VSE.As.Procdecls derive : ShATermConvertible !-}
{-! for VSE.As.Ranged derive : ShATermConvertible !-}
{-! for VSE.As.PlainProgram derive : ShATermConvertible !-}
{-! for VSE.As.VarDecl derive : ShATermConvertible !-}
{-! for VSE.As.VSEforms derive : ShATermConvertible !-}
{-! for VSE.As.BoxOrDiamond derive : ShATermConvertible !-}
{-! for VSE.As.ProcKind derive : ShATermConvertible !-}
{-! for VSE.As.Defproc derive : ShATermConvertible !-}
{-! for VSE.As.Procs derive : ShATermConvertible !-}

-- Generated by DrIFT, look but don't touch!

_tcParamkindTc :: TyCon
_tcParamkindTc = mkTyCon "VSE.As.Paramkind"
instance Typeable Paramkind where
    typeOf _ = mkTyConApp _tcParamkindTc []

_tcProcparamTc :: TyCon
_tcProcparamTc = mkTyCon "VSE.As.Procparam"
instance Typeable Procparam where
    typeOf _ = mkTyConApp _tcProcparamTc []

_tcProfileTc :: TyCon
_tcProfileTc = mkTyCon "VSE.As.Profile"
instance Typeable Profile where
    typeOf _ = mkTyConApp _tcProfileTc []

_tcSigentryTc :: TyCon
_tcSigentryTc = mkTyCon "VSE.As.Sigentry"
instance Typeable Sigentry where
    typeOf _ = mkTyConApp _tcSigentryTc []

_tcProcdeclsTc :: TyCon
_tcProcdeclsTc = mkTyCon "VSE.As.Procdecls"
instance Typeable Procdecls where
    typeOf _ = mkTyConApp _tcProcdeclsTc []

_tcRangedTc :: TyCon
_tcRangedTc = mkTyCon "VSE.As.Ranged"
instance Typeable1 Ranged where
    typeOf1 _ = mkTyConApp _tcRangedTc []

_tcPlainProgramTc :: TyCon
_tcPlainProgramTc = mkTyCon "VSE.As.PlainProgram"
instance Typeable PlainProgram where
    typeOf _ = mkTyConApp _tcPlainProgramTc []

_tcVarDeclTc :: TyCon
_tcVarDeclTc = mkTyCon "VSE.As.VarDecl"
instance Typeable VarDecl where
    typeOf _ = mkTyConApp _tcVarDeclTc []

_tcVSEformsTc :: TyCon
_tcVSEformsTc = mkTyCon "VSE.As.VSEforms"
instance Typeable VSEforms where
    typeOf _ = mkTyConApp _tcVSEformsTc []

_tcBoxOrDiamondTc :: TyCon
_tcBoxOrDiamondTc = mkTyCon "VSE.As.BoxOrDiamond"
instance Typeable BoxOrDiamond where
    typeOf _ = mkTyConApp _tcBoxOrDiamondTc []

_tcProcKindTc :: TyCon
_tcProcKindTc = mkTyCon "VSE.As.ProcKind"
instance Typeable ProcKind where
    typeOf _ = mkTyConApp _tcProcKindTc []

_tcDefprocTc :: TyCon
_tcDefprocTc = mkTyCon "VSE.As.Defproc"
instance Typeable Defproc where
    typeOf _ = mkTyConApp _tcDefprocTc []

_tcProcsTc :: TyCon
_tcProcsTc = mkTyCon "VSE.As.Procs"
instance Typeable Procs where
    typeOf _ = mkTyConApp _tcProcsTc []

instance ShATermConvertible Paramkind where
  toShATermAux att0 xv = case xv of
    In -> return $ addATerm (ShAAppl "In" [] []) att0
    Out -> return $ addATerm (ShAAppl "Out" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "In" [] _ -> (att0, In)
    ShAAppl "Out" [] _ -> (att0, Out)
    u -> fromShATermError "Paramkind" u

instance ShATermConvertible Procparam where
  toShATermAux att0 xv = case xv of
    Procparam a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Procparam" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Procparam" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Procparam a' b') }}
    u -> fromShATermError "Procparam" u

instance ShATermConvertible Profile where
  toShATermAux att0 xv = case xv of
    Profile a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Profile" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Profile" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Profile a' b') }}
    u -> fromShATermError "Profile" u

instance ShATermConvertible Sigentry where
  toShATermAux att0 xv = case xv of
    Procedure a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Procedure" [a', b', c'] []) att3
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Procedure" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Procedure a' b' c') }}}
    u -> fromShATermError "Sigentry" u

instance ShATermConvertible Procdecls where
  toShATermAux att0 xv = case xv of
    Procdecls a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Procdecls" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Procdecls" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Procdecls a' b') }}
    u -> fromShATermError "Procdecls" u

instance ShATermConvertible a => ShATermConvertible (Ranged a) where
  toShATermAux att0 xv = case xv of
    Ranged a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Ranged" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Ranged" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Ranged a' b') }}
    u -> fromShATermError "Ranged" u

instance ShATermConvertible PlainProgram where
  toShATermAux att0 xv = case xv of
    Abort -> return $ addATerm (ShAAppl "Abort" [] []) att0
    Skip -> return $ addATerm (ShAAppl "Skip" [] []) att0
    Assign a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Assign" [a', b'] []) att2
    Call a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Call" [a'] []) att1
    Return a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Return" [a'] []) att1
    Block a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Block" [a', b'] []) att2
    Seq a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "Seq" [a', b'] []) att2
    If a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "If" [a', b', c'] []) att3
    While a b -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      return $ addATerm (ShAAppl "While" [a', b'] []) att2
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Abort" [] _ -> (att0, Abort)
    ShAAppl "Skip" [] _ -> (att0, Skip)
    ShAAppl "Assign" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Assign a' b') }}
    ShAAppl "Call" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Call a') }
    ShAAppl "Return" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Return a') }
    ShAAppl "Block" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Block a' b') }}
    ShAAppl "Seq" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, Seq a' b') }}
    ShAAppl "If" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, If a' b' c') }}}
    ShAAppl "While" [a, b] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      (att2, While a' b') }}
    u -> fromShATermError "PlainProgram" u

instance ShATermConvertible VarDecl where
  toShATermAux att0 xv = case xv of
    VarDecl a b c d -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      return $ addATerm (ShAAppl "VarDecl" [a', b', c', d'] []) att4
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "VarDecl" [a, b, c, d] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      case fromShATerm' d att3 of
      { (att4, d') ->
      (att4, VarDecl a' b' c' d') }}}}
    u -> fromShATermError "VarDecl" u

instance ShATermConvertible VSEforms where
  toShATermAux att0 xv = case xv of
    Dlformula a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "Dlformula" [a', b', c'] []) att3
    Defprocs a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Defprocs" [a'] []) att1
    RestrictedConstraint a b c -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      return $ addATerm (ShAAppl "RestrictedConstraint" [a', b',
                                                         c'] []) att3
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Dlformula" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, Dlformula a' b' c') }}}
    ShAAppl "Defprocs" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Defprocs a') }
    ShAAppl "RestrictedConstraint" [a, b, c] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      case fromShATerm' b att1 of
      { (att2, b') ->
      case fromShATerm' c att2 of
      { (att3, c') ->
      (att3, RestrictedConstraint a' b' c') }}}
    u -> fromShATermError "VSEforms" u

instance ShATermConvertible BoxOrDiamond where
  toShATermAux att0 xv = case xv of
    Box -> return $ addATerm (ShAAppl "Box" [] []) att0
    Diamond -> return $ addATerm (ShAAppl "Diamond" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Box" [] _ -> (att0, Box)
    ShAAppl "Diamond" [] _ -> (att0, Diamond)
    u -> fromShATermError "BoxOrDiamond" u

instance ShATermConvertible ProcKind where
  toShATermAux att0 xv = case xv of
    Proc -> return $ addATerm (ShAAppl "Proc" [] []) att0
    Func -> return $ addATerm (ShAAppl "Func" [] []) att0
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Proc" [] _ -> (att0, Proc)
    ShAAppl "Func" [] _ -> (att0, Func)
    u -> fromShATermError "ProcKind" u

instance ShATermConvertible Defproc where
  toShATermAux att0 xv = case xv of
    Defproc a b c d e -> do
      (att1, a') <- toShATerm' att0 a
      (att2, b') <- toShATerm' att1 b
      (att3, c') <- toShATerm' att2 c
      (att4, d') <- toShATerm' att3 d
      (att5, e') <- toShATerm' att4 e
      return $ addATerm (ShAAppl "Defproc" [a', b', c', d', e'] []) att5
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Defproc" [a, b, c, d, e] _ ->
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
      (att5, Defproc a' b' c' d' e') }}}}}
    u -> fromShATermError "Defproc" u

instance ShATermConvertible Procs where
  toShATermAux att0 xv = case xv of
    Procs a -> do
      (att1, a') <- toShATerm' att0 a
      return $ addATerm (ShAAppl "Procs" [a'] []) att1
  fromShATermAux ix att0 = case getShATerm ix att0 of
    ShAAppl "Procs" [a] _ ->
      case fromShATerm' a att0 of
      { (att1, a') ->
      (att1, Procs a') }
    u -> fromShATermError "Procs" u
