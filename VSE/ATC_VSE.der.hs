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
