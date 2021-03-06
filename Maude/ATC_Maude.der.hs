{-# OPTIONS -w -O0 #-}
{- |
Module      :  Maude/ATC_Maude.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'Maude.Sign.Sign'
'Maude.Morphism.Morphism'
'Maude.Sentence.Sentence'
'Maude.Symbol.Symbol'
'Maude.AS_Maude.MaudeText'
'Maude.AS_Maude.Spec'
'Maude.AS_Maude.Module'
'Maude.AS_Maude.View'
'Maude.AS_Maude.Parameter'
'Maude.AS_Maude.ModExp'
'Maude.AS_Maude.Renaming'
'Maude.AS_Maude.ToPartRenaming'
'Maude.AS_Maude.Statement'
'Maude.AS_Maude.Import'
'Maude.AS_Maude.SubsortDecl'
'Maude.AS_Maude.Operator'
'Maude.AS_Maude.Membership'
'Maude.AS_Maude.Equation'
'Maude.AS_Maude.Rule'
'Maude.AS_Maude.Condition'
'Maude.AS_Maude.Attr'
'Maude.AS_Maude.StmntAttr'
'Maude.AS_Maude.Hook'
'Maude.AS_Maude.Term'
'Maude.AS_Maude.Type'
'Maude.AS_Maude.Sort'
'Maude.AS_Maude.Kind'
'Maude.AS_Maude.ParamId'
'Maude.AS_Maude.ViewId'
'Maude.AS_Maude.ModId'
'Maude.AS_Maude.LabelId'
'Maude.AS_Maude.OpId'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Maude/Sign.hs
Maude/Morphism.hs
Maude/Sentence.hs
Maude/Symbol.hs
Maude/AS_Maude.hs
-}

module Maude.ATC_Maude () where

import ATC.AS_Annotation
import ATerm.Lib
import Common.Doc
import Common.Doc (specBraces, text)
import Common.Doc (vcat)
import Common.Doc hiding (empty)
import Common.DocUtils (Pretty (..))
import Common.DocUtils (Pretty(..))
import Common.Id (Id, mkId, mkSimpleId, GetRange, getRange, nullRange)
import Common.Id (mkSimpleId, GetRange)
import Common.Id hiding (Id)
import Common.Lib.Rel (Rel)
import Common.Result (Result)
import Data.List (partition)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Typeable
import Maude.AS_Maude
import Maude.Meta
import Maude.Meta.HasName
import Maude.Morphism
import Maude.Printing ()
import Maude.Sentence
import Maude.Sentence (Sentence)
import Maude.Sign
import Maude.Sign (Sign, kindRel, KindRel)
import Maude.Symbol
import Maude.Util
import qualified Common.Doc as Doc
import qualified Common.Lib.Rel as Rel
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Maude.Sentence as Sen
import qualified Maude.Sign as Sign

{-! for Maude.Sign.Sign derive : Typeable !-}
{-! for Maude.Morphism.Morphism derive : Typeable !-}
{-! for Maude.Sentence.Sentence derive : Typeable !-}
{-! for Maude.Symbol.Symbol derive : Typeable !-}
{-! for Maude.AS_Maude.MaudeText derive : Typeable !-}
{-! for Maude.AS_Maude.Spec derive : Typeable !-}
{-! for Maude.AS_Maude.Module derive : Typeable !-}
{-! for Maude.AS_Maude.View derive : Typeable !-}
{-! for Maude.AS_Maude.Parameter derive : Typeable !-}
{-! for Maude.AS_Maude.ModExp derive : Typeable !-}
{-! for Maude.AS_Maude.Renaming derive : Typeable !-}
{-! for Maude.AS_Maude.ToPartRenaming derive : Typeable !-}
{-! for Maude.AS_Maude.Statement derive : Typeable !-}
{-! for Maude.AS_Maude.Import derive : Typeable !-}
{-! for Maude.AS_Maude.SubsortDecl derive : Typeable !-}
{-! for Maude.AS_Maude.Operator derive : Typeable !-}
{-! for Maude.AS_Maude.Membership derive : Typeable !-}
{-! for Maude.AS_Maude.Equation derive : Typeable !-}
{-! for Maude.AS_Maude.Rule derive : Typeable !-}
{-! for Maude.AS_Maude.Condition derive : Typeable !-}
{-! for Maude.AS_Maude.Attr derive : Typeable !-}
{-! for Maude.AS_Maude.StmntAttr derive : Typeable !-}
{-! for Maude.AS_Maude.Hook derive : Typeable !-}
{-! for Maude.AS_Maude.Term derive : Typeable !-}
{-! for Maude.AS_Maude.Type derive : Typeable !-}
{-! for Maude.AS_Maude.Sort derive : Typeable !-}
{-! for Maude.AS_Maude.Kind derive : Typeable !-}
{-! for Maude.AS_Maude.ParamId derive : Typeable !-}
{-! for Maude.AS_Maude.ViewId derive : Typeable !-}
{-! for Maude.AS_Maude.ModId derive : Typeable !-}
{-! for Maude.AS_Maude.LabelId derive : Typeable !-}
{-! for Maude.AS_Maude.OpId derive : Typeable !-}

{-! for Maude.Sign.Sign derive : ShATermConvertible !-}
{-! for Maude.Morphism.Morphism derive : ShATermConvertible !-}
{-! for Maude.Sentence.Sentence derive : ShATermConvertible !-}
{-! for Maude.Symbol.Symbol derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.MaudeText derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Spec derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Module derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.View derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Parameter derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.ModExp derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Renaming derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.ToPartRenaming derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Statement derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Import derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.SubsortDecl derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Operator derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Membership derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Equation derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Rule derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Condition derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Attr derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.StmntAttr derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Hook derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Term derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Type derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Sort derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.Kind derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.ParamId derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.ViewId derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.ModId derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.LabelId derive : ShATermConvertible !-}
{-! for Maude.AS_Maude.OpId derive : ShATermConvertible !-}
