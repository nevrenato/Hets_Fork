{-# OPTIONS -w -O0 #-}
{- |
Module      :  HasCASL/ATC_HasCASL.der.hs
Description :  generated Typeable, ShATermConvertible instances
Copyright   :  (c) DFKI Bremen 2008
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  non-portable(overlapping Typeable instances)

Automatic derivation of instances via DrIFT-rule Typeable, ShATermConvertible
  for the type(s):
'Common.Prec.PrecMap'
'HasCASL.As.BasicSpec'
'HasCASL.As.BasicItem'
'HasCASL.As.SigItems'
'HasCASL.As.OpBrand'
'HasCASL.As.Instance'
'HasCASL.As.ClassItem'
'HasCASL.As.ClassDecl'
'HasCASL.As.Variance'
'HasCASL.As.AnyKind'
'HasCASL.As.TypeItem'
'HasCASL.As.Vars'
'HasCASL.As.TypePattern'
'HasCASL.As.Type'
'HasCASL.As.TypeScheme'
'HasCASL.As.Partiality'
'HasCASL.As.OpItem'
'HasCASL.As.BinOpAttr'
'HasCASL.As.OpAttr'
'HasCASL.As.DatatypeDecl'
'HasCASL.As.Alternative'
'HasCASL.As.Component'
'HasCASL.As.Quantifier'
'HasCASL.As.TypeQual'
'HasCASL.As.LetBrand'
'HasCASL.As.BracketKind'
'HasCASL.As.InstKind'
'HasCASL.As.Term'
'HasCASL.As.ProgEq'
'HasCASL.As.PolyId'
'HasCASL.As.SeparatorKind'
'HasCASL.As.VarDecl'
'HasCASL.As.VarKind'
'HasCASL.As.TypeArg'
'HasCASL.As.GenVarDecl'
'HasCASL.As.SymbItems'
'HasCASL.As.SymbMapItems'
'HasCASL.As.SymbKind'
'HasCASL.As.Symb'
'HasCASL.As.SymbType'
'HasCASL.As.SymbOrMap'
'HasCASL.Le.ClassInfo'
'HasCASL.Le.GenKind'
'HasCASL.Le.AltDefn'
'HasCASL.Le.Selector'
'HasCASL.Le.DataEntry'
'HasCASL.Le.TypeDefn'
'HasCASL.Le.TypeInfo'
'HasCASL.Le.Sentence'
'HasCASL.Le.TypeVarDefn'
'HasCASL.Le.VarDefn'
'HasCASL.Le.ConstrInfo'
'HasCASL.Le.OpDefn'
'HasCASL.Le.OpInfo'
'HasCASL.Le.Env'
'HasCASL.Le.Constrain'
'HasCASL.Le.Morphism'
'HasCASL.Le.SymbolType'
'HasCASL.Le.Symbol'
'HasCASL.Le.RawSymbol'
'HasCASL.Sublogic.Formulas'
'HasCASL.Sublogic.Classes'
'HasCASL.Sublogic.Sublogic'
-}

{-
  Generated by 'genRules' (automatic rule generation for DrIFT). Don't touch!!
  dependency files:
Common/Prec.hs
HasCASL/As.hs
HasCASL/Le.hs
HasCASL/Sublogic.hs
-}

module HasCASL.ATC_HasCASL () where

import ATC.GlobalAnnotations
import ATerm.Lib
import Common.AS_Annotation
import Common.AS_Annotation (Named)
import Common.GlobalAnnotations
import Common.Id
import Common.Keywords
import Common.Prec
import Common.Result
import Data.List (partition)
import Data.Maybe
import Data.Ord
import Data.Typeable
import HasCASL.As
import HasCASL.AsUtils
import HasCASL.Builtin
import HasCASL.FoldTerm
import HasCASL.FoldType
import HasCASL.Le
import HasCASL.Sublogic
import qualified Common.Lib.Rel as Rel
import qualified Common.Lib.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set

{-! for Common.Prec.PrecMap derive : Typeable !-}
{-! for HasCASL.As.BasicSpec derive : Typeable !-}
{-! for HasCASL.As.BasicItem derive : Typeable !-}
{-! for HasCASL.As.SigItems derive : Typeable !-}
{-! for HasCASL.As.OpBrand derive : Typeable !-}
{-! for HasCASL.As.Instance derive : Typeable !-}
{-! for HasCASL.As.ClassItem derive : Typeable !-}
{-! for HasCASL.As.ClassDecl derive : Typeable !-}
{-! for HasCASL.As.Variance derive : Typeable !-}
{-! for HasCASL.As.AnyKind derive : Typeable !-}
{-! for HasCASL.As.TypeItem derive : Typeable !-}
{-! for HasCASL.As.Vars derive : Typeable !-}
{-! for HasCASL.As.TypePattern derive : Typeable !-}
{-! for HasCASL.As.Type derive : Typeable !-}
{-! for HasCASL.As.TypeScheme derive : Typeable !-}
{-! for HasCASL.As.Partiality derive : Typeable !-}
{-! for HasCASL.As.OpItem derive : Typeable !-}
{-! for HasCASL.As.BinOpAttr derive : Typeable !-}
{-! for HasCASL.As.OpAttr derive : Typeable !-}
{-! for HasCASL.As.DatatypeDecl derive : Typeable !-}
{-! for HasCASL.As.Alternative derive : Typeable !-}
{-! for HasCASL.As.Component derive : Typeable !-}
{-! for HasCASL.As.Quantifier derive : Typeable !-}
{-! for HasCASL.As.TypeQual derive : Typeable !-}
{-! for HasCASL.As.LetBrand derive : Typeable !-}
{-! for HasCASL.As.BracketKind derive : Typeable !-}
{-! for HasCASL.As.InstKind derive : Typeable !-}
{-! for HasCASL.As.Term derive : Typeable !-}
{-! for HasCASL.As.ProgEq derive : Typeable !-}
{-! for HasCASL.As.PolyId derive : Typeable !-}
{-! for HasCASL.As.SeparatorKind derive : Typeable !-}
{-! for HasCASL.As.VarDecl derive : Typeable !-}
{-! for HasCASL.As.VarKind derive : Typeable !-}
{-! for HasCASL.As.TypeArg derive : Typeable !-}
{-! for HasCASL.As.GenVarDecl derive : Typeable !-}
{-! for HasCASL.As.SymbItems derive : Typeable !-}
{-! for HasCASL.As.SymbMapItems derive : Typeable !-}
{-! for HasCASL.As.SymbKind derive : Typeable !-}
{-! for HasCASL.As.Symb derive : Typeable !-}
{-! for HasCASL.As.SymbType derive : Typeable !-}
{-! for HasCASL.As.SymbOrMap derive : Typeable !-}
{-! for HasCASL.Le.ClassInfo derive : Typeable !-}
{-! for HasCASL.Le.GenKind derive : Typeable !-}
{-! for HasCASL.Le.AltDefn derive : Typeable !-}
{-! for HasCASL.Le.Selector derive : Typeable !-}
{-! for HasCASL.Le.DataEntry derive : Typeable !-}
{-! for HasCASL.Le.TypeDefn derive : Typeable !-}
{-! for HasCASL.Le.TypeInfo derive : Typeable !-}
{-! for HasCASL.Le.Sentence derive : Typeable !-}
{-! for HasCASL.Le.TypeVarDefn derive : Typeable !-}
{-! for HasCASL.Le.VarDefn derive : Typeable !-}
{-! for HasCASL.Le.ConstrInfo derive : Typeable !-}
{-! for HasCASL.Le.OpDefn derive : Typeable !-}
{-! for HasCASL.Le.OpInfo derive : Typeable !-}
{-! for HasCASL.Le.Env derive : Typeable !-}
{-! for HasCASL.Le.Constrain derive : Typeable !-}
{-! for HasCASL.Le.Morphism derive : Typeable !-}
{-! for HasCASL.Le.SymbolType derive : Typeable !-}
{-! for HasCASL.Le.Symbol derive : Typeable !-}
{-! for HasCASL.Le.RawSymbol derive : Typeable !-}
{-! for HasCASL.Sublogic.Formulas derive : Typeable !-}
{-! for HasCASL.Sublogic.Classes derive : Typeable !-}
{-! for HasCASL.Sublogic.Sublogic derive : Typeable !-}

{-! for Common.Prec.PrecMap derive : ShATermConvertible !-}
{-! for HasCASL.As.BasicSpec derive : ShATermConvertible !-}
{-! for HasCASL.As.BasicItem derive : ShATermConvertible !-}
{-! for HasCASL.As.SigItems derive : ShATermConvertible !-}
{-! for HasCASL.As.OpBrand derive : ShATermConvertible !-}
{-! for HasCASL.As.Instance derive : ShATermConvertible !-}
{-! for HasCASL.As.ClassItem derive : ShATermConvertible !-}
{-! for HasCASL.As.ClassDecl derive : ShATermConvertible !-}
{-! for HasCASL.As.Variance derive : ShATermConvertible !-}
{-! for HasCASL.As.AnyKind derive : ShATermConvertible !-}
{-! for HasCASL.As.TypeItem derive : ShATermConvertible !-}
{-! for HasCASL.As.Vars derive : ShATermConvertible !-}
{-! for HasCASL.As.TypePattern derive : ShATermConvertible !-}
{-! for HasCASL.As.Type derive : ShATermConvertible !-}
{-! for HasCASL.As.TypeScheme derive : ShATermConvertible !-}
{-! for HasCASL.As.Partiality derive : ShATermConvertible !-}
{-! for HasCASL.As.OpItem derive : ShATermConvertible !-}
{-! for HasCASL.As.BinOpAttr derive : ShATermConvertible !-}
{-! for HasCASL.As.OpAttr derive : ShATermConvertible !-}
{-! for HasCASL.As.DatatypeDecl derive : ShATermConvertible !-}
{-! for HasCASL.As.Alternative derive : ShATermConvertible !-}
{-! for HasCASL.As.Component derive : ShATermConvertible !-}
{-! for HasCASL.As.Quantifier derive : ShATermConvertible !-}
{-! for HasCASL.As.TypeQual derive : ShATermConvertible !-}
{-! for HasCASL.As.LetBrand derive : ShATermConvertible !-}
{-! for HasCASL.As.BracketKind derive : ShATermConvertible !-}
{-! for HasCASL.As.InstKind derive : ShATermConvertible !-}
{-! for HasCASL.As.Term derive : ShATermConvertible !-}
{-! for HasCASL.As.ProgEq derive : ShATermConvertible !-}
{-! for HasCASL.As.PolyId derive : ShATermConvertible !-}
{-! for HasCASL.As.SeparatorKind derive : ShATermConvertible !-}
{-! for HasCASL.As.VarDecl derive : ShATermConvertible !-}
{-! for HasCASL.As.VarKind derive : ShATermConvertible !-}
{-! for HasCASL.As.TypeArg derive : ShATermConvertible !-}
{-! for HasCASL.As.GenVarDecl derive : ShATermConvertible !-}
{-! for HasCASL.As.SymbItems derive : ShATermConvertible !-}
{-! for HasCASL.As.SymbMapItems derive : ShATermConvertible !-}
{-! for HasCASL.As.SymbKind derive : ShATermConvertible !-}
{-! for HasCASL.As.Symb derive : ShATermConvertible !-}
{-! for HasCASL.As.SymbType derive : ShATermConvertible !-}
{-! for HasCASL.As.SymbOrMap derive : ShATermConvertible !-}
{-! for HasCASL.Le.ClassInfo derive : ShATermConvertible !-}
{-! for HasCASL.Le.GenKind derive : ShATermConvertible !-}
{-! for HasCASL.Le.AltDefn derive : ShATermConvertible !-}
{-! for HasCASL.Le.Selector derive : ShATermConvertible !-}
{-! for HasCASL.Le.DataEntry derive : ShATermConvertible !-}
{-! for HasCASL.Le.TypeDefn derive : ShATermConvertible !-}
{-! for HasCASL.Le.TypeInfo derive : ShATermConvertible !-}
{-! for HasCASL.Le.Sentence derive : ShATermConvertible !-}
{-! for HasCASL.Le.TypeVarDefn derive : ShATermConvertible !-}
{-! for HasCASL.Le.VarDefn derive : ShATermConvertible !-}
{-! for HasCASL.Le.ConstrInfo derive : ShATermConvertible !-}
{-! for HasCASL.Le.OpDefn derive : ShATermConvertible !-}
{-! for HasCASL.Le.OpInfo derive : ShATermConvertible !-}
{-! for HasCASL.Le.Env derive : ShATermConvertible !-}
{-! for HasCASL.Le.Constrain derive : ShATermConvertible !-}
{-! for HasCASL.Le.Morphism derive : ShATermConvertible !-}
{-! for HasCASL.Le.SymbolType derive : ShATermConvertible !-}
{-! for HasCASL.Le.Symbol derive : ShATermConvertible !-}
{-! for HasCASL.Le.RawSymbol derive : ShATermConvertible !-}
{-! for HasCASL.Sublogic.Formulas derive : ShATermConvertible !-}
{-! for HasCASL.Sublogic.Classes derive : ShATermConvertible !-}
{-! for HasCASL.Sublogic.Sublogic derive : ShATermConvertible !-}