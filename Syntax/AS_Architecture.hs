{- |
Module      :  $Header$
Description :  abstract syntax of CASL architectural specifications
Copyright   :  (c) Klaus Luettich, Uni Bremen 2002-2006
License     :  GPLv2 or higher, see LICENSE.txt
Maintainer  :  till@informatik.uni-bremen.de
Stability   :  provisional
Portability :  non-portable(imports Syntax.AS_Structured)

Abstract syntax of (Het)CASL architectural specifications
   Follows Sect. II:2.2.4 of the CASL Reference Manual.
-}

module Syntax.AS_Architecture where

-- DrIFT command:
{-! global: GetRange !-}

import Common.Id
import Common.IRI
import Common.AS_Annotation

import Syntax.AS_Structured

-- for arch-spec-defn and unit-spec-defn see AS_Library

data ARCH_SPEC = Basic_arch_spec [Annoted UNIT_DECL_DEFN]
                                 (Annoted UNIT_EXPRESSION) Range
                 -- pos: "unit","result"
               | Arch_spec_name ARCH_SPEC_NAME
               | Group_arch_spec (Annoted ARCH_SPEC) Range
                 -- pos: "{","}"
                 deriving (Show)


data UNIT_DECL_DEFN = Unit_decl UNIT_NAME REF_SPEC [Annoted UNIT_TERM] Range
                      -- pos: ":", opt ("given"; Annoted holds pos of commas)
                    | Unit_defn UNIT_NAME UNIT_EXPRESSION Range
                      -- pos: "="
                      deriving (Show)

data UNIT_SPEC = Unit_type [Annoted SPEC] (Annoted SPEC) Range
                 -- pos: opt "*"s , "->"
               | Spec_name SPEC_NAME
               | Closed_unit_spec UNIT_SPEC Range
                 -- pos: "closed"
                 deriving (Show)

data REF_SPEC = Unit_spec UNIT_SPEC
              | Refinement Bool UNIT_SPEC [G_mapping] REF_SPEC Range
                -- false means "behaviourally"
              | Arch_unit_spec (Annoted ARCH_SPEC) Range
                 -- pos: "arch","spec"
                 -- The ARCH_SPEC has to be surrounded with braces and
                 -- after the opening brace is a [Annotation] allowed
              | Compose_ref [REF_SPEC] Range
                 -- pos: "then"
              | Component_ref [UNIT_REF] Range
                -- pos "{", commas and "}"
                 deriving (Show)

data UNIT_REF = Unit_ref UNIT_NAME REF_SPEC Range
                 -- pos: ":"
                 deriving (Show)

data UNIT_EXPRESSION = Unit_expression [UNIT_BINDING] (Annoted UNIT_TERM) Range
                       -- pos: opt "lambda",semi colons, "."
                       deriving (Show)

data UNIT_BINDING = Unit_binding UNIT_NAME UNIT_SPEC Range
                    -- pos: ":"
                    deriving (Show)

data UNIT_TERM = Unit_reduction (Annoted UNIT_TERM) RESTRICTION
               | Unit_translation (Annoted UNIT_TERM) RENAMING
               | Amalgamation [Annoted UNIT_TERM] Range
                 -- pos: "and"s
               | Local_unit [Annoted UNIT_DECL_DEFN] (Annoted UNIT_TERM) Range
                 -- pos: "local", "within"
               | Unit_appl UNIT_NAME [FIT_ARG_UNIT] Range
                 -- pos: many of "[","]"
               | Group_unit_term (Annoted UNIT_TERM) Range
                 -- pos: "{","}"
                 deriving (Show)

data FIT_ARG_UNIT = Fit_arg_unit (Annoted UNIT_TERM) [G_mapping] Range
                    -- pos: opt "fit"
                    deriving (Show)

type ARCH_SPEC_NAME = IRI
type UNIT_NAME = IRI

-- Generated by DrIFT, look but don't touch!

instance GetRange ARCH_SPEC where
  getRange x = case x of
    Basic_arch_spec _ _ p -> p
    Arch_spec_name _ -> nullRange
    Group_arch_spec _ p -> p
  rangeSpan x = case x of
    Basic_arch_spec a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                         rangeSpan c]
    Arch_spec_name a -> joinRanges [rangeSpan a]
    Group_arch_spec a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange UNIT_DECL_DEFN where
  getRange x = case x of
    Unit_decl _ _ _ p -> p
    Unit_defn _ _ p -> p
  rangeSpan x = case x of
    Unit_decl a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                     rangeSpan c, rangeSpan d]
    Unit_defn a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                   rangeSpan c]

instance GetRange UNIT_SPEC where
  getRange x = case x of
    Unit_type _ _ p -> p
    Spec_name _ -> nullRange
    Closed_unit_spec _ p -> p
  rangeSpan x = case x of
    Unit_type a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                   rangeSpan c]
    Spec_name a -> joinRanges [rangeSpan a]
    Closed_unit_spec a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange REF_SPEC where
  getRange x = case x of
    Unit_spec _ -> nullRange
    Refinement _ _ _ _ p -> p
    Arch_unit_spec _ p -> p
    Compose_ref _ p -> p
    Component_ref _ p -> p
  rangeSpan x = case x of
    Unit_spec a -> joinRanges [rangeSpan a]
    Refinement a b c d e -> joinRanges [rangeSpan a, rangeSpan b,
                                        rangeSpan c, rangeSpan d, rangeSpan e]
    Arch_unit_spec a b -> joinRanges [rangeSpan a, rangeSpan b]
    Compose_ref a b -> joinRanges [rangeSpan a, rangeSpan b]
    Component_ref a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange UNIT_REF where
  getRange x = case x of
    Unit_ref _ _ p -> p
  rangeSpan x = case x of
    Unit_ref a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                  rangeSpan c]

instance GetRange UNIT_EXPRESSION where
  getRange x = case x of
    Unit_expression _ _ p -> p
  rangeSpan x = case x of
    Unit_expression a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                         rangeSpan c]

instance GetRange UNIT_BINDING where
  getRange x = case x of
    Unit_binding _ _ p -> p
  rangeSpan x = case x of
    Unit_binding a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                      rangeSpan c]

instance GetRange UNIT_TERM where
  getRange x = case x of
    Unit_reduction _ _ -> nullRange
    Unit_translation _ _ -> nullRange
    Amalgamation _ p -> p
    Local_unit _ _ p -> p
    Unit_appl _ _ p -> p
    Group_unit_term _ p -> p
  rangeSpan x = case x of
    Unit_reduction a b -> joinRanges [rangeSpan a, rangeSpan b]
    Unit_translation a b -> joinRanges [rangeSpan a, rangeSpan b]
    Amalgamation a b -> joinRanges [rangeSpan a, rangeSpan b]
    Local_unit a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                    rangeSpan c]
    Unit_appl a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                   rangeSpan c]
    Group_unit_term a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange FIT_ARG_UNIT where
  getRange x = case x of
    Fit_arg_unit _ _ p -> p
  rangeSpan x = case x of
    Fit_arg_unit a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                      rangeSpan c]