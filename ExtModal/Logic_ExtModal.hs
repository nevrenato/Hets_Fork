{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{- |
Module      :  $Header$
Description :  Instance of class Logic for ExtModal
Copyright   :  DFKI GmbH 2009
License     :  GPLv2 or higher, see LICENSE.txt
Maintainer  :  codruta.liliana@gmail.com
Stability   :  experimental
Portability :  non-portable (imports Logic)

Instance of class Logic for ExtModal
-}

module ExtModal.Logic_ExtModal where

import ExtModal.AS_ExtModal
import ExtModal.ExtModalSign
import ExtModal.ATC_ExtModal ()
import ExtModal.Parse_AS
import ExtModal.StatAna
import ExtModal.MorphismExtension

import CASL.Sign
import CASL.Morphism
import CASL.SymbolMapAnalysis
import CASL.AS_Basic_CASL
import CASL.MapSentence
import CASL.Parse_AS_Basic
import CASL.SymbolParser
import CASL.SimplifySen
import CASL.Taxonomy
import CASL.Logic_CASL ()

import Logic.Logic

import Common.DocUtils

data ExtModal = ExtModal deriving Show

instance Language ExtModal where
        description _ = unlines
         [ "ExtModal is the 'extended modal logic' extension of CASL. "
         , "Syntax for ordinary modalities, multi-modal logic, dynamic "
         , "logic, graded modal logic, hybrid logic, CTL* and mu-calculus  "
         , "is provided. Specific modal logics can be optained via "
         , "restrictions to sublanguages."
         ]

type ExtModalSign = Sign EM_FORMULA EModalSign
type ExtModalMorph = Morphism EM_FORMULA EModalSign MorphExtension
type ExtModalFORMULA = FORMULA EM_FORMULA

instance SignExtension EModalSign where
        isSubSignExtension = isSubEModalSign

instance Syntax ExtModal EM_BASIC_SPEC SYMB_ITEMS SYMB_MAP_ITEMS where
        parse_basic_spec ExtModal = Just $ basicSpec ext_modal_reserved_words
        parse_symb_items ExtModal = Just $ symbItems ext_modal_reserved_words
        parse_symb_map_items ExtModal =
            Just $ symbMapItems ext_modal_reserved_words

simEMmod :: Sign EM_FORMULA EModalSign -> MODALITY -> MODALITY
simEMmod sign tm = case tm of
  ModOp o tm1 tm2 -> ModOp o (simEMmod sign tm1) $ simEMmod sign tm2
  TransClos tm1 -> TransClos $ simEMmod sign tm1
  Guard frm -> Guard $ simplifySen frmTypeAna simEMSen sign frm
  TermMod trm -> TermMod $ simplifyTerm frmTypeAna simEMSen sign trm
  _ -> tm

-- Simplification of formulas - simplifySen for ExtFORMULA
simEMSen :: Sign EM_FORMULA EModalSign -> EM_FORMULA -> EM_FORMULA
simEMSen sign frm =
  let rsimf = simplifySen frmTypeAna simEMSen sign in case frm of
  BoxOrDiamond choice tm leq_geq number f pos -> BoxOrDiamond choice
      (simEMmod sign tm) leq_geq number (rsimf f) pos
  Hybrid choice nom f pos -> Hybrid choice nom (rsimf f) pos
  UntilSince choice f1 f2 pos -> UntilSince choice (rsimf f1) (rsimf f2) pos
  NextY choice f pos -> NextY choice (rsimf f) pos
  PathQuantification choice f pos -> PathQuantification choice (rsimf f) pos
  StateQuantification t_dir choice f pos ->
    StateQuantification t_dir choice (rsimf f) pos
  FixedPoint choice p_var f pos -> FixedPoint choice p_var (rsimf f) pos
  ModForm (ModDefn ti te is fs pos) -> ModForm $ ModDefn ti te is
    (map (fmap rsimf) fs) pos

instance Sentences ExtModal ExtModalFORMULA ExtModalSign ExtModalMorph Symbol
    where
        map_sen ExtModal morph = return . mapSen mapEMform morph
        simplify_sen ExtModal = simplifySen frmTypeAna simEMSen
        print_sign ExtModal sig = printSign pretty sig
        sym_of ExtModal = symOf
        symmap_of ExtModal = morphismToSymbMap
        sym_name ExtModal = symName

instance StaticAnalysis ExtModal EM_BASIC_SPEC ExtModalFORMULA SYMB_ITEMS
    SYMB_MAP_ITEMS ExtModalSign ExtModalMorph Symbol RawSymbol where
        basic_analysis ExtModal = Just basicEModalAnalysis
        stat_symb_map_items ExtModal = statSymbMapItems
        stat_symb_items ExtModal = statSymbItems

        symbol_to_raw ExtModal = symbolToRaw
        id_to_raw ExtModal = idToRaw
        matches ExtModal = CASL.Morphism.matches

        empty_signature ExtModal = emptySign emptyEModalSign
        signature_union ExtModal sgn = return . addSig addEModalSign sgn
        intersection ExtModal sgn = return . interSig interEModalSign sgn
        signatureDiff ExtModal sgn = return . diffSig diffEModalSign sgn
        final_union ExtModal = finalUnion addEModalSign
        morphism_union ExtModal = plainMorphismUnion addEModalSign
        is_subsig ExtModal = isSubSig isSubEModalSign
        subsig_inclusion ExtModal = sigInclusion emptyMorphExtension
        generated_sign ExtModal = generatedSign emptyMorphExtension
        cogenerated_sign ExtModal = cogeneratedSign emptyMorphExtension
        induced_from_morphism ExtModal = inducedFromMorphismExt inducedEMsign
          (constMorphExt emptyMorphExtension)
        induced_from_to_morphism ExtModal = inducedFromToMorphismExt
          inducedEMsign (constMorphExt emptyMorphExtension)
           (\ _ _ -> return emptyMorphExtension) isSubEModalSign diffEModalSign
        theory_to_taxonomy ExtModal = convTaxo

instance Logic ExtModal () EM_BASIC_SPEC ExtModalFORMULA SYMB_ITEMS
    SYMB_MAP_ITEMS ExtModalSign ExtModalMorph Symbol RawSymbol () where
        stability _ = Experimental
        empty_proof_tree _ = ()
