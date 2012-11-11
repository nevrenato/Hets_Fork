{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{- |
Module      :  $Header$
Description :  Comorphism from CommonLogic to CASL
Copyright   :  (c) Uni Bremen 2011
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  eugenk@informatik.uni-bremen.de
Stability   :  provisional
Portability :  non-portable (via Logic.Logic)

Translating comorphism from Common Logic to CASL

-}

module Comorphisms.CommonLogic2CASL
   (
     CommonLogic2CASL (..)
   )
   where

import Logic.Logic as Logic
import Logic.Comorphism

import Common.ProofTree
import Common.Result
import Common.AS_Annotation as AS_Anno
import qualified Common.Lib.MapSet as MapSet
import qualified Common.Id as Id

import qualified Data.Set as Set
import qualified Data.Map as Map

-- Common Logic
import qualified CommonLogic.Logic_CommonLogic as ClLogic
import qualified CommonLogic.AS_CommonLogic as ClBasic
import qualified CommonLogic.Sign as ClSign
import qualified CommonLogic.Symbol as ClSymbol
import qualified CommonLogic.Morphism as ClMor
import qualified CommonLogic.Sublogic as ClSl

import Comorphisms.CommonLogicModuleElimination (eliminateModules)

import CommonLogic.PredefinedCASLAxioms as Predefined

-- CASL
import qualified CASL.Logic_CASL as CLogic
import qualified CASL.AS_Basic_CASL as CBasic
import qualified CASL.Sublogic as CSL
import qualified CASL.Sign as CSign
import qualified CASL.Morphism as CMor

data CommonLogic2CASL = CommonLogic2CASL deriving Show

instance Language CommonLogic2CASL where
  language_name CommonLogic2CASL = "CommonLogic2CASL"

instance Comorphism
    CommonLogic2CASL       -- comorphism
    ClLogic.CommonLogic    -- lid domain
    ClSl.CommonLogicSL     -- sublogics codomain
    ClBasic.BASIC_SPEC     -- Basic spec domain
    ClBasic.TEXT_META      -- sentence domain
    ClBasic.SYMB_ITEMS     -- symbol items domain
    ClBasic.SYMB_MAP_ITEMS -- symbol map items domain
    ClSign.Sign            -- signature domain
    ClMor.Morphism         -- morphism domain
    ClSymbol.Symbol        -- symbol domain
    ClSymbol.Symbol        -- rawsymbol domain
    ProofTree              -- proof tree codomain
    CLogic.CASL            -- lid codomain
    CSL.CASL_Sublogics     -- sublogics codomain
    CLogic.CASLBasicSpec   -- Basic spec codomain
    CBasic.CASLFORMULA     -- sentence codomain
    CBasic.SYMB_ITEMS      -- symbol items codomain
    CBasic.SYMB_MAP_ITEMS  -- symbol map items codomain
    CSign.CASLSign         -- signature codomain
    CMor.CASLMor           -- morphism codomain
    CSign.Symbol           -- symbol codomain
    CMor.RawSymbol         -- rawsymbol codomain
    ProofTree              -- proof tree domain
    where
      sourceLogic CommonLogic2CASL = ClLogic.CommonLogic
      sourceSublogic CommonLogic2CASL = ClSl.funcNoPredsl
      targetLogic CommonLogic2CASL = CLogic.CASL
      mapSublogic CommonLogic2CASL = Just . mapSub
      map_theory CommonLogic2CASL = mapTheory
      map_morphism CommonLogic2CASL = mapMor  -- TODO prop
      map_sentence CommonLogic2CASL = mapSentence
      has_model_expansion CommonLogic2CASL = True

mapSub :: ClSl.CommonLogicSL -> CSL.CASL_Sublogics
mapSub _ = CSL.caslTop
        { CSL.cons_features = CSL.emptyMapConsFeature
        , CSL.sub_features = CSL.NoSub }

mapMor :: ClMor.Morphism -> Result CMor.CASLMor
mapMor mor = Result [] $ Just (CMor.embedMorphism ()
    (mapSig $ ClMor.source mor) $ mapSig $ ClMor.target mor)
    { CMor.pred_map = trMor $ ClMor.propMap mor }

-- | Helper for map mor
trMor :: Map.Map Id.Id Id.Id -> Map.Map (Id.Id, CSign.PredType) Id.Id
trMor mp =
    let
        pt = CSign.PredType {CSign.predArgs = []}
    in
      Map.foldWithKey
             (\ k a ->
              Map.insert (k, pt) a
             )
      Map.empty
      mp

-- |
mapTheory :: (ClSign.Sign,
              [AS_Anno.Named ClBasic.TEXT_META])
              -> Result
                  (CSign.CASLSign,
                   [AS_Anno.Named CBasic.CASLFORMULA])
mapTheory (sig, form) =  Result [] $
  Just (mapSig sig, Predefined.baseCASLAxioms ++ (map (trNamedForm sig) form))
  

mapSig :: ClSign.Sign -> CSign.CASLSign
mapSig sign = CSign.uniteCASLSign ((CSign.emptySign ()) {
               CSign.opMap = Set.fold (\ x -> MapSet.insert x
                                $ CSign.mkTotOpType [] individual)
                                MapSet.empty $ ClSign.allItems sign
               }) Predefined.caslSig

trNamedForm :: ClSign.Sign -> AS_Anno.Named (ClBasic.TEXT_META)
            -> AS_Anno.Named (CBasic.CASLFORMULA)
trNamedForm sig form = AS_Anno.mapNamed (trFormMeta sig . eliminateModules) form

mapSentence :: ClSign.Sign -> ClBasic.TEXT_META -> Result CBasic.CASLFORMULA
mapSentence sig form = Result [] $ Just $ trFormMeta sig (eliminateModules form)

-- ignores importations
trFormMeta :: ClSign.Sign -> ClBasic.TEXT_META -> CBasic.CASLFORMULA
trFormMeta sig tm = trForm sig $ ClBasic.getText tm

trForm :: ClSign.Sign -> ClBasic.TEXT -> CBasic.CASLFORMULA
trForm sig form = 
   case form of
     ClBasic.Text phrs rn ->
        let ps = filter nonImportAndNonEmpty phrs in
        if null ps then CBasic.True_atom Id.nullRange else
        CBasic.Conjunction (map (phraseForm sig) ps) rn
     ClBasic.Named_text _ t _ -> trForm sig t
   where nonImportAndNonEmpty :: ClBasic.PHRASE -> Bool
         nonImportAndNonEmpty p = case p of
            ClBasic.Importation _ -> False
            ClBasic.Comment_text _ t _ -> not $ isTextEmpty t
            _ -> True
         isTextEmpty :: ClBasic.TEXT -> Bool
         isTextEmpty txt = case txt of
            ClBasic.Named_text _ t _ -> isTextEmpty t
            ClBasic.Text [] _ -> True
            _ -> False

phraseForm :: ClSign.Sign -> ClBasic.PHRASE -> CBasic.CASLFORMULA
phraseForm sig phr =
   case phr of
     ClBasic.Module _ -> undefined -- cannot occur because module elimination applied
     ClBasic.Sentence s -> senForm sig s
     ClBasic.Importation _ -> undefined -- cannot occur, because filtered
     ClBasic.Comment_text _ t _ -> trForm sig t

senForm :: ClSign.Sign -> ClBasic.SENTENCE -> CBasic.CASLFORMULA
senForm sig form =
   case form of
     ClBasic.Bool_sent bs rn
        -> case bs of
             ClBasic.Negation s -> CBasic.Negation (senForm sig s) rn
             ClBasic.Conjunction [] -> CBasic.True_atom Id.nullRange
             ClBasic.Disjunction [] -> CBasic.True_atom Id.nullRange
             ClBasic.Conjunction ss ->
                CBasic.Conjunction (map (senForm sig) ss) rn
             ClBasic.Disjunction ss ->
                CBasic.Disjunction (map (senForm sig) ss) rn
             ClBasic.Implication s1 s2 ->
                CBasic.Implication (senForm sig s1) (senForm sig s2) True rn
             ClBasic.Biconditional s1 s2 -> CBasic.Equivalence
                                             (senForm sig s1) (senForm sig s2) rn
     ClBasic.Quant_sent qs rn
        -> case qs of
             ClBasic.Universal bs s ->
               CBasic.Quantification CBasic.Universal
               [CBasic.Var_decl (map bindingSeq bs) individual Id.nullRange]
               (senForm sig s) rn
             ClBasic.Existential bs s ->
               CBasic.Quantification CBasic.Existential
               [CBasic.Var_decl (map bindingSeq bs) individual Id.nullRange]
               (senForm sig s) rn
     ClBasic.Atom_sent at rn
        -> case at of
             ClBasic.Equation trm1 trm2 ->
                CBasic.Strong_equation (termForm sig trm1) (termForm sig trm2) rn
             ClBasic.Atom trm ts -> CBasic.Predication
                                       (CBasic.Qual_pred_name rel
                                       (CBasic.Pred_type [individual, list]
                                        Id.nullRange)
                                        Id.nullRange) ([termForm sig trm] ++
                                    (consSeq sig ts) : []) Id.nullRange
     ClBasic.Comment_sent _ s _ -> senForm sig s
     ClBasic.Irregular_sent s _ -> senForm sig s

termForm :: ClSign.Sign -> ClBasic.TERM -> CBasic.TERM a
termForm sig trm = case trm of
                 ClBasic.Name_term name ->
                    if ClSign.isSubSigOf (ClSign.emptySig {
                            ClSign.discourseNames =
                                Set.singleton (Id.simpleIdToId name)
                          }) sig
                    then CBasic.Application
                          (CBasic.Qual_op_name (Id.simpleIdToId name)
                            (CBasic.Op_type CBasic.Total [] individual Id.nullRange)
                            Id.nullRange)
                          [] $ Id.tokPos name
                    else CBasic.Qual_var name individual Id.nullRange
                 ClBasic.Funct_term term ts _ ->
                    CBasic.Application
                        (CBasic.Qual_op_name fun
                          (CBasic.Op_type
                            CBasic.Total [individual, list]
                            individual Id.nullRange)
                          Id.nullRange)
                        ([termForm sig term] ++
                          (consSeq sig ts) : []) Id.nullRange
                 ClBasic.Comment_term term _ _ -> termForm sig term

consSeq :: ClSign.Sign -> [ClBasic.TERM_SEQ] -> CBasic.TERM a
consSeq _ [] = CBasic.Application (CBasic.Qual_op_name nil
  (CBasic.Op_type CBasic.Total [] list Id.nullRange)
  Id.nullRange) [] Id.nullRange
consSeq sig (x : xs) = CBasic.Application (CBasic.Qual_op_name cons
  (CBasic.Op_type CBasic.Total [individual, list] list Id.nullRange)
  Id.nullRange) [termSeqForm sig x, consSeq sig xs] Id.nullRange

termSeqForm :: ClSign.Sign -> ClBasic.TERM_SEQ -> CBasic.TERM a
termSeqForm sig ts = case ts of
  ClBasic.Term_seq trm -> case trm of
    ClBasic.Name_term name -> if not subSig then
      CBasic.Qual_var name individual Id.nullRange else
          termForm sig trm
      where subSig = ClSign.isSubSigOf new sig
            new    = ClSign.emptySig
                  {
                    ClSign.discourseNames = Set.singleton $ Id.simpleIdToId name
                  }
    ClBasic.Funct_term _ _ _ -> termForm sig trm
    ClBasic.Comment_term _ _ _ -> termForm sig trm
  ClBasic.Seq_marks seqm ->
      if ClSign.isSubSigOf (ClSign.emptySig {
              ClSign.discourseNames =
                  Set.singleton (Id.simpleIdToId seqm)
            }) sig
      then CBasic.Application
            (CBasic.Qual_op_name (Id.simpleIdToId seqm)
              (CBasic.Op_type CBasic.Total [] individual Id.nullRange)
              Id.nullRange)
            [] $ Id.tokPos seqm
      else CBasic.Qual_var seqm individual Id.nullRange

bindingSeq :: ClBasic.NAME_OR_SEQMARK -> CBasic.VAR
bindingSeq bs = case bs of
                  ClBasic.Name name -> name
                  ClBasic.SeqMark seqm -> seqm
