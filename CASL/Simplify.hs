{- |
Module      :  $Header$
Description :  resolve empty conjunctions and other trivial cases
Copyright   :  (c) Christian Maeder, Uni Bremen 2005
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  portable

Resolve empty conjunctions and other trivial cases
-}

module CASL.Simplify where

import CASL.AS_Basic_CASL
import CASL.Fold

import Common.Id
import Common.Utils (nubOrd)

negateFormula :: FORMULA f -> Maybe (FORMULA f)
negateFormula f = case f of
  Sort_gen_ax {} -> Nothing
  _ -> Just $ negateForm f nullRange

simplifyRecord :: Ord f => (f -> f) -> Record f (FORMULA f) (TERM f)
simplifyRecord mf = (mapRecord mf)
    { foldConditional = \ _ t1 f t2 ps -> case f of
      True_atom _ -> t1
      False_atom _ -> t2
      _ -> Conditional t1 f t2 ps
    , foldQuantification = \ _ q vs qf ps ->
      let nf = Quantification q vs qf ps in
      case q of
      Unique_existential -> nf
      _ -> if null vs then qf else case (qf, q) of
           (True_atom _, Universal) -> qf
           (False_atom _, Existential) -> qf
           _ -> nf
    , foldConjunction = \ _ fs ps -> if any is_False_atom fs
      then False_atom ps else conjunctRange
           (nubOrd $ filter (not . is_True_atom) fs) ps
    , foldDisjunction = \ _ fs ps -> if any is_True_atom fs
      then True_atom ps else disjunctRange
           (nubOrd $ filter (not . is_False_atom) fs) ps
    , foldImplication = \ _ f1 f2 b ps ->
      let nf1 = negateForm f1 ps
          tf = True_atom ps
      in case f1 of
      True_atom _ -> f2
      False_atom _ -> tf
      _ -> case f2 of
           True_atom _ -> f2
           False_atom _ -> nf1
           _ | f1 == f2 -> tf
             | nf1 == f2 -> f1
           _ -> Implication f1 f2 b ps
    , foldEquivalence = \ _ f1 f2 ps ->
      let nf1 = negateForm f1 ps
      in case f2 of
      True_atom _ -> f1
      False_atom _ -> nf1
      _ -> case f1 of
           True_atom _ -> f2
           False_atom _ -> negateForm f2 ps
           _ | f1 == f2 -> True_atom ps
           _ | nf1 == f2 -> False_atom ps
           _ -> Equivalence f1 f2 ps
    , foldNegation = \ _ nf ps -> negateForm nf ps
    , foldStrong_equation = \ _ t1 t2 ps ->
      if t1 == t2 then True_atom ps else Strong_equation t1 t2 ps
    }

simplifyTerm :: Ord f => (f -> f) -> TERM f -> TERM f
simplifyTerm = foldTerm . simplifyRecord

simplifyFormula :: Ord f => (f -> f) -> FORMULA f -> FORMULA f
simplifyFormula = foldFormula . simplifyRecord
