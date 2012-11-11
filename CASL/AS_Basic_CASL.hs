{- |
Module      :  $Header$
Description :  Abstract syntax of CASL basic specifications
Copyright   :  (c) Klaus Luettich, Christian Maeder, Uni Bremen 2002-2006
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  portable

Abstract Syntax of CASL Basic_specs, Symb_items and Symb_map_items.

   Follows Sect. II:2.2 of the CASL Reference Manual.
-}

module CASL.AS_Basic_CASL where

import Common.Id
import Common.AS_Annotation
import qualified Data.Set as Set

-- DrIFT command
{-! global: GetRange !-}

data BASIC_SPEC b s f = Basic_spec [Annoted (BASIC_ITEMS b s f)]
                  deriving Show

data BASIC_ITEMS b s f = Sig_items (SIG_ITEMS s f)
                   {- the Annotation following the keyword is dropped
                   but preceding the keyword is now an Annotation allowed -}
                 | Free_datatype SortsKind [Annoted DATATYPE_DECL] Range
                   -- pos: free, type, semi colons
                 | Sort_gen [Annoted (SIG_ITEMS s f)] Range
                   -- pos: generated, opt. braces
                 | Var_items [VAR_DECL] Range
                   -- pos: var, semi colons
                 | Local_var_axioms [VAR_DECL] [Annoted (FORMULA f)] Range
                   -- pos: forall, semi colons, dots
                 | Axiom_items [Annoted (FORMULA f)] Range
                   -- pos: dots
                 | Ext_BASIC_ITEMS b
                   deriving Show

data SortsKind = NonEmptySorts | PossiblyEmptySorts deriving Show

data SIG_ITEMS s f = Sort_items SortsKind [Annoted (SORT_ITEM f)] Range
                 -- pos: sort, semi colons
               | Op_items [Annoted (OP_ITEM f)] Range
                 -- pos: op, semi colons
               | Pred_items [Annoted (PRED_ITEM f)] Range
                 -- pos: pred, semi colons
               | Datatype_items SortsKind [Annoted DATATYPE_DECL] Range
                 -- type, semi colons
               | Ext_SIG_ITEMS s
                 deriving Show

data SORT_ITEM f = Sort_decl [SORT] Range
                 -- pos: commas
               | Subsort_decl [SORT] SORT Range
                 -- pos: commas, <
               | Subsort_defn SORT VAR SORT (Annoted (FORMULA f)) Range
                 {- pos: "=", "{", ":", ".", "}"
                 the left anno list stored in Annoted Formula is
                 parsed after the equal sign -}
               | Iso_decl [SORT] Range
                 -- pos: "="s
                 deriving Show

data OP_ITEM f = Op_decl [OP_NAME] OP_TYPE [OP_ATTR f] Range
               -- pos: commas, colon, OP_ATTR sep. by commas
             | Op_defn OP_NAME OP_HEAD (Annoted (TERM f)) Range
               -- pos: "="
               deriving Show

data OpKind = Total | Partial deriving (Show, Eq, Ord)

data OP_TYPE = Op_type OpKind [SORT] SORT Range
               -- pos: "*"s, "->" ; if null [SORT] then Range = [] or pos: "?"
               deriving (Show, Eq, Ord)

args_OP_TYPE :: OP_TYPE -> [SORT]
args_OP_TYPE (Op_type _ args _ _) = args

res_OP_TYPE :: OP_TYPE -> SORT
res_OP_TYPE (Op_type _ _ res _) = res

data OP_HEAD = Op_head OpKind [VAR_DECL] (Maybe SORT) Range
               -- pos: "(", semicolons, ")", colon
               deriving (Show, Eq, Ord)

data OP_ATTR f = Assoc_op_attr | Comm_op_attr | Idem_op_attr
             | Unit_op_attr (TERM f)
               deriving (Show, Eq, Ord)

data PRED_ITEM f = Pred_decl [PRED_NAME] PRED_TYPE Range
                 -- pos: commas, colon
               | Pred_defn PRED_NAME PRED_HEAD (Annoted (FORMULA f)) Range
                 -- pos: "<=>"
                 deriving Show

data PRED_TYPE = Pred_type [SORT] Range
                 -- pos: if null [SORT] then "(",")" else "*"s
                 deriving (Show, Eq, Ord)

data PRED_HEAD = Pred_head [VAR_DECL] Range
                 -- pos: "(",semi colons , ")"
                 deriving Show

data DATATYPE_DECL = Datatype_decl SORT [Annoted ALTERNATIVE] Range
                     -- pos: "::=", "|"s
                     deriving Show

data ALTERNATIVE = Alt_construct OpKind OP_NAME [COMPONENTS] Range
                   -- pos: "(", semi colons, ")" optional "?"
                 | Subsorts [SORT] Range
                   -- pos: sort, commas
                   deriving Show

data COMPONENTS = Cons_select OpKind [OP_NAME] SORT Range
                  -- pos: commas, colon or ":?"
                | Sort SORT
                  deriving Show

data VAR_DECL = Var_decl [VAR] SORT Range
                -- pos: commas, colon
                deriving (Show, Eq, Ord)

varDeclRange :: VAR_DECL -> [Pos]
varDeclRange (Var_decl vs s _) = case vs of
  [] -> []
  v : _ -> joinRanges [tokenRange v, idRange s]

{- Position definition for FORMULA:
   Information on parens are also encoded in Range.  If there
   are more Pos than necessary there is a pair of Pos enclosing the
   other Pos informations which encode the brackets of every kind
-}

data FORMULA f = Quantification QUANTIFIER [VAR_DECL] (FORMULA f) Range
               -- pos: QUANTIFIER, semi colons, dot
             | Conjunction [FORMULA f] Range
               -- pos: "/\"s
             | Disjunction [FORMULA f] Range
               -- pos: "\/"s
             | Implication (FORMULA f) (FORMULA f) Bool Range
               -- pos: "=>" or "if" (True -> "=>")
             | Equivalence (FORMULA f) (FORMULA f) Range
               -- pos: "<=>"
             | Negation (FORMULA f) Range
               -- pos: not
             | True_atom Range
               -- pos: true
             | False_atom Range
               -- pos: false
             | Predication PRED_SYMB [TERM f] Range
               -- pos: opt. "(",commas,")"
             | Definedness (TERM f) Range
               -- pos: def
             | Existl_equation (TERM f) (TERM f) Range
               -- pos: =e=
             | Strong_equation (TERM f) (TERM f) Range
               -- pos: =
             | Membership (TERM f) SORT Range
               -- pos: in
             | Mixfix_formula (TERM f)
               {- Mixfix_ Term/Token/(..)/[..]/{..}
               a formula left original for mixfix analysis -}
             | Unparsed_formula String Range
               -- pos: first Char in String
             | Sort_gen_ax [Constraint] Bool -- flag: belongs to a free type?
             | QuantOp OP_NAME OP_TYPE (FORMULA f) -- second order quantifiers
             | QuantPred PRED_NAME PRED_TYPE (FORMULA f)
             | ExtFORMULA f
             -- needed for CASL extensions
               deriving (Show, Eq, Ord)

is_True_atom :: FORMULA f -> Bool
is_True_atom f = case f of
  True_atom _ -> True
  _ -> False

is_False_atom :: FORMULA f -> Bool
is_False_atom f = case f of
  False_atom _ -> True
  _ -> False

trueForm :: FORMULA f
trueForm = True_atom nullRange

falseForm :: FORMULA f
falseForm = False_atom nullRange

{- In the CASL institution, sort generation constraints have an
additional signature morphism component (Sect. III:2.1.3, p.134 of the
CASL Reference Manual).  The extra signature morphism component is
needed because the naive translation of sort generation constraints
along signature morphisms may violate the satisfaction condition,
namely when sorts are identified by the translation, with the effect
that new terms can be formed. We avoid this extra component here and
instead use natural numbers to decorate sorts, in this way retaining
their identity w.r.t. the original signature. The newSort in a
Constraint is implicitly decorated with its index in the list of
Constraints. The opSymbs component collects all the operation symbols
with newSort (with that index!) as a result sort. The argument sorts
of an operation symbol are decorated explicitly via a list [Int] of
integers. The origSort in a Constraint is the original sort
corresponding to the index.  A negative index indicates a sort outside
the constraint (i.e. a "parameter sort"). Note that this representation of
sort generation constraints is efficiently tailored towards both the use in
the proof calculus (Chap. IV:2, p. 282 of the CASL Reference Manual)
and the coding into second order logic (p. 429 of Theoret. Comp. Sci. 286).
-}

data Constraint = Constraint { newSort :: SORT,
                               opSymbs :: [(OP_SYMB, [Int])],
                               origSort :: SORT }
                  deriving (Show, Eq, Ord)

-- | no duplicate sorts, i.e. injective sort map?
isInjectiveList :: Ord a => [a] -> Bool
isInjectiveList l = Set.size (Set.fromList l) == length l

{- | from a Sort_gex_ax, recover:
a traditional sort generation constraint plus a sort mapping -}
recover_Sort_gen_ax :: [Constraint] ->
                        ([SORT], [OP_SYMB], [(SORT, SORT)])
recover_Sort_gen_ax constrs =
  if isInjectiveList sorts
     -- we can ignore indices
  then (sorts, map fst (concatMap opSymbs constrs), [])
     {- otherwise, we have to introduce new sorts for the indices
     and afterwards rename them into the sorts they denote -}
  else (origSorts, indOps, zip origSorts sorts)
  where
  sorts = map newSort constrs
  origSorts = map origSort constrs
  indSort s i = if i < 0 then s else origSorts !! i
  indOps = concatMap (\ c -> map (indOp $ origSort c) $ opSymbs c) constrs
  indOp res (Qual_op_name on (Op_type k args1 _ pos1) pos, args) =
     Qual_op_name on
         (Op_type k (zipWith indSort args1 args) res pos1) pos
  indOp _ _ = error
      "CASL/AS_Basic_CASL: Internal error: Unqualified OP_SYMB in Sort_gen_ax"

{- | from a free Sort_gex_ax, recover:
the sorts, each paired with the constructors
fails (i.e. delivers Nothing) if the sort map is not injective -}
recover_free_Sort_gen_ax :: [Constraint] -> Maybe [(SORT, [OP_SYMB])]
recover_free_Sort_gen_ax constrs =
  if isInjectiveList sorts
     then Just $ map getOpProfile constrs
     else Nothing
  where
  sorts = map newSort constrs
  getOpProfile constr = (newSort constr, map fst $ opSymbs constr)

data QUANTIFIER = Universal | Existential | Unique_existential
                  deriving (Show, Eq, Ord)

data PRED_SYMB = Pred_name PRED_NAME
               | Qual_pred_name PRED_NAME PRED_TYPE Range
                 -- pos: "(", pred, colon, ")"
                 deriving (Show, Eq, Ord)

predSymbName :: PRED_SYMB -> PRED_NAME
predSymbName p = case p of
  Pred_name n -> n
  Qual_pred_name n _ _ -> n

data TERM f = Qual_var VAR SORT Range -- pos: "(", var, colon, ")"
          | Application OP_SYMB [TERM f] Range
            -- pos: parens around TERM f if any and seperating commas
          | Sorted_term (TERM f) SORT Range
            -- pos: colon
          | Cast (TERM f) SORT Range
            -- pos: "as"
          | Conditional (TERM f) (FORMULA f) (TERM f) Range
            -- pos: "when", "else"
          | Unparsed_term String Range        -- SML-CATS

          -- A new intermediate state
          | Mixfix_qual_pred PRED_SYMB -- as part of a mixfix formula
          | Mixfix_term [TERM f]  -- not starting with Mixfix_sorted_term/cast
          | Mixfix_token Token   -- NO-BRACKET-TOKEN, LITERAL, PLACE
          | Mixfix_sorted_term SORT Range
            -- pos: colon
          | Mixfix_cast SORT Range
            -- pos: "as"
          | Mixfix_parenthesized [TERM f] Range
            {- non-emtpy term list
            pos: "(", commas, ")" -}
          | Mixfix_bracketed [TERM f] Range
            -- pos: "[", commas, "]"
          | Mixfix_braced [TERM f] Range
            {- also for list-notation
            pos: "{", "}" -}
          | ExtTERM f
            deriving (Show, Eq, Ord)

-- | state after mixfix- but before overload resolution
varOrConst :: Token -> TERM f
varOrConst t = Application (Op_name $ simpleIdToId t) [] $ tokPos t

data OP_SYMB = Op_name OP_NAME
             | Qual_op_name OP_NAME OP_TYPE Range
                 -- pos: "(", op, colon, ")"
               deriving (Show, Eq, Ord)

opSymbName :: OP_SYMB -> OP_NAME
opSymbName o = case o of
  Op_name n -> n
  Qual_op_name n _ _ -> n

-- * short cuts for terms and formulas

-- | create binding if variables are non-null
mkForallRange :: [VAR_DECL] -> FORMULA f -> Range -> FORMULA f
mkForallRange vl f ps =
  if null vl then f else Quantification Universal vl f ps

mkForall :: [VAR_DECL] -> FORMULA f -> FORMULA f
mkForall vl f = mkForallRange vl f nullRange

-- | create an existential binding
mkExist :: [VAR_DECL] -> FORMULA f -> FORMULA f
mkExist vs f = Quantification Existential vs f nullRange

-- | convert a singleton variable declaration into a qualified variable
toQualVar :: VAR_DECL -> TERM f
toQualVar (Var_decl v s ps) =
    if isSingle v then Qual_var (head v) s ps else error "toQualVar"

mkImpl :: FORMULA f -> FORMULA f -> FORMULA f
mkImpl f f' = Implication f f' True nullRange

mkExEq :: TERM f -> TERM f -> FORMULA f
mkExEq f f' = Existl_equation f f' nullRange

mkStEq :: TERM f -> TERM f -> FORMULA f
mkStEq u v = Strong_equation u v nullRange

mkEqv :: FORMULA f -> FORMULA f -> FORMULA f
mkEqv u v = Equivalence u v nullRange

mkAppl :: OP_SYMB -> [TERM f] -> TERM f
mkAppl op_symb fs = Application op_symb fs nullRange

mkPredication :: PRED_SYMB -> [TERM f] -> FORMULA f
mkPredication symb fs = Predication symb fs nullRange

-- | turn sorted variable into variable delcaration
mkVarDecl :: VAR -> SORT -> VAR_DECL
mkVarDecl v s = Var_decl [v] s nullRange

-- | turn sorted variable into term
mkVarTerm :: VAR -> SORT -> TERM f
mkVarTerm v = toQualVar . mkVarDecl v

-- | optimized conjunction
conjunctRange :: [FORMULA f] -> Range -> FORMULA f
conjunctRange fs ps = case fs of
  [] -> True_atom ps
  [phi] -> phi
  _ -> Conjunction fs ps

conjunct :: [FORMULA f] -> FORMULA f
conjunct fs = conjunctRange fs nullRange

disjunctRange :: [FORMULA f] -> Range -> FORMULA f
disjunctRange fs ps = case fs of
  [] -> False_atom ps
  [phi] -> phi
  _ -> Disjunction fs ps

disjunct :: [FORMULA f] -> FORMULA f
disjunct fs = disjunctRange fs nullRange

mkQualOp :: OP_NAME -> OP_TYPE -> OP_SYMB
mkQualOp f ty = Qual_op_name f ty nullRange

mkQualPred :: PRED_NAME -> PRED_TYPE -> PRED_SYMB
mkQualPred f ty = Qual_pred_name f ty nullRange

negateForm :: FORMULA f -> Range -> FORMULA f
negateForm f r = case f of
  False_atom ps -> True_atom ps
  True_atom ps -> False_atom ps
  Negation nf _ -> nf
  _ -> Negation f r

mkNeg :: FORMULA f -> FORMULA f
mkNeg f = negateForm f nullRange

mkVarDeclStr :: String -> SORT -> VAR_DECL
mkVarDeclStr = mkVarDecl . mkSimpleId

-- * type synonyms

type CASLFORMULA = FORMULA ()
type CASLTERM = TERM ()

type OP_NAME = Id
type PRED_NAME = Id
type SORT = Id
type VAR = Token

data SYMB_ITEMS = Symb_items SYMB_KIND [SYMB] Range
                  -- pos: SYMB_KIND, commas
                  deriving (Show, Eq)

data SYMB_MAP_ITEMS = Symb_map_items SYMB_KIND [SYMB_OR_MAP] Range
                      -- pos: SYMB_KIND, commas
                      deriving (Show, Eq)

data SYMB_KIND = Implicit | Sorts_kind
               | Ops_kind | Preds_kind
                 deriving (Show, Eq, Ord)

data SYMB = Symb_id Id
          | Qual_id Id TYPE Range
            -- pos: colon
            deriving (Show, Eq)

data TYPE = O_type OP_TYPE
          | P_type PRED_TYPE
          | A_type SORT -- ambiguous pred or (constant total) op
            deriving (Show, Eq)

data SYMB_OR_MAP = Symb SYMB
                 | Symb_map SYMB SYMB Range
                   -- pos: "|->"
                   deriving (Show, Eq)

-- Generated by DrIFT, look but don't touch!

instance (GetRange b, GetRange s,
          GetRange f) => GetRange (BASIC_SPEC b s f) where
  getRange = const nullRange
  rangeSpan x = case x of
    Basic_spec a -> joinRanges [rangeSpan a]

instance (GetRange b, GetRange s,
          GetRange f) => GetRange (BASIC_ITEMS b s f) where
  getRange x = case x of
    Sig_items _ -> nullRange
    Free_datatype _ _ p -> p
    Sort_gen _ p -> p
    Var_items _ p -> p
    Local_var_axioms _ _ p -> p
    Axiom_items _ p -> p
    Ext_BASIC_ITEMS _ -> nullRange
  rangeSpan x = case x of
    Sig_items a -> joinRanges [rangeSpan a]
    Free_datatype a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                       rangeSpan c]
    Sort_gen a b -> joinRanges [rangeSpan a, rangeSpan b]
    Var_items a b -> joinRanges [rangeSpan a, rangeSpan b]
    Local_var_axioms a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                          rangeSpan c]
    Axiom_items a b -> joinRanges [rangeSpan a, rangeSpan b]
    Ext_BASIC_ITEMS a -> joinRanges [rangeSpan a]

instance GetRange SortsKind where
  getRange = const nullRange
  rangeSpan x = case x of
    NonEmptySorts -> []
    PossiblyEmptySorts -> []

instance (GetRange s, GetRange f) => GetRange (SIG_ITEMS s f) where
  getRange x = case x of
    Sort_items _ _ p -> p
    Op_items _ p -> p
    Pred_items _ p -> p
    Datatype_items _ _ p -> p
    Ext_SIG_ITEMS _ -> nullRange
  rangeSpan x = case x of
    Sort_items a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                    rangeSpan c]
    Op_items a b -> joinRanges [rangeSpan a, rangeSpan b]
    Pred_items a b -> joinRanges [rangeSpan a, rangeSpan b]
    Datatype_items a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                        rangeSpan c]
    Ext_SIG_ITEMS a -> joinRanges [rangeSpan a]

instance GetRange f => GetRange (SORT_ITEM f) where
  getRange x = case x of
    Sort_decl _ p -> p
    Subsort_decl _ _ p -> p
    Subsort_defn _ _ _ _ p -> p
    Iso_decl _ p -> p
  rangeSpan x = case x of
    Sort_decl a b -> joinRanges [rangeSpan a, rangeSpan b]
    Subsort_decl a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                      rangeSpan c]
    Subsort_defn a b c d e -> joinRanges [rangeSpan a, rangeSpan b,
                                          rangeSpan c, rangeSpan d, rangeSpan e]
    Iso_decl a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange f => GetRange (OP_ITEM f) where
  getRange x = case x of
    Op_decl _ _ _ p -> p
    Op_defn _ _ _ p -> p
  rangeSpan x = case x of
    Op_decl a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                   rangeSpan c, rangeSpan d]
    Op_defn a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                   rangeSpan c, rangeSpan d]

instance GetRange OpKind where
  getRange = const nullRange
  rangeSpan x = case x of
    Total -> []
    Partial -> []

instance GetRange OP_TYPE where
  getRange x = case x of
    Op_type _ _ _ p -> p
  rangeSpan x = case x of
    Op_type a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                   rangeSpan c, rangeSpan d]

instance GetRange OP_HEAD where
  getRange x = case x of
    Op_head _ _ _ p -> p
  rangeSpan x = case x of
    Op_head a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                   rangeSpan c, rangeSpan d]

instance GetRange f => GetRange (OP_ATTR f) where
  getRange = const nullRange
  rangeSpan x = case x of
    Assoc_op_attr -> []
    Comm_op_attr -> []
    Idem_op_attr -> []
    Unit_op_attr a -> joinRanges [rangeSpan a]

instance GetRange f => GetRange (PRED_ITEM f) where
  getRange x = case x of
    Pred_decl _ _ p -> p
    Pred_defn _ _ _ p -> p
  rangeSpan x = case x of
    Pred_decl a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                   rangeSpan c]
    Pred_defn a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                     rangeSpan c, rangeSpan d]

instance GetRange PRED_TYPE where
  getRange x = case x of
    Pred_type _ p -> p
  rangeSpan x = case x of
    Pred_type a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange PRED_HEAD where
  getRange x = case x of
    Pred_head _ p -> p
  rangeSpan x = case x of
    Pred_head a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange DATATYPE_DECL where
  getRange x = case x of
    Datatype_decl _ _ p -> p
  rangeSpan x = case x of
    Datatype_decl a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                       rangeSpan c]

instance GetRange ALTERNATIVE where
  getRange x = case x of
    Alt_construct _ _ _ p -> p
    Subsorts _ p -> p
  rangeSpan x = case x of
    Alt_construct a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                         rangeSpan c, rangeSpan d]
    Subsorts a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange COMPONENTS where
  getRange x = case x of
    Cons_select _ _ _ p -> p
    Sort _ -> nullRange
  rangeSpan x = case x of
    Cons_select a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                       rangeSpan c, rangeSpan d]
    Sort a -> joinRanges [rangeSpan a]

instance GetRange VAR_DECL where
  getRange x = case x of
    Var_decl _ _ p -> p
  rangeSpan x = case x of
    Var_decl a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                  rangeSpan c]

instance GetRange f => GetRange (FORMULA f) where
  getRange x = case x of
    Quantification _ _ _ p -> p
    Conjunction _ p -> p
    Disjunction _ p -> p
    Implication _ _ _ p -> p
    Equivalence _ _ p -> p
    Negation _ p -> p
    True_atom p -> p
    False_atom p -> p
    Predication _ _ p -> p
    Definedness _ p -> p
    Existl_equation _ _ p -> p
    Strong_equation _ _ p -> p
    Membership _ _ p -> p
    Mixfix_formula _ -> nullRange
    Unparsed_formula _ p -> p
    Sort_gen_ax _ _ -> nullRange
    QuantOp _ _ _ -> nullRange
    QuantPred _ _ _ -> nullRange
    ExtFORMULA _ -> nullRange
  rangeSpan x = case x of
    Quantification a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                          rangeSpan c, rangeSpan d]
    Conjunction a b -> joinRanges [rangeSpan a, rangeSpan b]
    Disjunction a b -> joinRanges [rangeSpan a, rangeSpan b]
    Implication a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                       rangeSpan c, rangeSpan d]
    Equivalence a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                     rangeSpan c]
    Negation a b -> joinRanges [rangeSpan a, rangeSpan b]
    True_atom a -> joinRanges [rangeSpan a]
    False_atom a -> joinRanges [rangeSpan a]
    Predication a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                     rangeSpan c]
    Definedness a b -> joinRanges [rangeSpan a, rangeSpan b]
    Existl_equation a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                         rangeSpan c]
    Strong_equation a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                         rangeSpan c]
    Membership a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                    rangeSpan c]
    Mixfix_formula a -> joinRanges [rangeSpan a]
    Unparsed_formula a b -> joinRanges [rangeSpan a, rangeSpan b]
    Sort_gen_ax a b -> joinRanges [rangeSpan a, rangeSpan b]
    QuantOp a b c -> joinRanges [rangeSpan a, rangeSpan b, rangeSpan c]
    QuantPred a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                   rangeSpan c]
    ExtFORMULA a -> joinRanges [rangeSpan a]

instance GetRange Constraint where
  getRange = const nullRange
  rangeSpan x = case x of
    Constraint a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                    rangeSpan c]

instance GetRange QUANTIFIER where
  getRange = const nullRange
  rangeSpan x = case x of
    Universal -> []
    Existential -> []
    Unique_existential -> []

instance GetRange PRED_SYMB where
  getRange x = case x of
    Pred_name _ -> nullRange
    Qual_pred_name _ _ p -> p
  rangeSpan x = case x of
    Pred_name a -> joinRanges [rangeSpan a]
    Qual_pred_name a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                        rangeSpan c]

instance GetRange f => GetRange (TERM f) where
  getRange x = case x of
    Qual_var _ _ p -> p
    Application _ _ p -> p
    Sorted_term _ _ p -> p
    Cast _ _ p -> p
    Conditional _ _ _ p -> p
    Unparsed_term _ p -> p
    Mixfix_qual_pred _ -> nullRange
    Mixfix_term _ -> nullRange
    Mixfix_token _ -> nullRange
    Mixfix_sorted_term _ p -> p
    Mixfix_cast _ p -> p
    Mixfix_parenthesized _ p -> p
    Mixfix_bracketed _ p -> p
    Mixfix_braced _ p -> p
    ExtTERM _ -> nullRange
  rangeSpan x = case x of
    Qual_var a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                  rangeSpan c]
    Application a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                     rangeSpan c]
    Sorted_term a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                     rangeSpan c]
    Cast a b c -> joinRanges [rangeSpan a, rangeSpan b, rangeSpan c]
    Conditional a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                       rangeSpan c, rangeSpan d]
    Unparsed_term a b -> joinRanges [rangeSpan a, rangeSpan b]
    Mixfix_qual_pred a -> joinRanges [rangeSpan a]
    Mixfix_term a -> joinRanges [rangeSpan a]
    Mixfix_token a -> joinRanges [rangeSpan a]
    Mixfix_sorted_term a b -> joinRanges [rangeSpan a, rangeSpan b]
    Mixfix_cast a b -> joinRanges [rangeSpan a, rangeSpan b]
    Mixfix_parenthesized a b -> joinRanges [rangeSpan a, rangeSpan b]
    Mixfix_bracketed a b -> joinRanges [rangeSpan a, rangeSpan b]
    Mixfix_braced a b -> joinRanges [rangeSpan a, rangeSpan b]
    ExtTERM a -> joinRanges [rangeSpan a]

instance GetRange OP_SYMB where
  getRange x = case x of
    Op_name _ -> nullRange
    Qual_op_name _ _ p -> p
  rangeSpan x = case x of
    Op_name a -> joinRanges [rangeSpan a]
    Qual_op_name a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                      rangeSpan c]

instance GetRange SYMB_ITEMS where
  getRange x = case x of
    Symb_items _ _ p -> p
  rangeSpan x = case x of
    Symb_items a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                    rangeSpan c]

instance GetRange SYMB_MAP_ITEMS where
  getRange x = case x of
    Symb_map_items _ _ p -> p
  rangeSpan x = case x of
    Symb_map_items a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                        rangeSpan c]

instance GetRange SYMB_KIND where
  getRange = const nullRange
  rangeSpan x = case x of
    Implicit -> []
    Sorts_kind -> []
    Ops_kind -> []
    Preds_kind -> []

instance GetRange SYMB where
  getRange x = case x of
    Symb_id _ -> nullRange
    Qual_id _ _ p -> p
  rangeSpan x = case x of
    Symb_id a -> joinRanges [rangeSpan a]
    Qual_id a b c -> joinRanges [rangeSpan a, rangeSpan b, rangeSpan c]

instance GetRange TYPE where
  getRange = const nullRange
  rangeSpan x = case x of
    O_type a -> joinRanges [rangeSpan a]
    P_type a -> joinRanges [rangeSpan a]
    A_type a -> joinRanges [rangeSpan a]

instance GetRange SYMB_OR_MAP where
  getRange x = case x of
    Symb _ -> nullRange
    Symb_map _ _ p -> p
  rangeSpan x = case x of
    Symb a -> joinRanges [rangeSpan a]
    Symb_map a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                  rangeSpan c]
