{- |
Module      :  $Header$
Description :  predefined strings of several Isabelle logics
Copyright   :  (c) Uni Bremen 2005
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  Christian.Maeder@dfki.de
Stability   :  provisional
Portability :  portable

String constants of Isabelle keywords to be excluded by translations

obtained by manually taking @lexicon@ and @consts@ from
@print_syntax(theory "...")@
(initial empty strings omitted)
-}

module Isabelle.IsaStrings where

import qualified Data.Set as Set

-- | convert pasted strings to a relevant set of strings
mkIsaSet :: [String] -> Set.Set String
mkIsaSet = Set.fromList . concatMap words
{- fewer list elements are better to optimize ("\",\"" -> "   ")
   but keep all words in case we also want to use symbols
-}

data IsaSets = IsaSets
    { types :: Set.Set String
    , consts :: Set.Set String
    }

holcfS :: IsaSets
holcfS =
  IsaSets {
  types = mkIsaSet
    [ "lBool intT integerT charT ratT lString"
    , "unitT lOrdering sOrdering llist lprod lEither lMaybe"],
  consts = mkIsaSet [
  "!   !!   #   $   %   &   (   ()   (:   (]   (|   (}   )   *   **",
  "+   ++   ,   -   -->   ->   -`   .   ..   ...   ..}   /   //   0   1   :",
  ":)   ::   :=   ;   <   <*>   <*lex*>   <+>   <<   <<|   <=   <|   =   ==",
  "==>   =>   >   >.   ?   ?!   @   ALL   BIT   CHR   EX   EX!   GREATEST",
  "INF   INT   If   Int   LAM   LEAST   LUB   Let   MOST   O   OFCLASS",
  "PROP   SIGMA   SOME   THE   TYPE   UN   Un   WRT   [   [\\<mapsto>]   []",
  "[|   [|->]   \\<And>   \\<Colon>   \\<Inter>   \\<Inter>\\<^bsub>",
  "\\<Lambda>   \\<Longrightarrow>   \\<Prod>   \\<Rightarrow>   \\<Sigma>",
  "\\<Squnion>   \\<Sum>   \\<Union>   \\<Union>\\<^bsub>   \\<^esub>",
  "\\<^sub>   \\<^sub>1   \\<^sup>*   \\<^sup>+   \\<^sup>=   \\<and>",
  "\\<bar>   \\<bottom>   \\<cdot>   \\<circ>   \\<dots>   \\<epsilon>",
  "\\<equiv>   \\<equiv>\\<^sup>?   \\<exists>   \\<exists>!",
  "\\<exists>\\<^sub>\\<infinity>   \\<forall>",
  "\\<forall>\\<^sub>\\<infinity>   \\<in>   \\<index>   \\<int>",
  "\\<inter>   \\<inverse>   \\<lambda>   \\<lbrakk>   \\<le>   \\<leadsto>",
  "\\<lfloor>   \\<longrightarrow>   \\<lparr>   \\<mapsto>",
  "\\<mapsto>\\<lambda>   \\<nat>   \\<not>   \\<noteq>   \\<notin>",
  "\\<oplus>   \\<or>   \\<otimes>   \\<rbrakk>   \\<rightarrow>",
  "\\<rightharpoonup>   \\<rparr>   \\<sqsubseteq>   \\<struct>",
  "\\<subset>   \\<subseteq>   \\<subseteq>\\<^sub>m   \\<times>",
  "\\<twosuperior>   \\<union>   ]   ^   ^*   ^+   ^-1   ^=   _   _::   `",
  "`>   ``   andalso   case   choose   div   dvd   else   fi   if   in",
  "let   mem   mod   o   of   oo   op   orelse   then   {   {)   {..",
  "{\\<mapsto>}   {|->}   {}   |   |)   |->   |]   |_   }   ~   ~:   ~=",
  "~=>   ~>",

  "!!   #prop   *   **   +   ++   ->   0   1   ==   ==>   =?=",
  "@Coll   @Finset   @INTER   @INTER1   @INTER_le   @INTER_less   @LUB",
  "@SetCompr   @Sigma   @Times   @UNION   @UNION1   @UNION_le   @UNION_less",
  "@andalso   @chg_map   @cifte   @ctuple   @filter   @list   @oo   @orelse",
  "@stuple   ACe   ALL    Abs_CFun   Abs_Integ   Abs_Nat   Abs_Node",
  "Abs_Prod   Abs_Sprod   Abs_Ssum   Abs_Sum   Abs_Up   Abs_bin   Abs_char",
  "Abs_discr   Abs_lift   Abs_list   Abs_nibble   Abs_option   Abs_sumbool",
  "Abs_unit   All   Alm_all   Atom   Ball   Bex   Bit   CFun   CLet   Case",
  "Char   Cletbind   Cletbinds   Collect   Cons   Def   Discr   Domain",
  "EX    EX!    Eps   Ex   Ex1   FF   False   Field   Finites   GREATEST ",
  "Goal   Greatest   GreatestM   ID   INF    INTER   Icifte   Id   If   If2",
  "Ifix   Ifup   Image   In0   In1   Inf_many   Inl   Inl_Rep   Inr",
  "Inr_Rep   Integ   Inter   Ints   Inv   Isfst   Isinl   Isinr   Ispair",
  "Issnd   Istrictify   Iup   Iwhen   LAM    LC   LEAST    LUB    Leaf",
  "Least   LeastM   Left   Let   Lim   MOST    Max   Min   NCons   Nat",
  "Nats   Nibble0   Nibble1   Nibble2   Nibble3   Nibble4   Nibble5",
  "Nibble6   Nibble7   Nibble8   Nibble9   NibbleA   NibbleB   NibbleC",
  "NibbleD   NibbleE   NibbleF   Nil   Node   None   Not   Numb   Numeral0",
  "Numeral1   ONE   Pair   Pair_Rep   Part   Pls   Plus   Pow   Prod   Push",
  "Push_Node   Range   Rep_CFun   Rep_Integ   Rep_Nat   Rep_Node   Rep_Prod",
  "Rep_Sprod   Rep_Ssum   Rep_Sum   Rep_Up   Rep_bin   Rep_char   Rep_discr",
  "Rep_lift   Rep_list   Rep_nibble   Rep_option   Rep_sumbool   Rep_unit",
  "Right   Scons   Sigma   Sinl_Rep   Sinr_Rep   Some   Spair_Rep   Split",
  "Sprod   Ssum   Suc   Suc_Rep   Sum   Suml   Summation   Sumr   TT   TYPE",
  "The   True   Trueprop   UNION   UNIV   UU   Undef   Union   Unity   Up",
  "Zero_Rep   _Ball   _Bex   _CLet   _Cbind   _Cbinds   _Char   _DDDOT",
  "_Eps   _GreatestM   _K   _LAM   _LUpdate   _LeastM   _Let   _Map",
  "_MapUpd   _Maplets   _Numeral   _String   _Summation   _TYPE   _The",
  "_Update   _abs   _appl   _applC   _aprop   _args   _asms   _bigimpl",
  "_bind   _binds   _bracket   _cargs   _case1   _case2   _case_syntax",
  "_classes   _constify   _constrain   _dummy_ofsort   _field   _field_type",
  "_field_types   _fields   _idts   _idtyp   _index   _index1   _indexvar",
  "_lambda   _leAll   _leEx   _lessAll   _lessEx   _lupdbind   _lupdbinds",
  "_maplet   _maplets   _meta_conjunction   _mk_ofclass   _noindex",
  "_not_equal   _ofclass   _ofsort   _pattern   _patterns   _pttrns",
  "_record   _record_scheme   _record_type   _record_type_scheme",
  "_record_update   _reflcl   _setle   _setless   _setprod   _setsum",
  "_sort   _square   _struct   _tapp   _tappl   _topsort   _tuple",
  "_tuple_arg   _tuple_args   _types   _update   _update_name   _updates",
  "_updbind   _updbinds   abelian_group   abelian_group_class",
  "abelian_semigroup   abelian_semigroup_class   abs   acyclic   adjust",
  "adm   adm_wf   admw   all   almost_ordered_semiring",
  "almost_ordered_semiring_class   almost_semiring   almost_semiring_class",
  "antisym   any   apfst   aprop   arbitrary   args   asms   atLeast",
  "atLeastAtMost   atLeastLessThan   atMost   atmost_one   bij   bin",
  "bin_add   bin_case   bin_minus   bin_mult   bin_pred   bin_rec",
  "bin_rec_set   bin_rep_set   bin_succ   binomial   bool   bool_case",
  "bool_rec   bool_rec_set   butlast   card   cardR   cargs   case_syn",
  "cases_syn   cfcomp   cfst   chain   char   char_case   char_rec",
  "char_rec_set   char_rep_set   chfin   chfin_class   chg_map   classes",
  "comp   concat   congruent   congruent2   cont   contents   contlub",
  "converse   cpair   cpo   cpo_class   csnd   csplit   curry   cut   diag",
  "discr   discr_case   discr_rec   discr_rec_set   discr_rep_set",
  "distinct   div   divAlg   div_class   divide   division_by_zero",
  "division_by_zero_class   dom   dprod   drop   dropWhile   dsum   dtree",
  "dummy   dummy_pattern   empty   equiv   even   even_odd   even_odd_class",
  "field   field_class   field_type   field_types   fields   filter",
  "finite   finite_chain   finite_class   finite_psubset   fix   flat",
  "flat_class   flift1   flift2   fold   foldSet   foldl   foldr   fst",
  "fun   fun_upd   fup   gfp   greaterThan   greaterThanAtMost",
  "greaterThanLessThan   hd   id   ident   idt   idts   image   ind   index",
  "induct_conj   induct_equal   induct_forall   induct_implies   infinite",
  "inj   inj_on   insert   int   int_aux   internal_split   intrel   inv",
  "inv_image   inverse   inverse_class   iszero   item   iterate   itself",
  "last   length   lessThan   less_cfun   less_than   letbind   letbinds",
  "lex   lex_prod   lexico   lexn   lfp   lift   lift_case   lift_rec",
  "lift_rec_set   liftpair   linorder   linorder_class   list   list_all",
  "list_all2   list_case   list_rec   list_rec_set   list_rep_set",
  "list_update   lists   logic   logic_class   longid   lub   lupdbind",
  "lupdbinds   map   map_add   map_image   map_le   map_of   map_subst",
  "map_upd_s   map_upds   maplet   maplets   max   max_in_chain   measure",
  "min   minus   minus_class   mono   monofun   myinv   nat   nat_aux",
  "nat_case   nat_rec   nat_rec_set   ndepth   neg   negDivAlg   negateSnd",
  "nibble   nibble_case   nibble_rec   nibble_rec_set   nibble_rep_set",
  "node   nth   ntrunc   null   num   num_const   number   number_class",
  "number_of   number_ring   number_ring_class   o2s   odd   of_int",
  "of_nat   one   one_class   op &   op *   op +   op -   op -->   op :",
  "op <   op <<   op <<|   op <=   op <|   op =   op @   op Int   op Un",
  "op div   op dvd   op mem   op mod   op |   op ~:   option   option_case",
  "option_map   option_rec   option_rec_set   option_rep_set   ord",
  "ord_class   order   order_class   ordered_field   ordered_field_class",
  "ordered_ring   ordered_ring_class   ordered_semiring",
  "ordered_semiring_class   overwrite   patterns   pcpo   pcpo_class   plus",
  "plus_ac0   plus_ac0_class   plus_class   po   po_class   posDivAlg",
  "power   power_class   pred_nat   prod_case   prod_fun   prod_rec",
  "prod_rec_set   product_type   prop   pttrn   pttrns   quorem   quotient",
  "ran   range   refl   reflexive   rel_comp   remdups   replicate",
  "restrict_map   rev   ring   ring_class   ringpower   ringpower_class",
  "rtrancl   same_fst   semiring   semiring_class   set   setprod   setsum",
  "sfst   single_valued   sinl   sinr   size   snd   sort   spair   split",
  "sq_ord   sq_ord_class   sscase   ssnd   ssplit   strictify   string",
  "struct   sublist   sum_case   sum_rec   sum_rec_set   sumbool",
  "sumbool_case   sumbool_rec   sumbool_rec_set   sumbool_rep_set   surj",
  "sym   take   takeWhile   the   tid   times   times_ac1   times_ac1_class",
  "times_class   tl   tord   tr   trancl   trand   trans   tror",
  "tuple_args   tvar   type   type_class   type_definition   types   u",
  "uminus   undiscr   unit   unit_case   unit_rec   unit_rec_set   up",
  "upd_fst   upd_snd   update   updates   updbind   updbinds   uprod   upt",
  "upto   usum   var   vimage   wellorder   wellorder_class   wf   wfrec",
  "wfrec_rel   xnum   xstr   zero   zero_class   zip   {}   ~=>"
  ]}

mainS :: IsaSets
mainS =
  IsaSets {
  types = mkIsaSet
    ["bool Nat Int int partial list unit char rat string option either *"],
  consts = mkIsaSet ["2 3 4 5 6 7 8 9",
  "inject n [__] [__/__] __[__/__] {__} >= > /",
  "!   !!   #   %   &   (   ()   (]   (|   (}   )   *   +   ++   ,",
  "-   -->   -`   .   ..   ...   ..}   /   //   0   1   :   ::   :=   ;   <",
  "<*>   <*lex*>   <+>   <=   =   ==   ==>   =>   ?   ?!   @   ALL   BIT",
  "CHR   EX   EX!   GREATEST   INF   INT   Int   LEAST   MOST   O   OFCLASS",
  "PROP   SIGMA   SOME   THE   TYPE   UN   Un   WRT   [   [\\<mapsto>]   []",
  "[|   [|->]   \\<And>   \\<Colon>   \\<Inter>   \\<Inter>\\<^bsub>",
  "\\<Longrightarrow>   \\<Prod>   \\<Rightarrow>   \\<Sigma>   \\<Sum>",
  "\\<Union>   \\<Union>\\<^bsub>   \\<^esub>   \\<^sub>   \\<^sub>1",
  "\\<^sup>*   \\<^sup>+   \\<^sup>=   \\<and>   \\<bar>   \\<circ>",
  "\\<dots>   \\<epsilon>   \\<equiv>   \\<equiv>\\<^sup>?   \\<exists>",
  "\\<exists>!   \\<exists>\\<^sub>\\<infinity>   \\<forall>",
  "\\<forall>\\<^sub>\\<infinity>   \\<in>   \\<index>   \\<int>",
  "\\<inter>   \\<inverse>   \\<lambda>   \\<lbrakk>   \\<le>   \\<leadsto>",
  "\\<lfloor>   \\<longrightarrow>   \\<lparr>   \\<mapsto>",
  "\\<mapsto>\\<lambda>   \\<nat>   \\<not>   \\<noteq>   \\<notin>",
  "\\<or>   \\<rbrakk>   \\<rightharpoonup>   \\<rparr>   \\<struct>",
  "\\<subset>   \\<subseteq>   \\<subseteq>\\<^sub>m   \\<times>",
  "\\<twosuperior>   \\<union>   ]   ^   ^*   ^+   ^-1   ^=   _   _::   `",
  "`>   ``   case   choose   div   dvd   else   if   in   let   mem   mod",
  "o   of   op   then   {   {)   {..   {\\<mapsto>}   {|->}   {}   |   |)",
  "|->   |]   |_   }   ~   ~:   ~=   ~=>   ~>",

  "!!   #prop   *   +   0   1   ==   ==>   =?=   @Coll   @Finset",
  "@INTER   @INTER1   @INTER_le   @INTER_less   @SetCompr   @Sigma   @Times",
  "@UNION   @UNION1   @UNION_le   @UNION_less   @chg_map   @filter   @list",
  "ACe   ALL    Abs_Integ   Abs_Nat   Abs_Node   Abs_Prod   Abs_Sum",
  "Abs_bin   Abs_char   Abs_list   Abs_nibble   Abs_option   Abs_sumbool",
  "Abs_unit   All   Alm_all   Atom   Ball   Bex   Bit   Case   Char",
  "Collect   Cons   Domain   EX    EX!    Eps   Ex   Ex1   False   Field",
  "Finites   GREATEST    Goal   Greatest   GreatestM   INF    INTER   Id",
  "If   Image   In0   In1   Inf_many   Inl   Inl_Rep   Inr   Inr_Rep",
  "Integ   Inter   Ints   Inv   LC   LEAST    Leaf   Least   LeastM   Left",
  "Let   Lim   MOST    Max   Min   NCons   Nat   Nats   Nibble0   Nibble1",
  "Nibble2   Nibble3   Nibble4   Nibble5   Nibble6   Nibble7   Nibble8",
  "Nibble9   NibbleA   NibbleB   NibbleC   NibbleD   NibbleE   NibbleF",
  "Nil   Node   None   Not   Numb   Numeral0   Numeral1   Pair   Pair_Rep",
  "Part   Pls   Plus   Pow   Prod   Push   Push_Node   Range   Rep_Integ",
  "Rep_Nat   Rep_Node   Rep_Prod   Rep_Sum   Rep_bin   Rep_char   Rep_list",
  "Rep_nibble   Rep_option   Rep_sumbool   Rep_unit   Right   Scons   Sigma",
  "Some   Split   Suc   Suc_Rep   Sum   Suml   Summation   Sumr   TYPE",
  "The   True   Trueprop   UNION   UNIV   Union   Unity   Zero_Rep   _Ball",
  "_Bex   _Char   _DDDOT   _Eps   _GreatestM   _K   _LUpdate   _LeastM",
  "_Let   _Map   _MapUpd   _Maplets   _Numeral   _String   _Summation",
  "_TYPE   _The   _Update   _abs   _appl   _applC   _aprop   _args   _asms",
  "_bigimpl   _bind   _binds   _bracket   _cargs   _case1   _case2",
  "_case_syntax   _classes   _constify   _constrain   _dummy_ofsort",
  "_field   _field_type   _field_types   _fields   _idts   _idtyp   _index",
  "_index1   _indexvar   _lambda   _leAll   _leEx   _lessAll   _lessEx",
  "_lupdbind   _lupdbinds   _maplet   _maplets   _meta_conjunction",
  "_mk_ofclass   _noindex   _not_equal   _ofclass   _ofsort   _pattern",
  "_patterns   _pttrns   _record   _record_scheme   _record_type",
  "_record_type_scheme   _record_update   _reflcl   _setle   _setless",
  "_setprod   _setsum   _sort   _square   _struct   _tapp   _tappl",
  "_topsort   _tuple   _tuple_arg   _tuple_args   _types   _update",
  "_update_name   _updates   _updbind   _updbinds   abelian_group",
  "abelian_group_class   abelian_semigroup   abelian_semigroup_class   abs",
  "acyclic   adjust   adm_wf   all   almost_ordered_semiring",
  "almost_ordered_semiring_class   almost_semiring   almost_semiring_class",
  "antisym   any   apfst   aprop   arbitrary   args   asms   atLeast",
  "atLeastAtMost   atLeastLessThan   atMost   atmost_one   bij   bin",
  "bin_add   bin_case   bin_minus   bin_mult   bin_pred   bin_rec",
  "bin_rec_set   bin_rep_set   bin_succ   binomial   bool   bool_case",
  "bool_rec   bool_rec_set   butlast   card   cardR   cargs   case_syn",
  "cases_syn   char   char_case   char_rec   char_rec_set   char_rep_set",
  "chg_map   classes   comp   concat   congruent   congruent2   contents",
  "converse   curry   cut   diag   distinct   div   divAlg   div_class",
  "divide   division_by_zero   division_by_zero_class   dom   dprod   drop",
  "dropWhile   dsum   dtree   dummy   dummy_pattern   empty   equiv   even",
  "even_odd   even_odd_class   field   field_class   field_type",
  "field_types   fields   filter   finite   finite_class   finite_psubset",
  "fold   foldSet   foldl   foldr   fst   fun   fun_upd   gfp   greaterThan",
  "greaterThanAtMost   greaterThanLessThan   hd   id   ident   idt   idts",
  "image   ind   index   induct_conj   induct_equal   induct_forall",
  "induct_implies   infinite   inj   inj_on   insert   int   int_aux",
  "internal_split   intrel   inv   inv_image   inverse   inverse_class",
  "iszero   item   itself   last   length   lessThan   less_than   letbind",
  "letbinds   lex   lex_prod   lexico   lexn   lfp   linorder",
  "linorder_class   list   list_all   list_all2   list_case   list_rec",
  "list_rec_set   list_rep_set   list_update   lists   logic   logic_class",
  "longid   lupdbind   lupdbinds   map   map_add   map_image   map_le",
  "map_of   map_subst   map_upd_s   map_upds   maplet   maplets   max",
  "measure   min   minus   minus_class   mono   myinv   nat   nat_aux",
  "nat_case   nat_rec   nat_rec_set   ndepth   neg   negDivAlg   negateSnd",
  "nibble   nibble_case   nibble_rec   nibble_rec_set   nibble_rep_set",
  "node   nth   ntrunc   null   num   num_const   number   number_class",
  "number_of   number_ring   number_ring_class   o2s   odd   of_int",
  "of_nat   one   one_class   op &   op *   op +   op -   op -->   op :",
  "op <   op <=   op =   op @   op Int   op Un   op div   op dvd   op mem",
  "op mod   op |   op ~:   option   option_case   option_map   option_rec",
  "option_rec_set   option_rep_set   ord   ord_class   order   order_class",
  "ordered_field   ordered_field_class   ordered_ring   ordered_ring_class",
  "ordered_semiring   ordered_semiring_class   overwrite   patterns   plus",
  "plus_ac0   plus_ac0_class   plus_class   posDivAlg   power   power_class",
  "pred_nat   prod_case   prod_fun   prod_rec   prod_rec_set   product_type",
  "prop   pttrn   pttrns   quorem   quotient   ran   range   refl",
  "reflexive   rel_comp   remdups   replicate   restrict_map   rev   ring",
  "ring_class   ringpower   ringpower_class   rtrancl   same_fst   semiring",
  "semiring_class   set   setprod   setsum   single_valued   size   snd",
  "sort   split   string   struct   sublist   sum_case   sum_rec",
  "sum_rec_set   sumbool   sumbool_case   sumbool_rec   sumbool_rec_set",
  "sumbool_rep_set   surj   sym   take   takeWhile   the   tid   times",
  "times_ac1   times_ac1_class   times_class   tl   trancl   trans",
  "tuple_args   tvar   type   type_class   type_definition   types   uminus",
  "unit   unit_case   unit_rec   unit_rec_set   upd_fst   upd_snd   update",
  "updates   updbind   updbinds   uprod   upt   upto   usum   var   vimage",
  "wellorder   wellorder_class   wf   wfrec   wfrec_rel   xnum   xstr",
  "zero   zero_class   zip   {}   ~=>"
  ]}
