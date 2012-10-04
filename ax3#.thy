theory ax3#
imports Main
uses "$HETS_ISABELLE_LIB/prelude.ML"
begin

setup "Header.initialize [\"Ax2\"]"

typedecl World

consts
X_Acc_R :: "World => World => bool" ("Acc'_R/'(_,/ _')" [3,3] 999)
X_Wrl_N :: "World" ("Wrl'_N")
X_p :: "World => bool" ("p/'(_')" [3] 999)

theorem Ax2 :
"(ALL v. Acc_R(Wrl_N, v) --> p(v)) -->
 (ALL v. Acc_R(Wrl_N, v) --> (EX Wrl_XX. v = Wrl_XX & p(Wrl_XX)))"
by (auto)

setup "Header.record \"Ax2\""

end
