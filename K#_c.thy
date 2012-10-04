theory K#_c
imports Main
uses "$HETS_ISABELLE_LIB/prelude.ML"
begin

setup "Header.initialize [\"inconsistent\"]"

typedecl World

consts
X_Acc_empty :: "World => World => bool" ("Acc'_empty/'(_,/ _')" [3,3] 999)

lemma inconsistent : "~ True"
refute

setup "Header.record \"inconsistent\""

end
