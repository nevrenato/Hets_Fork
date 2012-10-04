theory Nomies#_c
imports Main
uses "$HETS_ISABELLE_LIB/prelude.ML"
begin

setup "Header.initialize [\"inconsistent\"]"

typedecl World

consts
X_Wrl_FIFO :: "World" ("Wrl'_FIFO")

lemma inconsistent : "~
                      ((ALL w. X_Wrl_FIFO = w) &
                       (X_Wrl_FIFO = X_Wrl_FIFO --> True) &
                       X_Wrl_FIFO = X_Wrl_FIFO & ~ X_Wrl_FIFO = X_Wrl_FIFO)"
refute

setup "Header.record \"inconsistent\""

end
