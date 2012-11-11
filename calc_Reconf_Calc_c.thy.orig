theory calc_Reconf_Calc_c
imports "$HETS_ISABELLE_LIB/MainHC"
uses "$HETS_ISABELLE_LIB/prelude.ML"
begin

setup "Header.initialize [\"inconsistent\"]"

typedecl World
typedecl X_Nat

consts
Acc_Shift :: "World * World => bool"
Wrl_Mul :: "World"
Wrl_Sum :: "World"
c :: "World => X_Nat"
pre :: "World * X_Nat => X_Nat"
suc :: "World * X_Nat => X_Nat"
x_op :: "(World * X_Nat) * X_Nat => X_Nat"



lemma inconsistent : "~
                      ((ALL w.
                        ALL X_n.
                        apt (Some pre)
                        (pair (Some w)
                         (apt (Some suc) (pair (Some w) (Some X_n)))) =
                        Some X_n) &
                       (ALL w.
                        ALL X_n.
                        ALL k.
                        apt (Some x_op)
                        (pair (pair (Some w) (Some X_n)) (Some k)) =
                        apt (Some x_op)
                        (pair (pair (Some w) (Some k)) (Some X_n))) &
                       (ALL w.
                        ALL X_n.
                        ALL k.
                        ALL l.
                        apt (Some x_op)
                        (pair (pair (Some w) (Some X_n))
                         (apt (Some x_op)
                          (pair (pair (Some w) (Some k)) (Some l)))) =
                        apt (Some x_op)
                        (pair
                         (pair (Some w)
                          (apt (Some x_op)
                           (pair (pair (Some w) (Some X_n)) (Some k))))
                         (Some l))) &
                       (ALL w.
                        ALL X_n.
                        apt (Some x_op)
                        (pair (pair (Some Wrl_Sum) (Some X_n))
                         (apt (Some c) (Some Wrl_Sum))) =
                        Some X_n) &
                       (ALL w.
                        ALL X_n.
                        apt (Some suc) (pair (Some Wrl_Sum) (Some X_n)) =
                        apt (Some x_op)
                        (pair (pair (Some Wrl_Sum) (Some X_n))
                         (apt (Some suc)
                          (pair (Some Wrl_Sum)
                           (apt (Some c) (Some Wrl_Sum)))))) &
                       (ALL w.
                        ALL X_n.
                        apt (Some x_op)
                        (pair (pair (Some Wrl_Mul) (Some X_n))
                         (apt (Some c) (Some Wrl_Mul))) =
                        apt (Some c) (Some Wrl_Mul)) &
                       (ALL w.
                        ALL X_n.
                        apt (Some x_op)
                        (pair (pair (Some Wrl_Mul) (Some X_n))
                         (apt (Some suc)
                          (pair (Some Wrl_Mul)
                           (apt (Some c) (Some Wrl_Mul))))) =
                        Some X_n) &
                       (ALL w.
                        ALL X_n.
                        apt (Some x_op)
                        (pair (pair (Some w) (Some X_n))
                         (apt (Some c) (Some w))) =
                        Some X_n -->
                        (ALL v0.
                         pApp (Some Acc_Shift) (pair (Some w) (Some v0)) -->
                         apt (Some x_op)
                         (pair (pair (Some v0) (Some X_n))
                          (apt (Some c) (Some v0))) =
                         apt (Some c) (Some v0))) &
                       (ALL w.
                        ~
                        (ALL v0.
                         pApp (Some Acc_Shift)
                         (pair (Some Wrl_Sum) (Some v0)) -->
                         ~ Some Wrl_Mul = Some v0)) &
                       (ALL w.
                        ~
                        (ALL v0.
                         pApp (Some Acc_Shift)
                         (pair (Some Wrl_Mul) (Some v0)) -->
                         ~ Some Wrl_Sum = Some v0)) &
                       (ALL w.
                        ALL X_n.
                        apt (Some pre)
                        (pair (Some w)
                         (apt (Some pre)
                          (pair (Some w)
                           (apt (Some suc)
                            (pair (Some w)
                             (apt (Some suc) (pair (Some w) (Some X_n)))))))) =
                        Some X_n) &
                       (ALL w.
                        ALL X_n.
                        ALL k.
                        EX l.
                        EX m.
                        apt (Some x_op)
                        (pair
                         (pair (Some Wrl_Mul)
                          (apt (Some suc) (pair (Some Wrl_Mul) (Some X_n))))
                         (Some k)) =
                        Some l &
                        apt (Some x_op)
                        (pair (pair (Some Wrl_Mul) (Some X_n)) (Some k)) =
                        Some m &
                        apt (Some x_op)
                        (pair
                         (pair (Some Wrl_Sum)
                          (apt (Some suc) (pair (Some Wrl_Sum) (Some X_n))))
                         (Some m)) =
                        Some l) &
                       (ALL w.
                        ALL X_n.
                        ALL k.
                        EX l.
                        EX m.
                        apt (Some x_op)
                        (pair
                         (pair (Some Wrl_Mul)
                          (apt (Some pre) (pair (Some Wrl_Mul) (Some X_n))))
                         (Some k)) =
                        Some l &
                        apt (Some x_op)
                        (pair (pair (Some Wrl_Mul) (Some X_n)) (Some k)) =
                        Some m &
                        apt (Some x_op)
                        (pair
                         (pair (Some Wrl_Sum)
                          (apt (Some pre) (pair (Some Wrl_Sum) (Some X_n))))
                         (Some m)) =
                        Some l) &
                       (ALL w.
                        ALL X_n.
                        ALL k.
                        apt (Some suc) (pair (Some w) (Some X_n)) = Some k -->
                        (ALL v0.
                         pApp (Some Acc_Shift) (pair (Some w) (Some v0)) -->
                         apt (Some pre) (pair (Some v0) (Some k)) = Some X_n)) &
                       (ALL w.
                        ALL X_n.
                        EX l.
                        apt (Some x_op)
                        (pair (pair (Some Wrl_Sum) (Some X_n))
                         (apt (Some c) (Some Wrl_Sum))) =
                        Some l &
                        apt (Some x_op)
                        (pair (pair (Some Wrl_Mul) (Some X_n))
                         (apt (Some suc)
                          (pair (Some Wrl_Mul)
                           (apt (Some c) (Some Wrl_Mul))))) =
                        Some l) &
                       (ALL w.
                        ALL X_n.
                        EX l.
                        apt (Some x_op)
                        (pair (pair (Some Wrl_Sum) (Some X_n)) (Some X_n)) =
                        Some l &
                        apt (Some x_op)
                        (pair
                         (pair (Some Wrl_Mul)
                          (apt (Some suc)
                           (pair (Some Wrl_Mul)
                            (apt (Some suc)
                             (pair (Some Wrl_Mul)
                              (apt (Some c) (Some Wrl_Mul)))))))
                         (Some X_n)) =
                        Some l))"
refute

setup "Header.record \"inconsistent\""

end
