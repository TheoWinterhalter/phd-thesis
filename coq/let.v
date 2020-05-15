Section Let.

  Context (u : nat).
  Context (f : u = u -> nat).

  Definition foo :=
    let x := u in f (eq_refl x).

  (** The term "eq_refl" has type "x = x" while it is expected
      to have type "u = u" (cannot unify "x" and "u").
  *)
  Fail Definition bar :=
    (fun x => f (eq_refl x)) u.

End Let.