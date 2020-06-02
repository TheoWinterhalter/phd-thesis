Set Default Goal Selector "!".

Fail Fixpoint fib n :=
  match n with
  | 0 => 1
  | 1 => 1
  | S (S n) => (fib (S n)) + (fib n)
  end.

(* Fixpoint fib n :=
  match n with
  | 0 => 1
  | 1 => 1
  | S ((S n) as m) => fib m + fib n
  end. *)

Require Import Lia Arith.

Lemma wf_nat :
  well_founded lt.
Proof.
  intro n. induction n as [| n ih].
  - constructor. intros m h. lia.
  - constructor. intros m h.
    destruct (eq_nat_dec m n).
    + subst. assumption.
    + destruct ih as [ih]. apply ih. lia.
Qed.

(* Definition fib n :=
  Acc_rec (fun _ => nat)
    (fun n =>
      match n with
      | 0 => fun _ _ => 1
      | 1 => fun _ _ => 1
      | S (S m) =>
        fun acc_lt_n fib_lt_n =>
          fib_lt_n (S m) (Nat.lt_succ_diag_r (S m)) +
          fib_lt_n m
            (Nat.lt_trans _ _ _
              (Nat.lt_succ_diag_r m)
              (Nat.lt_succ_diag_r (S m)))
      end
    )
    (wf_nat n). *)

From Equations Require Equations.

Equations fib (n : nat) : nat
  by wf n lt :=

  fib 0 := 1 ;
  fib 1 := 1 ;
  fib (S (S n)) := fib (S n) + fib n.

Import Subterm.

(* Ackermann *)
Equations ack (x : nat * nat) : nat
  by wf x (lexprod _ _ lt lt) :=

  ack (0,   n)   := S n ;
  ack (S m, 0)   := ack (m, 1) ;
  ack (S m, S n) := ack (m, ack (S m, n)).

(* Simple lexicographic order modulo another relation *)
Inductive lexmod {A B}
    (leA : A -> A -> Prop)
    (eA : A -> A -> Prop)
    (leB : B -> B -> Prop)
  : A * B -> A * B -> Prop :=
| left_lexmod :
    forall x x' y y',
      leA x x' ->
      lexmod leA eA leB (x,y) (x',y')

| right_lexmod :
    forall x x' y y',
      eA x x' ->
      leB y y' ->
      lexmod leA eA leB (x,y) (x',y').

Notation "e \ R1 ⊕ R2" :=
  (lexmod R1 e R2) (at level 20, right associativity).

Require Import Equations.Prop.DepElim.
Derive Signature for lexmod.

Open Scope type_scope.

Lemma acc_lexmod :
  forall A B (leA : A -> A -> Prop) (eA : A -> A -> Prop)
    (leB : B -> B -> Prop),
    well_founded leB ->
    (forall x x' y,
      eA x x' ->
      leA y x' ->
      exists y',
        leA y' x *
        eA y' y
    ) ->
    (forall a b c, eA a c -> eA b c -> eA a b) ->
    forall x,
      Acc leA x ->
      forall y,
        Acc leB y ->
        forall x' (e : eA x x'),
          Acc (eA \ leA ⊕ leB) (x', y).
Proof.
  intros A B leA eA leB hw hsim htrans.
  induction 1 as [x hx ih1].
  induction 1 as [y hy ih2].
  intros x' e.
  constructor.
  intros [x'' y''] h. dependent destruction h.
  - specialize hsim with (1 := e) (2 := ltac:(eauto)) as [z [hz ez]].
    eapply ih1.
    + eassumption.
    + apply hw.
    + assumption.
  - eapply ih2. 1: auto.
    eapply htrans. all: eauto.
Qed.

Lemma lexmod_Acc :
  forall A B (leA : A -> A -> Prop) (eA : A -> A -> Prop)
    (leB : B -> B -> Prop),
    well_founded leB ->
    (forall x x' y,
      eA x x' ->
      leA y x' ->
      exists y',
        leA y' x *
        eA y' y
    ) ->
    (forall a, eA a a) ->
    (forall a b c, eA a c -> eA b c -> eA a b) ->
    forall x y,
      Acc leA x ->
      Acc (eA \ leA ⊕ leB) (x, y).
Proof.
  intros A B leA eA leB hw hsim hrefl htrans x y h.
  eapply acc_lexmod. all: eauto.
Qed.