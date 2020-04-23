Set Primitive Projections.

Notation "'∑' x .. y , p" := (sigT (fun x => .. (sigT (fun y => p%type)) ..))
  (at level 200, x binder, right associativity,
   format "'[' '∑'  '/  ' x  ..  y ,  '/  ' p ']'")
  : type_scope.

Notation "( x ; y )" := (@existT _ _ x y).
Notation "( x ; y ; z )" := (x ; ( y ; z)).
Notation "( x ; y ; z ; t )" := (x ; ( y ; (z ; t))).
Notation "( x ; y ; z ; t ; u )" := (x ; ( y ; (z ; (t ; u)))).
Notation "( x ; y ; z ; t ; u ; v )" := (x ; ( y ; (z ; (t ; (u ; v))))).
Notation "x .π1" := (@projT1 _ _ x) (at level 3, format "x '.π1'").
Notation "x .π2" := (@projT2 _ _ x) (at level 3, format "x '.π2'").

Definition compose {A B C} (f : B -> C) (g : A -> B) :=
  fun x => f (g x).

Notation "f ∘ g" := (compose f g) (at level 0).

Definition Con := Type.

(* Substitutions Γ → Δ *)
Definition Subs (Γ Δ : Con) := Γ -> Δ.

Definition Ty (Γ : Con) := Γ -> Type.

Definition Tm (Γ : Con) (A : Ty Γ) := forall γ : Γ, A γ.

Definition subsTy {Γ Δ} (σ : Subs Γ Δ) (A : Ty Δ) : Ty Γ :=
  fun γ => A (σ γ).

Notation "A [ σ ]" :=
  (subsTy σ A)
  (at level 0).

Definition subsTm {Γ Δ} (σ : Subs Γ Δ) {A} (t : Tm Δ A)
  : Tm Γ A[σ]
  := fun γ => t (σ γ).

Definition empty : Con :=
  unit.

Definition cons (Γ : Con) (A : Ty Γ) : Con :=
  ∑ (γ : Γ), A γ.

Definition p {Γ : Con} {A : Ty Γ} : Subs (cons Γ A) Γ :=
  fun '(γ;a) => γ.

Definition q {Γ : Con} {A : Ty Γ} : Tm (cons Γ A) A[p] :=
  fun '(γ;a) => a.

Definition scons {Γ Δ} (σ : Subs Γ Δ) {A} (a : Tm Γ A[σ])
  : Subs Γ (cons Δ A)
  := fun γ => (σ γ ; a γ).

Lemma p_scons :
  forall Γ Δ (σ : Subs Γ Δ) A (a : Tm Γ A[σ]),
    p ∘ (scons σ a) = σ.
Proof.
  reflexivity.
Qed.

Lemma q_scons :
  forall Γ Δ (σ : Subs Γ Δ) A (a : Tm Γ A[σ]),
    subsTm (scons σ a) q = a.
Proof.
  reflexivity.
Qed.