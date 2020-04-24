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

(* Definition compose {A B C} (f : B -> C) (g : A -> B) :=
  fun x => f (g x). *)

(* Notation "f ∘ g" := (compose f g) (at level 0). *)

Notation "f ∘ g" := (fun x => f (g x)) (at level 20).

Definition id {A} (x : A) : A := x.

Definition Con := Type.

(* Substitutions Γ → Δ *)
Definition Subs (Γ Δ : Con) := Γ -> Δ.

Definition Ty (Γ : Con) := Γ -> Type.

Definition Tm (Γ : Con) (A : Ty Γ) := forall γ : Γ, A γ.

Definition subsTy {Γ Δ} (σ : Subs Γ Δ) (A : Ty Δ) : Ty Γ :=
  fun γ => A (σ γ).

Notation "A [ σ ]" := (subsTy σ A) (at level 0).

Definition subsTm {Γ Δ} (σ : Subs Γ Δ) {A} (t : Tm Δ A)
  : Tm Γ A[σ]
  := fun γ => t (σ γ).

Notation "t [ σ ]ᵗ" := (subsTm σ t) (at level 0).

Definition empty : Con :=
  unit.

Definition cons (Γ : Con) (A : Ty Γ) : Con :=
  ∑ (γ : Γ), A γ.

Notation "Γ , A" := (cons Γ A) (at level 20).

Definition p {Γ : Con} {A : Ty Γ} : Subs (Γ, A) Γ :=
  fun '(γ;a) => γ.

Definition q {Γ : Con} {A : Ty Γ} : Tm (Γ, A) A[p] :=
  fun '(γ;a) => a.

Definition scons {Γ Δ} (σ : Subs Γ Δ) {A} (a : Tm Γ A[σ])
  : Subs Γ (Δ, A)
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

(* It supporst Π-types *)
Definition Π {Γ} (A : Ty Γ) (B : Ty (Γ, A)) : Ty Γ :=
  fun γ => forall (x : A γ), B (γ ; x).

Definition λ {Γ A B} (b : Tm (Γ, A) B) : Tm Γ (Π A B) :=
  fun γ => fun (x : A γ) => b (γ ; x).

Definition app {Γ A B} (f : Tm Γ (Π A B)) (u : Tm Γ A) : Tm Γ B[scons id u] :=
  fun γ => f γ (u γ).

Lemma Πsubs :
  forall Γ Δ (σ : Subs Γ Δ) (A : Ty Δ) (B : Ty (Δ, A)),
    (Π A B)[σ] = Π A[σ] B[scons (Γ := Γ,A[σ]) (σ ∘ p) q].
Proof.
  reflexivity.
Qed.

Lemma λsubs :
  forall Γ Δ (σ : Subs Γ Δ) A B (b : Tm (Δ, A) B),
    (λ b)[σ]ᵗ = λ (b[scons (Γ := Γ,A[σ]) (σ ∘ p) q]ᵗ).
Proof.
  reflexivity.
Qed.

Lemma app_subs :
  forall Γ Δ (σ : Subs Γ Δ) A B (f : Tm Δ (Π A B)) (u : Tm Δ A),
    (app f u)[σ]ᵗ = app (B := B[scons (Γ := Γ,A[σ]) (σ ∘ p) q]) f[σ]ᵗ u[σ]ᵗ.
Proof.
  reflexivity.
Qed.

Lemma beta :
  forall Γ A B (b : Tm (Γ, A) B) (u : Tm Γ A),
    app (λ b) u = b[scons id u]ᵗ.
Proof.
  reflexivity.
Qed.

Lemma eta :
  forall Γ A B (f : Tm Γ (Π A B)),
    λ (app (B := B[scons (Γ := (Γ,A),A[p]) (p ∘ p) q]) f[p]ᵗ q) = f.
Proof.
  reflexivity.
Qed.

(* It supports a universe *)
Definition U {Γ} : Ty Γ :=
  fun γ => Type.

Definition El {Γ} (a : Tm Γ U) : Ty Γ :=
  fun γ => a γ.

Definition π {Γ} (a : Tm Γ U) (b : Tm Γ (Π (El a) U)) : Tm Γ U :=
  fun γ => forall (x : a γ), (b γ x).

Lemma Usubs :
  forall Γ Δ (σ : Subs Γ Δ),
    U[σ] = U.
Proof.
  reflexivity.
Qed.

Lemma Elsubs :
  forall Γ Δ (σ : Subs Γ Δ) (a : Tm Δ U),
    (El a)[σ] = El a[σ]ᵗ.
Proof.
  reflexivity.
Qed.

Lemma Elπ :
  forall Γ (a : Tm Γ U) (b : Tm Γ (Π (El a) U)),
    El (π a b) = Π (El a) (El (app (B := U) b[p]ᵗ q)).
Proof.
  reflexivity.
Qed.