(** Abstract syntax *)

(** The type of variable names. *)
type name = string

(** Linear types *)
type ltype =
  | LInt (** integer *)
  | LBool (** boolean *)
  | LLolli of ltype * ltype (** linear function [s ⊸ t] *)
  | LAnd of ltype * ltype (** tensor (multiplicative conjunction) [s ⊗ t] *)
  | LWith of ltype * ltype (** with (additive conjunction) [s & t] *)
  | LPlus of ltype * ltype (** plus (additive disjunction) [s ⊕ t] *)
  (* | LPar of ltype * ltype *)


type expr =
  | Var of name           (** variable *)
  | Int of int            (** integer constant *)
  | Bool of bool          (** boolean constant *)
  | Times of expr * expr  (** product [e1 * e2] *)
  | Divide of expr * expr (** quotient [e1 / e2] *)
  | Mod of expr * expr    (** remainder [e1 % e2] *)
  | Plus of expr * expr   (** sum [e1 + e2] *)
  | Minus of expr * expr  (** difference [e1 - e2] *)
  | Equal of expr * expr
  | Less of expr * expr

  | If of expr * expr * expr (** conditional [if e1 then e2 else e3] *)

  | Pair of expr * expr   (** pair e1⊗e2  [(e1, e2)]*)
  | Split of expr * name * name * expr  (** Applies e1 and e2 to f, where e = (e_1, e2). [split e to e1 e2 in f(e1, e2)]*)

  | Fun of name * ltype * expr (** linear function [lambda x : t in e] *)
  | Apply of expr * expr  (** linear aplication [e1 e2]*)

  | Inl of expr
  | Inr of expr
  | Match of expr * name * expr * name * expr

  | Bundle of expr * expr
  | Fst of expr
  | Snd of expr



(** Toplevel commands *)
type toplevel_cmd =
  | Expr of expr       (** an expression to be evaluated *)
  | Def of name * expr (** toplevel definition [let x = e] *)
  | Quit               (* exit toplevel [$quit] *)

