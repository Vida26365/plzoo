(** Abstract syntax *)

(** The type of variable names. *)
type name = string

(* Linear types *)

type ltype = 
  | LInt (** integer *)
  | LEphBool
  | LPerBool
  | LLoli of ltype * ltype
  | LAnd of ltype * ltype
  | LWith of ltype * ltype
  | LPlus of ltype * ltype
  | LPar of ltype * ltype


type expr =
  | Var of name           (** variable *)
  | Int of int            (** integer constant *)
  | Times of expr * expr  (** product [e1 * e2] *)
  | Divide of expr * expr (** quotient [e1 / e2] *)
  | Mod of expr * expr    (** remainder [e1 % e2] *)
  | Plus of expr * expr   (** sum [e1 + e2] *)
  | Minus of expr * expr  (** difference [e1 - e2] *)
  | Equal of expr * expr
  | Less of expr * expr
  | And of expr * expr    (** multiplicative conjunction [e1 ⊗ e2] *)
  | With of expr * expr   (** additive conjunction [e1 & e2] *)
  | OPlus of expr * expr  (** additive disjunction [e1 ⊕ e2] *)
  | Par of expr * expr    (** multiplicative disjunction [e1 ⅋ e2] *)
  | Loli of expr * expr   (** linear implication [e1 ⊸ e2] *)
  | Top                   (** top Τ *)
  | Bot                   (** bottom 𝈜 *)
  | Unit                  (** unit 1 *)
  | Zero                  (** zero 0 *)


(** Toplevel commands *)
type toplevel_cmd =
  | Expr of expr       (** an expression to be evaluated *)
  | Def of name * expr (** toplevel definition [let x = e] *)
  | Quit               (** exit toplevel [$quit] *)



(** Conversion from an expression to a string *)
let string_of_expression (e : expr) : string =
  let rec to_str (n:int) (e : expr)= (* we need n, to know where should we put brackets *)
    let (m, str) = match e with
      | (Var x) -> (10, x);
      | Int x ->  (10, string_of_int x)
      | Times (a, b) -> (8, (to_str 7 a) ^" * " ^ (to_str 8 b))
      | Divide (a, b) -> (8, (to_str 7 a) ^" / " ^ (to_str 8 b))
      | Mod (e1, e2) ->    (8, (to_str 7 e1) ^ " % " ^ (to_str 8 e2))
      | Plus (e1, e2) ->   (7, (to_str 6 e1) ^ " + " ^ (to_str 7 e2))
      | Minus (e1, e2) ->  (7, (to_str 6 e1) ^ " - " ^ (to_str 7 e2))
      | Equal (e1, e2) ->  (5, (to_str 5 e1) ^ " = " ^ (to_str 5 e2))
	    | Less (e1, e2) ->   (5, (to_str 5 e1) ^ " < " ^ (to_str 5 e2))
      | And (a, b) -> (16, "(" ^(to_str 15 a) ^ ", " ^ (to_str 15 b) ^ ")" )
      | With (a, b) -> (15, (to_str 15 a) ^ " & " ^ (to_str 15 b))
      | OPlus (a, b) -> (15, (to_str 15 a) ^ " ⊕ " ^ (to_str 15 b))
      | Par (a, b) -> (15, (to_str 15 a) ^ " ⅋ " ^ (to_str 15 b))
      | Top -> (15, "Τ")
      | Bot -> (15, "𝈜")
      | Unit -> (16, "1")
      | Zero -> (16, "0")
      | Loli (a, b) -> (17, (to_str 16 a) ^ " ⊸ " ^ (to_str 16 b))
    in
    if m > n then str else "(" ^ str ^ ")"
  in
  to_str (-1) e


(** Conversion from a type to a string *)
let string_of_type (ty : ltype) : string =
  let rec to_str n ty =
    let (m, str) =
      match ty with
      | LInt -> (4, "int")
      | LEphBool -> (4, "ephemeral bool")
      | LPerBool -> (4, "persistant bool")
      | LLoli (ta, tb) -> (1, (to_str 1 ta) ^ " ⊸ " ^ (to_str 0 tb))
      | LAnd (ty1, ty2) -> (3, (to_str 3 ty1) ^ " ⊗ " ^ (to_str 3 ty2))
      | LWith (ta, tb) -> (3, (to_str 3 ta) ^ " & " ^ (to_str 3 tb))
      | LPlus (ta, tb) -> (2, (to_str 2 ta) ^ " ⊕ " ^ (to_str 2 tb))
      | LPar (ta, tb) -> (2, (to_str 2 ta) ^ " ⅋ " ^ (to_str 2 tb))
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) ty


    (** [subst [(x1,e1);...;(xn;en)] e] replaces in [e] free occurrences
    of variables [x1], ..., [xn] with expressions [e1], ..., [en]. *)

let rec subst s = function
  |  (Var x) as e -> (try List.assoc x s with Not_found -> e)
  | (Int _ ) as e -> e
  | Times (e1, e2) -> Times (subst s e1, subst s e2)
  | Divide (e1, e2) -> Divide (subst s e1, subst s e2)
  | Mod (e1, e2) -> Mod (subst s e1, subst s e2)
  | Plus (e1, e2) -> Plus (subst s e1, subst s e2)
  | Minus (e1, e2) -> Minus (subst s e1, subst s e2)
  | Equal (e1, e2) -> Equal (subst s e1, subst s e2)
  | Less (e1, e2) -> Less (subst s e1, subst s e2)
  | And (a, b) -> And (subst s a, subst s b)
  | With (a, b) -> With (subst s a, subst s b)
  | OPlus (a, b) -> OPlus (subst s a, subst s b)
  | Par (a, b) -> Par (subst s a, subst s b)
  | Loli (a, b) -> Loli (subst s a, subst s b)
  | (Top | Bot | Zero | Unit) as e -> e





