(* open Syntax *)

type name = Syntax.name

type expr =
  | PVar of name           (** variable *)
  | PInt of int            (** integer constant *)
  | PBool of bool          (** boolean constant *)
  | PTimes of expr * expr  (** product [e1 * e2] *)
  | PDivide of expr * expr (** quotient [e1 / e2] *)
  | PMod of expr * expr    (** remainder [e1 % e2] *)
  | PPlus of expr * expr   (** sum [e1 + e2] *)
  | PMinus of expr * expr  (** difference [e1 - e2] *)
  | PEqual of expr * expr
  | PLess of expr * expr

  | PPair of expr * expr   (** pair e1⊗e2  [(e1, e2)]*)
  | PSplit of expr * name * name * expr  (** Applies e1 and e2 to f, where e = (e_1, e2). [split e to e1 e2 in f(e1, e2)]*)

  | PFun of Syntax.name * expr (** linear function [fun x:t ⊸ e] *)
  | PApply of expr * expr  (** linear aplication [e1 e2]*)

  | PInl of expr
  | PInr of expr
  | PMatch of expr * name * expr * name * expr

  | PBundle of expr * expr
  | PFst of expr
  | PSnd of expr

type atom_expr =
  | ALRparen of expr
  | APair of expr * expr
  | AVar of name
  | ABool of bool


type app_expr = 
  | Atom
  | App of app_expr

(* 
let print_atom_expr = function
  | ALRparen e -> "(" ^ (print_expr e) ^ ")" *)
