open Syntax

type environment = (name * value ref) list

and value =
  | VInt of int
  | VBool of bool
  | VPair of value * value
  | VPlus of value * value
  | VWith of value * value
  | VClosure of environment * expr



exception Runtime_error of string




(* type atomar =
  | AtInt of int
  | AtBool of bool
  | AtExpr of expr

type app =
  | ApAtom of atomar
  | ApAppAtom of app * atomar 


type expression =
  | ExAtom of atomar
  | ExApp of app


let rec expr_to_expression = function
  | Var name -> ExAtom (AtExpr (Var name))
  | Int value -> ExAtom (AtInt value)
  | Bool value -> ExAtom (AtBool value)
  | Times (left, right) ->
    ExApp (ApAppAtom (ApAtom (AtExpr left), AtExpr right))
  | Divide (left, right) ->
    ExApp (ApAppAtom (ApAtom (AtExpr left), AtExpr right))
  | Mod (left, right) ->
    ExApp (ApAppAtom (ApAtom (AtExpr left), AtExpr right))
  | Plus (left, right) ->
    ExApp (ApAppAtom (ApAtom (AtExpr left), AtExpr right))
  | Minus (left, right) ->
    ExApp (ApAppAtom (ApAtom (AtExpr left), AtExpr right))
  | Equal (left, right) ->
    ExApp (ApAppAtom (ApAtom (AtExpr left), AtExpr right))
  | Less (left, right) ->
    ExApp (ApAppAtom (ApAtom (AtExpr left), AtExpr right)) *)



(* let rec interp (env : environment) = function
  | Var x -> (
    try
      let r = List.assoc x env in 
        match !r with 
          |  
  )
*)

(* 
let runtime_error message = raise (Runtime_error message)

let rec interp (env : environment) = function
  | Var name ->
    (try
     match !(List.assoc name env) with
     | VClosure (closure_env, expression) -> interp closure_env expression
     | value -> value
     with Not_found -> runtime_error ("Unknown variable " ^ name))
  | Int value -> VInt value
  | Times (left, right) ->
    (match interp env left, interp env right with
     | VInt left_value, VInt right_value -> VInt (left_value * right_value)
     | _ -> runtime_error "Integers expected in multiplication")
  | Divide (left, right) ->
    (match interp env left, interp env right with
     | VInt _, VInt 0 -> runtime_error "Division by 0"
     | VInt left_value, VInt right_value -> VInt (left_value / right_value)
     | _ -> runtime_error "Integers expected in division")
  | Mod (left, right) ->
    (match interp env left, interp env right with
     | VInt _, VInt 0 -> runtime_error "Division by 0"
     | VInt left_value, VInt right_value -> VInt (left_value mod right_value)
     | _ -> runtime_error "Integers expected in remainder")
  | Plus (left, right) ->
    (match interp env left, interp env right with
     | VInt left_value, VInt right_value -> VInt (left_value + right_value)
     | _ -> runtime_error "Integers expected in addition")
  | Minus (left, right) ->
    (match interp env left, interp env right with
     | VInt left_value, VInt right_value -> VInt (left_value - right_value)
     | _ -> runtime_error "Integers expected in subtraction")
  | Equal (left, right) ->
    (match interp env left, interp env right with
     | VInt left_value, VInt right_value -> VBool (left_value = right_value)
     | _ -> runtime_error "Integers expected in =")
  | Less (left, right) ->
    (match interp env left, interp env right with
     | VInt left_value, VInt right_value -> VBool (left_value < right_value)
     | _ -> runtime_error "Integers expected in <")
  | Pair (left, right) -> VPair (interp env left, interp env right)
  | Split (source, left_name, right_name, body) ->
    (match interp env source with
     | VPair (left_value, right_value) ->
       let extended_env =
       (left_name, ref left_value)
       :: (right_name, ref right_value)
       :: env
       in
       interp extended_env body
     | _ -> runtime_error "Pair expected in split")
  | Fun _ as expression -> VClosure (env, expression)
  | Apply (function_expr, argument_expr) ->
    (match interp env function_expr with
     | VClosure (closure_env, Fun (_, argument_name, _, body)) ->
       let argument_value = interp env argument_expr in
       interp ((argument_name, ref argument_value) :: closure_env) body
     | _ -> runtime_error "Function expected in application")
  | Inl _ as expression -> VClosure (env, expression)
  | Inr _ as expression -> VClosure (env, expression)
  | Match _ as expression -> VClosure (env, expression)
  | Bundle _ as expression -> VClosure (env, expression)
  | Fst source ->
    (match interp env source with
     | VPair (left_value, _) -> left_value
     | _ -> runtime_error "Pair expected in fst")
  | Snd source ->
    (match interp env source with
     | VPair (_, right_value) -> right_value
     | _ -> runtime_error "Pair expected in snd")

let string_of_value = function
  | VInt value -> string_of_int value
  | VBool value -> string_of_bool value
  | VPair (left_value, right_value) ->
    "(" ^ string_of_value left_value ^ ", " ^ string_of_value right_value ^ ")"
  | VClosure (_, expression) -> Pretty_print.string_of_expression expression
  | VPlus (left_value, right_value) ->
    "inl " ^ string_of_value left_value ^ " + " ^ string_of_value right_value
  | VWith (left_value, right_value) ->
    "(" ^ string_of_value left_value ^ " & " ^ string_of_value right_value ^ ")" *)