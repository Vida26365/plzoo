open Syntax


type value =
  | VInt of int
  | VBool of bool
  | VPair of value * value
    | VInl of value
    | VInr of value
  | VWith of value * value
  (* | VClosure of environment * expr *)

exception Runtime_error of string

let runtime_error message = raise (Runtime_error message)

let string_of_expr expression =
  let rec to_str precedence = function
    | Var name -> name
    | Int value -> string_of_int value
    | Bool value -> string_of_bool value
    | Times (left, right) ->
        let text = to_str 8 left ^ " * " ^ to_str 9 right in
        if precedence > 8 then "(" ^ text ^ ")" else text
    | Divide (left, right) ->
        let text = to_str 8 left ^ " / " ^ to_str 9 right in
        if precedence > 8 then "(" ^ text ^ ")" else text
    | Mod (left, right) ->
        let text = to_str 8 left ^ " % " ^ to_str 9 right in
        if precedence > 8 then "(" ^ text ^ ")" else text
    | Plus (left, right) ->
        let text = to_str 7 left ^ " + " ^ to_str 8 right in
        if precedence > 7 then "(" ^ text ^ ")" else text
    | Minus (left, right) ->
        let text = to_str 7 left ^ " - " ^ to_str 8 right in
        if precedence > 7 then "(" ^ text ^ ")" else text
    | Equal (left, right) ->
        let text = to_str 6 left ^ " = " ^ to_str 6 right in
        if precedence > 6 then "(" ^ text ^ ")" else text
    | Less (left, right) ->
        let text = to_str 6 left ^ " < " ^ to_str 6 right in
        if precedence > 6 then "(" ^ text ^ ")" else text
    | Pair (left, right) ->
        "(" ^ to_str 0 left ^ ", " ^ to_str 0 right ^ ")"
    | Split (source, left_name, right_name, body) ->
        "split " ^ to_str 0 source ^ " to " ^ left_name ^ " " ^ right_name ^ " in " ^ to_str 0 body
    | Fun (argument_name, body) ->
        "lambda " ^ argument_name ^ " in " ^ to_str 0 body
    | Apply (function_expr, argument_expr) ->
        let text = to_str 9 function_expr ^ " " ^ to_str 10 argument_expr in
        if precedence > 9 then "(" ^ text ^ ")" else text
    | Inl value -> "inl " ^ to_str 10 value
    | Inr value -> "inr " ^ to_str 10 value
    | Match (value, left_name, left_expr, right_name, right_expr) ->
        "match " ^ to_str 0 value ^ " with inl " ^ left_name ^ " -> " ^ to_str 0 left_expr ^
        " | inr " ^ right_name ^ " -> " ^ to_str 0 right_expr
    | Bundle (left, right) ->
        "bundle " ^ to_str 0 left ^ " " ^ to_str 0 right
    | Fst value -> "fst " ^ to_str 10 value
    | Snd value -> "snd " ^ to_str 10 value
  in
  to_str 0 expression

let rec string_of_value = function
  | VInt value -> string_of_int value
  | VBool value -> string_of_bool value
  | VPair (left_value, right_value) ->
      "(" ^ string_of_value left_value ^ ", " ^ string_of_value right_value ^ ")"
    | VInl value -> "inl " ^ string_of_value value
    | VInr value -> "inr " ^ string_of_value value
  | VWith (left_value, right_value) ->
      string_of_value left_value ^ " & " ^ string_of_value right_value
    | VClosure (_, expression) -> string_of_expr expression

let rec interp env = function
  | Var name ->
      (try
         let cell = List.assoc name env in
                 let value = force !cell in
                 cell := value;
                 value
       with Not_found -> runtime_error ("Unknown variable " ^ name))
  | Int value -> VInt value
  | Bool value -> VBool value
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
  | Pair (left, right) -> VPair (VClosure (env, left), VClosure (env, right))
  | Split (source, left_name, right_name, body) ->
      (match force (interp env source) with
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
      (match force (interp env function_expr) with
       | VClosure (closure_env, Fun (argument_name, body)) ->
           let argument_value = VClosure (env, argument_expr) in
           interp ((argument_name, ref argument_value) :: closure_env) body
       | _ -> runtime_error "Function expected in application")
  | Inl value -> VInl (VClosure (env, value))
  | Inr value -> VInr (VClosure (env, value))
  | Match (scrutinee, left_name, left_expr, right_name, right_expr) ->
      (match force (interp env scrutinee) with
       | VInl value ->
           interp ((left_name, ref value) :: env) left_expr
       | VInr value ->
           interp ((right_name, ref value) :: env) right_expr
       | _ -> runtime_error "Sum expected in match")
  | Bundle (left, right) -> VWith (VClosure (env, left), VClosure (env, right))
  | Fst source ->
      (match force (interp env source) with
       | VWith (left_value, _) -> left_value
       | _ -> runtime_error "Bundle expected in fst")
  | Snd source ->
      (match force (interp env source) with
       | VWith (_, right_value) -> right_value
       | _ -> runtime_error "Bundle expected in snd")

and force = function
    | VClosure (closure_env, expression) -> interp closure_env expression
    | value -> value