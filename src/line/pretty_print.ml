
open Syntax

let rec string_of_type = function
  | LInt -> "int"
  | LLolli (left, right) -> string_of_type left ^ " ⊸ " ^ string_of_type right
  | LAnd (left, right) -> string_of_type left ^ " ⊗ " ^ string_of_type right
  | LWith (left, right) -> string_of_type left ^ " & " ^ string_of_type right
  | LPlus (left, right) -> string_of_type left ^ " ⊕ " ^ string_of_type right

let string_of_expression expression =
  let rec to_str precedence = function
    | Var name -> name
    | Int value -> string_of_int value
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
    | Fun (function_name, argument_name, ty, body) ->
        "fun " ^ function_name ^ " " ^ argument_name ^ " : " ^ string_of_type ty ^ " -> " ^ to_str 0 body
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





