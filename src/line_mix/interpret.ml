open Syntax

(* Define a structure that will save variable names and their values. Equivalent to Map.Make(String) *)
module Environment = Map.Make(struct 
  type t = name
  let compare = compare
end)



(** Define a structure, that will serve as set of free variables of an expression*)
module NameSet = Set.Make(String)

type value =
  | VInt of int
  | VBool of bool
  | VPair of value * value
  | VInl of value
  | VInr of value
  | VWith of environment * expr * expr 
  | VFun of environment * name * expr (* This form is needed for application. Name serves for a name of a variable that will get substituted on apply. It can be done with VClosure, but it is easier with VFun*)
  | VArr of value * value array

and environment = value Environment.t


exception Runtime_error of string
let runtime_error message = raise (Runtime_error message)
let index_error message = raise (Invalid_argument message)


let rec str_of_value = function
  | VInt x -> string_of_int x
  | VBool x -> string_of_bool x
  | VPair (a, b) -> "(" ^ (str_of_value a) ^ ", " ^ (str_of_value b) ^ ")"
  | VInl x -> "inl " ^ str_of_value x
  | VInr x -> "inr " ^ str_of_value x
  | VWith _ -> "< & >"
  | VFun _ -> "<fun>"
  | VArr (_, arr) -> (
    let string_of_value_array arr =
    let elements = Array.to_list arr |> List.map str_of_value in
    "[" ^ String.concat ", " elements ^ "]"
    in
    string_of_value_array arr
  )



  let rec interp env =
    function
  | Var name -> (match Environment.find_opt name env with
    | None -> runtime_error ("Unknown variable " ^ name)
    | Some value -> value )
  | Int value -> VInt value
  | Bool value -> VBool value
  | Times (e1, e2) -> (match interp env e1, interp env e2 with 
    | VInt a, VInt b -> VInt (a * b)
    | _ -> assert false)
  | Divide (e1, e2) -> (match interp env e1, interp env e2 with 
    | VInt _, VInt 0 -> runtime_error "Division by 0"
    | VInt a, VInt b -> VInt (a / b)
    | _ -> assert false)
  | Mod (e1, e2) -> (match interp env e1, interp env e2 with 
    | VInt _, VInt 0 -> runtime_error "Division by 0"
    | VInt a, VInt b -> VInt (a mod b)
    | _ -> assert false)
  | Plus (e1, e2) -> (match interp env e1, interp env e2 with 
    | VInt a, VInt b -> VInt (a + b)
    | _ -> assert false)
  | Minus (e1, e2) -> (match interp env e1, interp env e2 with 
    | VInt a, VInt b -> VInt (a - b)
    | _ -> assert false)
  | Equal (e1, e2) -> (match interp env e1, interp env e2 with 
    | VInt a, VInt b -> VBool (a == b)
    | _ -> assert false)
  | Less (e1, e2) -> (match interp env e1, interp env e2 with 
    | VInt a, VInt b -> VBool (a < b)
    | _ -> assert false)

  | If (cond, e1, e2) -> (
    match interp env cond with
    | VBool true -> interp env e1
    | VBool false -> interp env e2
    | _ -> assert false
  )

  | Pair (e1, e2) -> VPair (interp env e1, interp env e2)
  | Split (pair, name1, name2, expr) -> 
      (match interp env pair with
        | VPair (e1, e2) -> (
          let env' = env
            |> Environment.add name1 e1 
            |> Environment.add name2 e2 in
          interp env' expr
        )
        | _ -> assert false)
  | Fun (name_x, _ty, expr) -> VFun (Environment.remove name_x env, name_x, expr)
  | Apply (f, a) -> (
    match interp env f with
    | VFun (env', name_x, expr) -> (
      let env'' = Environment.add name_x (interp env a) env' in
      interp env'' expr
    )
    | _ -> assert false
  )
  | Inl expr -> VInl (interp env expr)
  | Inr expr -> VInr (interp env expr)
  | Match (sum, lname, lexpr, rname, rexpr) -> (
    match interp env sum with
    | VInl value -> (
      let env' = Environment.add lname value env in
      interp env' lexpr
    )
    | VInr value -> (
      let env' = Environment.add rname value env in
      interp env' rexpr
    )
    | _ -> assert false
    )
  | Bundle (expr1, expr2) -> VWith (env, expr1, expr2)
  | Fst expr -> (
    match interp env expr with
    | VWith (env', expr, _) -> interp env' expr
    | _ -> assert false
  )
  | Snd expr -> (
    match interp env expr with
    | VWith (env', _, expr) -> interp env' expr
    | _ -> assert false
  )
  | Annot (expr, _) -> interp env expr

  | Array lst -> (
    let arr = Array.of_list lst in
    VArr (VInt (Array.length arr), (Array.map (interp env) arr))
  ) 
  | Make (n, expr) -> (
    let k = match interp env n with
    | VInt k -> k
    | _ -> assert false
    in
    VArr (VInt k, (Array.make k (interp env expr)))
  )
    
  | Length (arr) -> (match interp env arr with
    | VArr (n, _) -> n
    | _ -> assert false
    )
  | Lookup (earr, i) -> (match interp env earr with
    | VArr (_, varr) -> Array.get varr (match interp env i with
      | VInt k -> k
      | _ -> assert false)
    | _ -> assert false
    )
    | Set (earr, i, expr) -> (
      let varr = match interp env earr with
        | VArr (_, varr) -> varr
        | _ -> assert false
      in
      let k = match interp env i with
        | VInt i -> i
        | _ -> assert false
      in
      Array.set varr k (interp env expr);
      VArr (VInt (Array.length varr), varr)
    )
  | Bang expr -> interp env expr

