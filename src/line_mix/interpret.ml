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
  | VArr of value array
  | VRecFun of environment * name * name * expr (* recursive function: environment, function name, argument name, body *)

and environment = value Environment.t


exception Runtime_error of string
let runtime_error message = raise (Runtime_error message)
let index_error message = raise (Invalid_argument message)


let define = Environment.add
let remove = Environment.remove

let rec str_of_value = function
  | VInt x -> string_of_int x
  | VBool x -> string_of_bool x
  | VPair (a, b) -> "(" ^ (str_of_value a) ^ ", " ^ (str_of_value b) ^ ")"
  | VInl x -> "inl " ^ str_of_value x
  | VInr x -> "inr " ^ str_of_value x
  | VWith _ -> "< & >"
  | VFun _ | VRecFun _ -> "<fun>"
  | VArr arr -> (
    let string_of_value_array arr =
    let elements = Array.to_list arr |> List.map str_of_value in
    "[" ^ String.concat ", " elements ^ "]"
    in
    string_of_value_array arr
  )

let rec equal_value v1 v2 =
  match v1, v2 with
  | VInt x, VInt y -> x = y
  | VBool x, VBool y -> x = y
  | VPair (a1, b1), VPair (a2, b2) -> equal_value a1 a2 && equal_value b1 b2
  | VInl x, VInl y -> equal_value x y
  | VInr x, VInr y -> equal_value x y
  | VArr arr1, VArr arr2 -> Array.length arr1 = Array.length arr2 && Array.for_all2 equal_value arr1 arr2
  | (VFun _ | VRecFun _ ), _ -> runtime_error "Cannot compare functions"
  | VWith _, _ -> runtime_error "Cannot compare &-pair"
  | _ -> assert false  


let rec interp env =
  function
  | Var name -> (match Environment.find_opt name env with
      | None -> assert false
      | Some x -> x
      )
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
  | Equal (e1, e2) -> VBool (equal_value (interp env e1) (interp env e2))
  | Less (e1, e2) -> (match interp env e1, interp env e2 with 
    | VInt a, VInt b -> VBool (a < b)
    | _ -> assert false)

  | If (cond, e1, e2) -> (
    match interp env cond with
    | VBool true -> interp env e1
    | VBool false -> interp env e2
    | _ -> assert false
  )

  | Annot (expr, _) -> interp env expr
  | Demote e -> interp env e

  | Pair (e1, e2) -> VPair (interp env e1, interp env e2)
  | Split (pair, x, y, body) -> (
    match interp env pair with
    | VPair (e1, e2) -> 
        let nenv = define x e1 (define y e2 env) in
        interp nenv body
    | _ -> assert false
  )

  | Fun (x, _, body) -> VFun (remove x env, x, body)
  | Apply (f, a) -> (
    match interp env f with
    | VFun (env1, x, expr) -> 
      let env2 = define x (interp env a) env1 in
      interp env2 expr
    | VRecFun (env1, f, x, expr) as v -> 
        let env2 = define x (interp env a) (define f v env1)in
        interp env2 expr
    | _ -> assert false
  )

  | Inl expr -> VInl (interp env expr)
  | Inr expr -> VInr (interp env expr)
  | Match (sum, lname, le, rname, re) -> (
    match interp env sum with
    | VInl v -> interp (define lname v env) le
    | VInr v -> interp (define rname v env) re
    | _ -> assert false
  )

  | Bundle (e1, e2) -> VWith (env, e1, e2)
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

  | Array lst -> (
      let arr = Array.of_list lst in
      VArr (Array.map (interp env) arr)
  ) 
  | Make (n, expr) -> (
    let k = match interp env n with
    | VInt k -> k
    | _ -> assert false
    in
    VArr (Array.make k (interp env expr))
  )
  | Length arr -> (
    match interp env arr with
    | VArr arr' -> VPair (VInt (Array.length arr'), VArr arr')
    | _ -> assert false
  )
  | Lookup (earr, i) -> (
    let varr = match interp env earr with
    | VArr varr' -> varr'
    | _ -> assert false
    in
    let k = match interp env i with
      | VInt k -> k
      | _ -> assert false
    in
    if not (0 <= k && k < (Array.length varr))
        then
          index_error "Array index out of range"
        else 
      VPair (Array.get varr k, VArr varr)
    )
  | Set (earr, i, expr) -> (
      let varr = match interp env earr with
        | VArr varr -> varr
        | _ -> assert false
      in
      let k = match interp env i with
        | VInt i -> i
        | _ -> assert false
      in
      if not (0 <= k && k < (Array.length varr))
        then
          index_error "Array index out of range"
        else 
          Array.set varr k (interp env expr);
          VArr varr
    )


    
let make_rec f = function
  | VFun (env, x, body) -> VRecFun (env, f, x, body)
  | _ -> runtime_error "let rec expects a function"

