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
  | VBuiltin of (value -> value)

and environment = value Environment.t


exception Runtime_error of string
let runtime_error ~loc fmt = Zoo.error ~kind:"Runtime error" ~loc fmt

let index_error ~loc fmt = Zoo.error ~kind:"Index error" ~loc fmt



let define = Environment.add
let remove = Environment.remove

let empty = Environment.empty

let rec str_of_value = function
  | VInt x -> string_of_int x
  | VBool x -> string_of_bool x
  | VPair (a, b) -> "(" ^ (str_of_value a) ^ ", " ^ (str_of_value b) ^ ")"
  | VInl x -> "inl " ^ str_of_value x
  | VInr x -> "inr " ^ str_of_value x
  | VWith _ -> "< & >"
  | VFun _ | VRecFun _ | VBuiltin _ -> "<fun>"
  | VArr arr -> (
    let string_of_value_array arr =
    let elements = Array.to_list arr |> List.map str_of_value in
    "[" ^ String.concat ", " elements ^ "]"
    in
    string_of_value_array arr
  )

let rec equal_value ~loc v1 v2 =
  match v1, v2 with
  | VInt x, VInt y -> x = y
  | VBool x, VBool y -> x = y
  | VPair (a1, b1), VPair (a2, b2) -> equal_value ~loc a1 a2 && equal_value ~loc b1 b2
  | VInl x, VInl y -> equal_value ~loc x y
  | VInr x, VInr y -> equal_value ~loc x y
  | VArr arr1, VArr arr2 -> Array.length arr1 = Array.length arr2 && Array.for_all2 (equal_value ~loc) arr1 arr2
  | (VFun _ | VRecFun _ | VBuiltin _), _ -> runtime_error ~loc "Cannot compare functions"
  | VWith _, _ -> runtime_error ~loc "Cannot compare &-pair"
  | _ -> assert false  


let rec interp env {Zoo.data = e; loc} : value = 
  match e with
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
    | VInt _, VInt 0 -> runtime_error ~loc "Division by 0"
    | VInt a, VInt b -> VInt (a / b)
    | _ -> assert false)
  | Mod (e1, e2) -> (match interp env e1, interp env e2 with 
    | VInt _, VInt 0 -> runtime_error ~loc "Division by 0"
    | VInt a, VInt b -> VInt (a mod b)
    | _ -> assert false)
  | Plus (e1, e2) -> (match interp env e1, interp env e2 with 
    | VInt a, VInt b -> VInt (a + b)
    | _ -> assert false)
  | Minus (e1, e2) -> (match interp env e1, interp env e2 with 
    | VInt a, VInt b -> VInt (a - b)
    | _ -> assert false)
  | Equal (e1, e2) -> VBool (equal_value ~loc (interp env e1) (interp env e2))
  | Less (e1, e2) -> (match interp env e1, interp env e2 with 
    | VInt a, VInt b -> VBool (a < b)
    | _ -> assert false)


  | Array lst -> (
      let arr = Array.of_list lst in
      VArr (Array.map (interp env) arr)
  ) 
  | Make ->
      VBuiltin ( fun n ->
        VBuiltin ( fun v ->
          match n with
          | VInt k -> VArr (Array.make k v)
          | _ -> assert false
        )
      )
  | Length ->
      VBuiltin (fun a ->
        match a with
        | VArr arr -> VPair (VInt (Array.length arr), a)
        | _ -> assert false
      )
  | Lookup ->
      VBuiltin (fun i ->
        VBuiltin (fun a ->
          match a, i with
          | VArr arr, VInt k ->
              if k < 0 || k >= Array.length arr then
                index_error ~loc "index %d out of range (array length %d)" k (Array.length arr)
              else
                VPair (arr.(k), VArr arr)
          | _ -> assert false
          )
        )
  | Modify ->
      VBuiltin (fun i ->
        VBuiltin (fun f ->
          VBuiltin (fun a ->
            match i, f, a with
            | VInt i, VFun (env1, name, expr), VArr a -> (
                let v1 = Array.get a i in
                let env2 = define name v1 env1 in
                let v2 = interp env2 expr in
                Array.set a i v2;
                VArr a
            )
            | _ -> assert false
          )
        )
      )
  | Get ->
      VBuiltin (fun i ->
        VBuiltin (fun f ->
          VBuiltin (fun arr ->
            match i, f, arr with
            | VInt i, VFun (env1, name, expr), VArr a -> (
                let v1 = Array.get a i in
                let env2 = define name v1 env1 in
                let v2 = interp env2 expr in
                match v2 with
                | VPair (result, v2) -> 
                    Array.set a i v2;
                    VPair (result, VArr a)
                | _ -> assert false
            )
            | _ -> assert false
          )
        )
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
      | VBuiltin f -> f (interp env a)
      | _ -> assert false
    )


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

    
let make_rec ~loc f = function
  | VFun (env, x, body) -> VRecFun (env, f, x, body)
  | _ -> runtime_error ~loc "let rec expects a function"

