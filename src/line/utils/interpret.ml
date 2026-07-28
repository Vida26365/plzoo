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
  | VWith of value * value
  | VClosure of environment * expr
  | VFun of environment * name * expr (* This form is needed for application. Name serves for a name of a variable that will get substituted on apply. It can be done with VClosure, but it is easier with VFun*)

and environment = value Environment.t


exception Runtime_error of string
let runtime_error message = raise (Runtime_error message)


(** Find free variables in expression *)
let rec find_free_variables = 
  function
  | Var x -> NameSet.singleton x
  | Int _ | Bool _        -> NameSet.empty
  | Times (left, right)   -> NameSet.union (find_free_variables left) (find_free_variables right)
  | Divide (left, right)  -> NameSet.union (find_free_variables left) (find_free_variables right)
  | Mod (left, right)     -> NameSet.union (find_free_variables left) (find_free_variables right)
  | Plus (left, right)    -> NameSet.union (find_free_variables left) (find_free_variables right)
  | Minus (left, right)   -> NameSet.union (find_free_variables left) (find_free_variables right)
  | Equal (left, right)   -> NameSet.union (find_free_variables left) (find_free_variables right)
  | Less (left, right)    -> NameSet.union (find_free_variables left) (find_free_variables right)

  | Pair (left, right)                -> NameSet.union (find_free_variables left) (find_free_variables right)
  | Split (pair, name1, name2, expr)  -> ( (* Free variables are variables in expr and pair, without variables nam1 and name2 *)
    find_free_variables pair
    |> NameSet.union (find_free_variables expr)
    |> NameSet.remove name1
    |> NameSet.remove name2
  )
  | Fun (name, expr)           -> ( (* when expression e gets turnes into x -> e, x stops being free variable *)
    find_free_variables expr
    |> NameSet.remove name)
  | Apply (expr1, expr2)       -> NameSet.union (find_free_variables expr1) (find_free_variables expr2)
  
  | Inl expr      -> find_free_variables expr
  | Inr expr      -> find_free_variables expr
  | Match (sum, name_inl, expr1, name_inr, expr2) -> (* free variables are from sum, expr1 and expr2, but name_inl and name_inr stop being free variables*)
    find_free_variables sum
    |> NameSet.union (find_free_variables expr1) 
    |> NameSet.union (find_free_variables expr2) 
    |> NameSet.remove name_inl
    |> NameSet.remove name_inr
  | Bundle (left, right) -> NameSet.union (find_free_variables left) (find_free_variables right)
  | Fst expr -> find_free_variables expr
  | Snd expr -> find_free_variables expr

(** Get an subset of an environment, that containes only subset defined by subset of free variables*)
let free_vars_to_env main_env free_vars = 
  let filter_func key _ = NameSet.inter free_vars (NameSet.singleton key) == NameSet.empty in
  Environment.filter filter_func main_env


(** interp models introduction and elimination rules for linear programms *)
let rec interp env = 
  let local_env expr = free_vars_to_env env (find_free_variables expr) in
  function
  | Var name -> (match Environment.find_opt name env with
    | None -> runtime_error ("Unknown variable " ^ name)
    | Some value -> value )
  | Int value -> VInt value
  | Bool value -> VBool value
  | Times (left, right) -> (
    match (interp env left, interp env right) with
    | VInt x, VInt y -> VInt (x * y)
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
  
  (** Defining pair with VClosure with local environment serves, that when Split gets called, only variables in expr get bounded. Similarly for other times VClosure is used *)
  | Pair (left, right) -> VPair (VClosure ((local_env left), left), VClosure ((local_env right), right))
  | Split (pair, name1, name2, expr) -> (
    match interp env pair with
    | VPair (VClosure (env1, _), VClosure (env2, _)) -> ( 
      let env3 = local_env expr 
      |> Environment.remove name1
      |> Environment.remove name2
      in
      let new_env = env3
      |> Environment.union (fun _ value1 value2 -> if value1 == value2 then Some value1 else None) env2
      |> Environment.union (fun _ value1 value2 -> if value1 == value2 then Some value1 else None) env1
      in
      VClosure (new_env, expr)
    )
    | _ -> runtime_error "Pair expected in split")

  | Fun (name_x, expr) -> VFun (Environment.remove name_x (local_env expr), name_x, expr)
  | Apply (f, a) -> (
    match interp env f with
    | VFun (lenv, name_x, _) -> (
      let new_env = Environment.add name_x (interp env a) lenv in
      VClosure (new_env, f))
    | _ -> runtime_error "Function expected in application")

  | Inl expr -> VInl (interp env expr)
  | Inr expr -> VInr (interp env expr)
  | Match (sum, inlx, expr1, inly, expr2) -> (
    match interp env sum with
    | VInl value -> (
      let new_env = Environment.add inlx value (local_env expr1) in
      VClosure (new_env, expr1))
    | VInr value -> (
      let new_env = Environment.add inly value (local_env expr2) in
      VClosure (new_env, expr2))
    | _ -> runtime_error "Inl or Inr expected in match")

  | Bundle (expr1, expr2) -> VWith ((interp env expr1), (interp env expr2))
  | Fst expr -> (
    match expr with
    | Bundle (_, _) -> interp env expr
    | _ -> runtime_error "Bundle expected in fst")
  | Snd expr -> (
    match expr with
    | Bundle (_, _) -> interp env expr
    | _ -> runtime_error "Bundle expected in fst")
