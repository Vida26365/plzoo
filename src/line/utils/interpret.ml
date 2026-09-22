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
  | Fun (name, _ty, expr)      -> ( (* when expression e gets turnes into x -> e, x stops being free variable *)
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
  | Bundle (expr1, expr2) -> VWith (interp env expr1, interp env expr2)
  | Fst expr -> (
    match interp env expr with
    | VWith (value, _) -> value
    | _ -> assert false
  )
  | Snd expr -> (
    match interp env expr with
    | VWith (_, value) -> value
    | _ -> assert false
  )

