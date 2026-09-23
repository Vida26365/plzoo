open Utils.Syntax

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

