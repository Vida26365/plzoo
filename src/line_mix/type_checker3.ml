open Syntax

let typing_error fmt = Zoo.error ~kind:"Type error" fmt
let linear_error fmt = Zoo.error ~kind:"Linear error" fmt

module Context = Map.Make (struct
  type t = name
  let compare = compare
end)

type variable =
  | Linear of ltype
  | Unrestricted of ltype

type context = variable Context.t

let define ctx x (ty : variable) = Context.add x ty ctx




let empty : context = Context.empty

let ltype_to_variable = function
  | LInt -> Unrestricted LInt
  | LBool  -> Unrestricted LBool
  | LStr -> Unrestricted LStr
  | LUnit -> Unrestricted LUnit
  | LFile -> Linear LFile
  | LLolli (t1, t2) -> Linear (LLolli (t1, t2))
  | LAnd (t1, t2) -> Linear (LAnd (t1, t2))
  | LWith (t1, t2) -> Linear (LWith (t1, t2))
  | LPlus (t1, t2) -> Linear (LPlus (t1, t2))
  | LArr t -> Linear (LArr t)

let rec string_of_ltype = function
  | LInt -> "Int"
  | LBool -> "Bool"
  | LStr -> "Str"
  | LUnit -> "Unit"
  | LFile -> "File"
  | LLolli (t1, t2) -> Printf.sprintf "(%s -o %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LAnd (t1, t2) -> Printf.sprintf "(%s * %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LWith (t1, t2) -> Printf.sprintf "(%s & %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LPlus (t1, t2) -> Printf.sprintf "(%s + %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LArr t -> Printf.sprintf "(%s Array)" (string_of_ltype t)

let string_of_variable = function
  | Linear t -> Printf.sprintf "Linear %s" (string_of_ltype t)
  | Unrestricted t -> Printf.sprintf "Unrestricted %s" (string_of_ltype t)

let unwrap = function
  | Linear t -> t
  | Unrestricted t -> t

let require_same_context ctx1 ctx2 =
if not (Context.equal (=) ctx1 ctx2) then
  linear_error
    "the two branches must use exactly the same resources from the surrounding context"

let f_with_added_var ctx x ty (f : context -> 'a * context)  =
  let org_ty = Context.find_opt x ctx in
  let (result, ctx') = f (Context.add x ty ctx) in
  let _ = match ty with
    | Unrestricted _ -> ()
    | Linear _ -> if Context.mem x ctx' then
        linear_error "linear variable %s is never used" x
  in
  let ctx'' = match org_ty with
    | Some _ -> Context.add x ty ctx'
    | None -> Context.remove x ctx'
  in
  (result, ctx'')

let rec infer_linearity lst ty =
  match lst with
  | [] -> Unrestricted ty
  | x::xs -> match x with
    | Linear _ -> Linear ty
    | Unrestricted _ -> infer_linearity xs ty

let check_ltype lty ty =
  if (unwrap lty) <> ty then
    typing_error "expected type %s but got type %s"
      (string_of_ltype (unwrap lty)) (string_of_ltype ty)

let check_same_type ty1 ty2 =
  if ty1 <> ty2 then
    typing_error "expected type %s but got type %s"
      (string_of_variable ty1) (string_of_variable ty2)
  
let rec check ctx e lty =
  match e, lty with
  | Inl e', LPlus (lty1, _) -> check ctx e' lty1
  | Inr e', LPlus (_, lty2) -> check ctx e' lty2
  | Inl _, lty' -> typing_error "inl is used at type %s, which is not a sum type" (string_of_ltype lty')
  | Inr _, lty' -> typing_error "inr is used at type %s, which is not a sum type" (string_of_ltype lty')
  | _ ->
      let (ty, ctx1) = infer ctx e in
      check_ltype ty lty ;
      (ty, ctx1)
  
and infer ctx e : variable * context =
  match e with
  | Var x -> (match Context.find_opt x ctx with
      | Some v -> ( match v with
        | Linear ty -> (Linear ty, Context.remove x ctx)
        | Unrestricted ty -> (Unrestricted ty, ctx)
        )
      | None -> typing_error "unbound variable %s" x
      )
  | Int _ -> (Unrestricted LInt, ctx)
  | Bool _ -> (Unrestricted LBool, ctx)
  | String _ -> (Unrestricted LStr, ctx)
  | Unit -> (Unrestricted LUnit, ctx)
  | Times (e1, e2) | Divide (e1, e2) | Mod (e1, e2) | Plus (e1, e2) | Minus (e1, e2) ->
      let (ty1, ctx1) = check ctx e1 LInt in
      let (ty2, ctx2) = check ctx1 e2 LInt in
      (infer_linearity [ty1; ty2] LInt, ctx2)
  | Less (e1, e2) ->
      let (ty1, ctx1) = check ctx e1 LInt in
      let (ty2, ctx2) = check ctx1 e2 LInt in
      (infer_linearity [ty1; ty2] LBool, ctx2)
  | Equal (e1, e2) ->
      let (ty1, ctx1) = infer ctx e1 in
      let (ty2, ctx2) = infer ctx1 e2 in
      check_same_type ty1 ty2;
      (infer_linearity [ty1; ty2] LBool, ctx2)
  | Concat (e1, e2) ->
      let (ty1, ctx1) = check ctx e1 LStr in
      let (ty2, ctx2) = check ctx1 e2 LStr in
      (infer_linearity [ty1; ty2] LStr, ctx2)
  | Let (x, e1, e2) -> 
      let ty1, ctx1 = infer ctx e1 in
      f_with_added_var ctx1 x ty1 (fun ctx2 -> infer ctx2 e2)
  | If (cond, e1, e2) ->
      let (_, ctx1) = check ctx cond LBool in
      let (ty1, ctx2) = infer ctx1 e1 in
      let (ty2, ctx3) = infer ctx2 e2 in
      require_same_context ctx2 ctx3;
      check_same_type ty1 ty2;
      (ty1, ctx3)
  | Open e ->
      let (_, ctx1) = check ctx e LStr in
      (Linear LFile, ctx1)
  | Close e ->
      let (_, ctx1) = check ctx e LFile in
      (Unrestricted LUnit, ctx1)
  | Read e -> 
      let (_, ctx1) = check ctx e LFile in
      (Linear (LAnd (LStr, LFile)), ctx1)
  | Write (f, s) ->
      let (_, ctx1) = check ctx f LFile in
      let (_, ctx2) = check ctx1 s LStr in
      (Linear LFile, ctx2)
  
  | Pair (e1, e2) -> 
      let (ty1, ctx1) = infer ctx e1 in
      let (ty2, ctx2) = infer ctx1 e2 in
      (Linear (LAnd (unwrap ty1, unwrap ty2)), ctx2)
  | Split (pair, x, y, body) ->(
      match infer ctx pair with
      | (Linear (LAnd (ty1, ty2)), ctx1) ->
          let f3 ctx = infer ctx body in
          let f2 ctx = f_with_added_var ctx y (ltype_to_variable ty2) f3 in
          let f1 ctx = f_with_added_var ctx x (ltype_to_variable ty1) f2 in
          f1 ctx1
      | (ty, _) -> typing_error "expected a pair but got %s" (string_of_variable ty))
  
  | Fun (x, lty, body) -> 
      let f ctx = infer ctx body in
      let (ty_body, ctx1) = f_with_added_var ctx x (ltype_to_variable lty) f in
      (Linear (LLolli (lty, unwrap ty_body)), ctx1)
    | Apply (f, a) -> (
      match infer ctx f with
      | (Linear (LLolli (lty1, lty2)), ctx1) -> 
          let (_, ctx2) = check ctx1 a lty1 in
          (ltype_to_variable lty2, ctx2)
      | (ty, _) -> typing_error "expected a function but got %s" (string_of_variable ty)
      )
    
    | Annot (t, lty) -> 
        let (ty, ctx1) = infer ctx t in
        check_ltype ty lty;
        (ty, ctx1)
    | Inl _ | Inr _ ->
        typing_error
          "cannot infer the type of %s: add a type annotation, e.g. (%s : Int + Bool)"
          (match e with Inl _ -> "inl e" | _ -> "inr e")
          (match e with Inl _ -> "inl e" | _ -> "inr e")
    | Match (scrut, x, e1, y, e2) -> (
        match infer ctx scrut with
        | (Linear (LPlus (ty1, ty2)), ctx1) ->
            let ta, ctx_left = f_with_added_var ctx1 x (ltype_to_variable ty1) (fun ctx -> infer ctx e1) in
            let tb, ctx_right = f_with_added_var ctx1 y (ltype_to_variable ty2) (fun ctx -> infer ctx e2) in
            require_same_context ctx_left ctx_right;
            check_same_type ta tb;
            (ta, ctx_right)
        | (ty, _) -> typing_error "expected a sum type but got %s"
            (string_of_variable ty)    
            )
      
    | Bundle (e1, e2) -> (
        let (ty1, ctx1) = infer ctx e1 in
        let (ty2, ctx2) = infer ctx1 e2 in
        require_same_context ctx1 ctx2;
        (Linear (LWith (unwrap ty1, unwrap ty2)), ctx2)
    )

    | Fst e -> (
        match infer ctx e with
        | (Linear (LWith (ty1, _)), ctx1) -> (ltype_to_variable ty1, ctx1)
        | (ty, _) -> typing_error "expected a with-pair type but got %s" (string_of_variable ty)
    )
    | Snd e -> (
        match infer ctx e with
        | (Linear (LWith (_, ty2)), ctx1) -> (ltype_to_variable ty2, ctx1)
        | (ty, _) -> typing_error "expected a with-pair type but got %s" (string_of_variable ty)
    )
    
    | Array lst -> (
      match lst with
      | [] -> typing_error "cannot infer the type of an empty array: add a type annotation"
      | e::es -> 
        let (ty, ctx1) = infer ctx e in
        let rec check_all ctx lst =
          match lst with
          | [] -> Linear (LArr (unwrap ty)), ctx
          | e::es -> 
            let (_, ctx') = check ctx e (unwrap ty) in
            check_all ctx' es
        in
        check_all ctx1 es
    )
    | Make (n, e) -> 
        let nty, ctx1 =  infer ctx n in
        check_ltype nty LInt;
        let ety, ctx2 = infer ctx1 e in
        (Linear (LArr (unwrap ety)), ctx2)
    | Length e -> (
        match infer ctx e with
        | (Linear (LArr ty), ctx1) -> (Linear (LAnd (LInt, LArr ty)), ctx1)
        | (ty, _) -> typing_error "expected an array but got %s" (string_of_variable ty)
        );
    | Lookup (a, i) -> 
        let aty, ctx1 = infer ctx a in
        let _, ctx2 = check ctx1 i LInt in
        (match aty with
        | Linear (LArr t) -> (Linear (LAnd (t, LArr t)), ctx2)
        | _ -> typing_error "expected an array but got %s" (string_of_variable aty))
    | Set (a, i, v) -> 
        let aty, ctx1 = infer ctx a in
        let _, ctx2 = check ctx1 i LInt in
        let vty, ctx3 = infer ctx2 v in
        (match aty with
        | Linear (LArr t) -> 
            check_ltype vty t;
            (Linear (LArr t), ctx3)
        | _ -> typing_error "expected an array but got %s" (string_of_variable aty))
        
  
      
let check_rec ctx f ty e =
  match ty with
  | Linear (LLolli _) ->    
    check (define ctx f ty) e (unwrap ty)
  | _ -> typing_error "a recursive function must have a type of the form !(s -o t), but it has type %s"
           (string_of_variable ty)