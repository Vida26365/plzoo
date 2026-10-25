open Syntax

let typing_error fmt = Zoo.error ~kind:"Type error" fmt
let linear_error fmt = Zoo.error ~kind:"Linear error" fmt

module Context = Map.Make (struct
  type t = name
  let compare = compare
end)

type context = ltype Context.t

let empty : context = Context.empty


let define ctx x ty = Context.add x ty ctx







let rec string_of_ltype = function
  | LInt -> "Int"
  | LBool -> "Bool"
  | LStr -> "Str"
  | LUnit -> "Unit"
  | LFile -> "File"
  | LBang t -> Printf.sprintf "!%s" (string_of_ltype t)
  | LLolli (t1, t2) -> Printf.sprintf "(%s -o %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LAnd (t1, t2) -> Printf.sprintf "(%s * %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LWith (t1, t2) -> Printf.sprintf "(%s & %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LPlus (t1, t2) -> Printf.sprintf "(%s + %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LArr t -> Printf.sprintf "(%s Array)" (string_of_ltype t)


let is_linear = function
  | LBang _ -> false
  | _ -> true

let rec are_bang = function
  | [] -> true
  | x::xs -> not (is_linear x) && are_bang xs

let f_with_added_var (ctx : context) x (ty : ltype) (f : context -> ltype * context) : ltype * context =
  let org_ty = Context.find_opt x ctx in
  let result, ctx' = f (define ctx x ty) in
  let _ = match ty with
  | LBang _ -> ();
  | _ -> if Context.mem x ctx' then
      linear_error "linear variable %s is never used" x ;
  in
  let ctx' =
    match org_ty with
    | Some old_ty -> define ctx' x old_ty
    | None -> Context.remove x ctx'
  in
  result, ctx'

let require_same_context ctx1 ctx2 =
  if not (Context.equal (=) ctx1 ctx2) then
    linear_error
      "the two branches must use exactly the same resources from the surrounding context"


let require_sameish_type ty1 ty2 =
  match ty1, ty2 with
  | LBang ta, LBang tb 
  | LBang ta, tb 
  | ta, LBang tb 
  | ta, tb -> 
    if ta <> tb then
      typing_error "expected %s and %s to be the same or diferrented by !" (string_of_ltype ty1) (string_of_ltype ty2)

let require_same_type ty1 ty2 =
  if ty1 <> ty2 then
    typing_error "expected %s and %s to be the same" (string_of_ltype ty1) (string_of_ltype ty2)

(* let merge_types = function
    | [] -> assert false
    | [ty] -> (
      match ty with
      | LBang
    ) *)
let merge_types ty1 ty2 =
  require_sameish_type ty1 ty2;
  match ty1, ty2 with
  | _, LBang _  -> ty1
  | _, _ -> ty2



let rec check ctx ty e : context =
  match e with
  | _ -> 
    let ty', ctx' = infer ctx e in
    if ty <> ty' then
      typing_error "expected type %s but got %s" (string_of_ltype ty) (string_of_ltype ty') ;
    ctx'

and infer ctx e : ltype * context = 
  match e with
  | Var x -> (
      match Context.find_opt x ctx with
      | Some ty -> 
        if is_linear ty then
          (ty, Context.remove x ctx)
        else
          (ty, ctx)
    | None -> typing_error "unbound variable %s." x
  )
  | Annot (e, ty) -> ty, check ctx ty e 
  | Int _ -> LBang LInt, ctx
  | Bool _ -> LBang LBool, ctx
  | String _ -> LBang LStr, ctx
  | Unit -> LBang LUnit, ctx

  | Times (e1, e2) | Plus (e1, e2) | Divide (e1, e2) | Minus (e1, e2) | Mod (e1, e2) ->
      let ty1, ctx1 = infer ctx e1 in 
      let ty2, ctx2 = infer ctx1 e2 in
      let ty = merge_types ty1 ty2 in
      require_sameish_type ty LInt;
      ty, ctx2
  | Less (e1, e2) ->
      let ty1, ctx1 = infer ctx e1 in 
      let ty2, ctx2 = infer ctx1 e2 in
      let ty = merge_types ty1 ty2 in
      require_sameish_type ty LBool;
      ty, ctx2
  | Equal (e1, e2) ->
      let (ty1, ctx1) = infer ctx e1 in
      let (ty2, ctx2) = infer ctx1 e2 in
      require_sameish_type ty1 ty2;
      (LBool, ctx2)

  | Demote e -> 
      let ty, ctx1 = infer ctx e in
      (match ty with
      | LBang ty' -> ty', ctx1
      | ty -> typing_error "expected !-type but got %s" (string_of_ltype ty))

  | Pair (e1, e2) ->
      let ty1, ctx1 = infer ctx e1 in
      let ty2, ctx2 = infer ctx1 e2 in
      LAnd (ty1, ty2), ctx2
  | Split (pair, x, y, body) ->  
      (match infer ctx pair with
      | LAnd (ty1, ty2), ctx' -> 
        let f0 ctx2 = infer ctx2 body in
        let f1 ctx' = f_with_added_var ctx' x ty1 f0 in
        let f2 ctx1 = f_with_added_var ctx1 y ty2 f1 in
        f2 ctx'

      | ty, _ -> typing_error "expected pair type but got %s" (string_of_ltype ty) ;)
  | Fun (x, ty, body) ->
      let f ctx = infer ctx body in
      let ty_body, ctx' = f_with_added_var ctx x ty f in
      LBang (LLolli (ty, ty_body)), ctx'
  | Apply (f, a) ->
      let f_ty, ctx' = infer ctx f in
      (match f_ty with
      | LBang (LLolli (ty1, ty2)) | LLolli (ty1, ty2) -> 
        ty2, check ctx' ty1 a
      | ty -> typing_error "this expression has type %s but is applied as if it were a function" (string_of_ltype ty))

  | Inl _ | Inr _ ->
      typing_error
        "cannot infer the type of %s: add a type annotation, e.g. (%s : Int + Bool)"
        (match e with Inl _ -> "inl e" | _ -> "inr e")
        (match e with Inl _ -> "inl e" | _ -> "inr e")
  | Match (scrut, x, e1, y, e2) -> (
      match infer ctx scrut with
      | LPlus (ta, tb), ctx1 ->  
        let ty1, ctx_left = f_with_added_var ctx1 x ta (fun ctx -> infer ctx e1) in
        let ty2, ctx_right = f_with_added_var ctx1 y tb (fun ctx -> infer ctx e2) in
        require_same_context ctx_left ctx_right ;
        require_sameish_type ty1 ty2;
        merge_types ty1 ty2, ctx_left
      | ty, _ -> typing_error "this expression has type %s but match expects a Sum type" (string_of_ltype ty)
  )

  | Bundle (e1, e2) ->
      let ty1, ctx1 = infer ctx e1 in
      let ty2, ctx2 = infer ctx e2 in
      require_same_context ctx1 ctx2 ;
      LWith (ty1, ty2), ctx1
  | Fst e -> (
      match infer ctx e with
      | LWith (ta, _), ctx' -> ta, ctx'
      | ty, _ ->
        typing_error "this expression has type %s but fst expects a with-pair" (string_of_ltype ty)
  )
  | Snd e -> (
      match infer ctx e with
      | LWith (_, tb), ctx' -> tb, ctx'
      | ty, _ ->
        typing_error "this expression has type %s but snd expects a with-pair" (string_of_ltype ty)
  )

  | Let (x, e1, e2) -> (
    let ty1, ctx1 = infer ctx e1 in
    f_with_added_var ctx1 x ty1 (fun ctx -> infer ctx e2)
  )

  | Array lst ->
      (match lst with
      | [] -> typing_error "cannot infer the type of an empty array"
      | e::es -> (
          let ty, ctx' = infer ctx e in
          let rec aux ctx'' = function
            | [] -> LArr ty, ctx''
            | e::es -> (
              let ctx''' = check ctx'' ty e in
              aux ctx''' es
            )
          in
          aux ctx' es
        ))
  | Make (n, e) -> 
      let nty, ctx1 = infer ctx n in
      require_sameish_type nty LInt;
      let ty, ctx2 = infer ctx1 e in
      (match ty with
      | LBang _ -> ();
      | ty -> typing_error "make copies its element, so it must have type !t, but it has type %s" (string_of_ltype ty) ;);
      LArr ty, ctx2
  | Length e ->
      (match infer ctx e with
      | LArr ty, ctx1 -> LAnd (LInt, LArr ty), ctx1
      | ty, _ -> typing_error "this expression has type %s but is used as if it has type Array" (string_of_ltype ty))
  | Lookup (a, i) -> 
      let aty, ctx1 = infer ctx a in
      (match aty with
      | LArr ty -> 
          let ity, ctx2 = infer ctx1 i in
          require_sameish_type ity LInt;
          LAnd (ty, aty), ctx2
      | ty -> typing_error "this expression has type %s but is used as if it has type Array" (string_of_ltype ty)
        )
  | Set (a, i, v) -> 
      let aty, ctx1 = infer ctx a in
      (match aty with
      | LArr ty -> 
        let ity, ctx2 = infer ctx1 i in
        require_sameish_type ity LInt;
        let vty, ctx3 = infer ctx2 v in
        require_same_type ty vty;
        ty, ctx3
        
      | ty -> typing_error "this expression has type %s but is used as if it has type Array" (string_of_ltype ty)
      )

  | Open e -> 
      let ty, ctx' = infer ctx e in
      require_sameish_type ty LStr;
      ty, ctx'
  | Read f -> 
      let ty, ctx' = infer ctx f in
      require_sameish_type ty LStr;
      LAnd (LStr, LFile), ctx'
  | Write (f, s) ->
      let ctx1 = check ctx LFile f in
      let sty, ctx2 = infer ctx1 s in
      require_sameish_type sty LStr;
      LFile, ctx2
  | Close f ->
      LBang LUnit, check ctx LFile f
  
  | If (cond, e1, e2) -> 
      let cty, ctx' = infer ctx cond in
      require_sameish_type cty LBool;
      let ty1, ctx1 = infer ctx' e1 in
      let ty2, ctx2 = infer ctx' e2 in
      require_same_context ctx1 ctx2;
      merge_types ty1 ty2, ctx1

  | Concat (e1, e2) ->
      let ty1, ctx1 = infer ctx e1 in
      let ty2, ctx2 = infer ctx1 e2 in
      require_sameish_type ty1 LStr;
      require_sameish_type ty2 LStr;
      merge_types ty1 ty2, ctx2




