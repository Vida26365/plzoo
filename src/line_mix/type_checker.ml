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



let is_bang_type = function
  | LBang _ -> true
  | _ -> false

(** [subtype a b] holds when a value of type [a] can be used where [b] is expected ([!t] can be used as [t]). *)
let rec subtype a b =
  a = b ||
  match a, b with
  | LBang a', LBang b' -> subtype a' b'
  | LBang a', _ -> subtype a' b
  | LAnd (a1, a2), LAnd (b1, b2)
  | LWith (a1, a2), LWith (b1, b2)
  | LPlus (a1, a2), LPlus (b1, b2) -> subtype a1 b1 && subtype a2 b2
| LArr a', LArr b' -> subtype a' b'
  | LLolli (a1, a2), LLolli (b1, b2) -> subtype b1 a1 && subtype a2 b2
  | _ -> false

(** The common type of two branches: the less restrictive of the two. *)
let join a b =
  if subtype a b then b
  else if subtype b a then a
  else typing_error "the branches have types %s and %s, which are not compatible"
         (string_of_ltype a) (string_of_ltype b)

(** Type of a toplevel [let x = e]. Without an annotation base values are unrestricted ([!Int], [!Bool]),
    with an annotation [let x : t = e] the type is exactly [t]. *)
let default_bang e ty =
  match e, ty with
  | Annot _, _ -> ty
  | _, (LInt | LBool) -> LBang ty
  | _, _ -> ty

(** f : ctx -> (a, ctx'). Adds variable to context and runs f. Checks if each variables is used exactly once. *)
let f_with_added_var_in_ctx ctx x (ty : ltype) f =
  let org_ty = Context.find_opt x ctx in
  let result, ctx' = f (Context.add x ty ctx) in
  let _ = match ty with
  | LBang _ -> ();
  | _ -> if Context.mem x ctx' then
      linear_error "linear variable %s is never used" x ;
  in
  let ctx' =
    match org_ty with
    | Some old_ty -> Context.add x old_ty ctx'
    | None -> Context.remove x ctx'
  in
  result, ctx'


let require_same_context ctx1 ctx2 =
if not (Context.equal (=) ctx1 ctx2) then
  linear_error
    "the two branches must use exactly the same resources from the surrounding context"

(** [!e] may not use linear variables, since duplicating [!e] would duplicate them. *)
let require_no_linear_used ctx ctx' =
  Context.iter (fun x ty ->
    if not (is_bang_type ty) && not (Context.mem x ctx') then
      linear_error "!e cannot use the linear variable %s" x) ctx

(** Does a value of this type contain a file handle? *)
let rec contains_file = function
  | LFile -> true
  | LAnd (a, b) | LWith (a, b) | LPlus (a, b) | LLolli (a, b) -> contains_file a || contains_file b
  | LArr t | LBang t -> contains_file t
  | LInt | LBool | LStr | LUnit -> false

(** A file handle may not be promoted to [!t], since that would allow closing it twice. *)
let require_no_file ty =
  if contains_file ty then
    linear_error "a value of type %s contains a file, so it cannot be made unrestricted with !" (string_of_ltype ty)

let rec check ctx ty e : context =
  match e, ty with
  | Inl e', LPlus (ta, _) -> check ctx ta e'
  | Inl _, ty -> typing_error "inl is used at type %s, which is not a sum type" (string_of_ltype ty)
  | Inr e', LPlus (_, tb) -> check ctx tb e'
  | Inr _, ty -> typing_error "inr is used at type %s, which is not a sum type" (string_of_ltype ty)

  | If (cond, e1, e2), _ ->
    let ctx1 = check ctx LBool cond in
    let ctx_left = check ctx1 ty e1 in
    let ctx_right = check ctx1 ty e2 in
    require_same_context ctx_left ctx_right ;
    ctx_left

  | Match (scrut, x, e1, y, e2), _ ->
    (match infer ctx scrut with
     | LPlus (ta, tb), ctx1 ->
       let (), ctx_left = f_with_added_var_in_ctx ctx1 x ta (fun ctx -> (), check ctx ty e1) in
       let (), ctx_right = f_with_added_var_in_ctx ctx1 y tb (fun ctx -> (), check ctx ty e2) in
       require_same_context ctx_left ctx_right ;
       ctx_left
     | ty', _ ->
       typing_error "this expression has type %s but match expects a sum type s + t"
         (string_of_ltype ty'))

  | Split (p, x, y, body), _ ->
    (match infer ctx p with
     | LAnd (tx, ty'), ctx1 ->
       let (), ctx2 =
         f_with_added_var_in_ctx ctx1 x tx (fun ctx ->
           f_with_added_var_in_ctx ctx y ty' (fun ctx -> (), check ctx ty body))
       in
       ctx2
     | ty', _ ->
       typing_error "this expression has type %s but split expects a tensor product s * t"
         (string_of_ltype ty'))

  | Pair (e1, e2), LAnd (ta, tb) ->
    let ctx1 = check ctx ta e1 in
    check ctx1 tb e2

  | Bundle (e1, e2), LWith (ta, tb) ->
    let ctx1 = check ctx ta e1 in
    let ctx2 = check ctx tb e2 in
    require_same_context ctx1 ctx2 ;
    ctx1

  | Fun (x, ty1, body), LLolli (ty1', ty2) when ty1 = ty1' ->
    let (), ctx' = f_with_added_var_in_ctx ctx x ty1 (fun ctx -> (), check ctx ty2 body) in
    ctx'

  | Let (x, e1, e2), _ ->
    let ty1, ctx1 = infer ctx e1 in
    let (), ctx2 = f_with_added_var_in_ctx ctx1 x ty1 (fun ctx -> (), check ctx ty e2) in
    ctx2

  | Promote e', LBang ty' ->
    require_no_file ty' ;
    let ctx' = check ctx ty' e' in
    require_no_linear_used ctx ctx' ;
    ctx'

  | Array es, LArr t -> List.fold_left (fun ctx e -> check ctx t e) ctx es

  | _, _ ->
    let ty', ctx' = infer ctx e in
    if not (subtype ty' ty) then
      typing_error "this expression has type %s but is used as if it has type %s"
        (string_of_ltype ty') (string_of_ltype ty)
    else
      ctx'


and infer ctx e : ltype * context = 
  match e with
  | Var name -> (
    match Context.find_opt name ctx with
    | Some ty -> ty, if is_bang_type ty then ctx else Context.remove name ctx
    | None -> typing_error "unbound variable %s." name
  )
  | Int _ -> LBang LInt, ctx
  | Bool _ -> LBang LBool, ctx
  | String _ -> LBang LStr, ctx
  | Unit -> LBang LUnit, ctx
  | Concat (e1, e2) -> (
    let ty1, ctx1 = infer ctx e1 in
    let ty2, ctx2 = infer ctx1 e2 in
    match ty1, ty2 with
    | LBang LStr, LBang LStr -> LBang LStr, ctx2
    | (LStr | LBang LStr), (LStr | LBang LStr) -> LStr, ctx2
    | (LStr | LBang LStr), ty | ty, _ ->
      typing_error "this expression has type %s but is used as if it has type !Str or Str" (string_of_ltype ty)
  )
  | Let (x, e1, e2) -> (
    let ty1, ctx1 = infer ctx e1 in
    f_with_added_var_in_ctx ctx1 x ty1 (fun ctx -> infer ctx e2)
  )
  | Open e ->
    LFile, check ctx LStr e
  | Read f ->
    LAnd (LStr, LFile), check ctx LFile f
  | Write (f, s) -> (
    let ctx1 = check ctx LFile f in
    LFile, check ctx1 LStr s
  )
  | Close f ->
    LBang LUnit, check ctx LFile f
  | Times (e1, e2) | Divide (e1, e2) | Mod (e1, e2)| Plus (e1, e2) | Minus (e1, e2) -> (
    let aux ctx' = match infer ctx' e2 with
    | LBang LInt, ctx2 -> LBang LInt, ctx2
    | LInt, ctx2 -> LInt, ctx2
    | ty, _ -> typing_error "this expression has type %s but is used as if it has type !Int or Int" (string_of_ltype ty) ;
    in
    match infer ctx e1 with
    | LBang LInt, ctx1 -> ( aux ctx1)
    | LInt, ctx1 -> (LInt, snd (aux ctx1))
    | ty, _ -> typing_error "this expression has type %s but is used as  if it has type !Int or Int" (string_of_ltype ty) ;
  )
  | Equal (e1, e2) | Less (e1, e2) -> (
    let ty1, ctx1 = infer ctx e1 in
    let ty2, ctx2 = infer ctx1 e2 in
    match ty1, ty2 with
    | LBang LInt, LBang LInt -> LBang LBool, ctx2
    | (LInt | LBang LInt), (LInt | LBang LInt) -> LBool, ctx2
    | (LInt | LBang LInt), ty | ty, _ ->
      typing_error "this expression has type %s but is used as if it has type !Int or Int" (string_of_ltype ty)
  )
  | If (cond, e1, e2) -> (
    let ctx1 = match infer ctx cond with
      | LBang LBool, ctx1 -> ctx1
      | LBool, ctx1 -> ctx1
      | ty, _ -> typing_error "this expression has type %s but is used as if it has type !Bool or Bool" (string_of_ltype ty) ;
    in
    let ty1, ctx_left = infer ctx1 e1 in
    let ty2, ctx_right = infer ctx1 e2 in
    require_same_context ctx_left ctx_right ;
    join ty1 ty2, ctx_left
  )
  | Pair (e1, e2) -> (
    let ty1, ctx1 = infer ctx e1 in
    let ty2, ctx2 = infer ctx1 e2 in
    LAnd (ty1, ty2), ctx2
  )
  | Split (pair, x, y, body) -> (
    match infer ctx pair with
    | LAnd (ty1, ty2), ctx1 -> (
      let f1 context = infer context body in (* The context applied should contain x and y *)
      (* Add x and y to the context then apply f1*)
      let f2 context = f_with_added_var_in_ctx context y ty2 f1 in
      let f3 context = f_with_added_var_in_ctx context x ty1 f2 in
      let ty_body, ctx2 = f3 ctx1 in
      ty_body, ctx2
    )
    | ty, _ -> typing_error "expected a pair insted got %s" (string_of_ltype ty)
  )
  | Fun (x, ty, body) -> (
    let f context = infer context body in
    let ty_body, ctx' = f_with_added_var_in_ctx ctx x ty f in
    LLolli (ty, ty_body), ctx'
  )
  | Apply (f, a) -> (
    match infer ctx f with
    | (LLolli (ty1, ty2) | LBang (LLolli (ty1, ty2))), ctx' -> (
      let ctx'' = check ctx' ty1 a in
      ty2, ctx''
    )
    | ty, _ -> typing_error "this expression has type %s but is applied as if it were a function" (string_of_ltype ty)
  )
  | Inl _ | Inr _ ->
    typing_error
      "cannot infer the type of %s: add a type annotation, e.g. (%s : Int + Bool)"
      (match e with Inl _ -> "inl e" | _ -> "inr e")
      (match e with Inl _ -> "inl e" | _ -> "inr e")

  | Match (scrut, x, e1, y, e2) -> (
    match infer ctx scrut with
    | LPlus (ta, tb), ctx1 -> (
      let ty1, ctx_left = f_with_added_var_in_ctx ctx1 x ta (fun ctx -> infer ctx e1) in
      let ty2, ctx_right = f_with_added_var_in_ctx ctx1 y tb (fun ctx -> infer ctx e2) in
      require_same_context ctx_left ctx_right ;
      join ty1 ty2, ctx_left
    )
    | ty, _ -> typing_error "this expression has type %s but match expects a" (string_of_ltype ty)
  )

  | Bundle (e1, e2) -> (
    let ty1, ctx1 = infer ctx e1 in
    let ty2, ctx2 = infer ctx e2 in
    require_same_context ctx1 ctx2 ;
    LWith (ty1, ty2), ctx1
  )
  | Fst e -> (
    match infer ctx e with
     | LWith (ta, _), ctx' -> ta, ctx'
     | ty, _ ->
       typing_error "this expression has type %s but fst expects a with-pair s & t" (string_of_ltype ty)
  )
  | Snd e -> (
    match infer ctx e with
     | LWith (_, tb), ctx' -> tb, ctx'
     | ty, _ ->
       typing_error "this expression has type %s but snd expects a with-pair s & t" (string_of_ltype ty)
  )
  | Annot (e, ty) -> 
    ty, check ctx ty e
  | Array lst -> (
    match lst with
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
    )
  )
  | Make (n, e) -> (
      let ctx' = assure_int ctx n in
      let ty, ctx'' = infer ctx' e in
      if not (is_bang_type ty) then
        typing_error "make copies its element, so it must have type !t, but it has type %s" (string_of_ltype ty) ;
      LArr ty, ctx''
  )
  | Length e -> (
    match infer ctx e with
    | LArr _, ctx' -> LInt, ctx'
    | ty, _ -> typing_error "this expression has type %s but is used as if it has type Array" (string_of_ltype ty)
  )
  | Lookup (a, i) -> (
    let ctx' = assure_int ctx i in
    match infer ctx' a with
    | LArr ty, ctx'' -> ty, ctx''
    | ty, _ -> typing_error "this expression has type %s but is used as if it has type Array" (string_of_ltype ty)
  )
  | Set (a, i, v) -> (
    let ctx' = assure_int ctx i in
    match infer ctx' a with
    | LArr ty, ctx'' ->(
      let ctx''' = check ctx'' ty v in
      LArr ty, ctx'''
    )
    | ty, _ -> typing_error "this expression has type %s but is used as if it has type Array" (string_of_ltype ty)
  )
  | Bang (e, name, body) -> (
    match infer ctx e with
    | LBang ty, ctx1 -> f_with_added_var_in_ctx ctx1 name (LBang ty) (fun ctx -> infer ctx body)
    | ty, _ -> typing_error "this expression has type %s but match expects !t" (string_of_ltype ty)
  )
  | Promote e -> (
    let ty, ctx' = infer ctx e in
    require_no_linear_used ctx ctx' ;
    require_no_file ty ;
    (match ty with LBang _ -> ty | _ -> LBang ty), ctx'
  )


and assure_int ctx e  = 
    match infer ctx e with
    | LInt, ctx' -> ctx'
    | LBang LInt, ctx' -> ctx'
    | ty, _ -> typing_error "this expression has type %s but is used as if it has type !Int or Int" (string_of_ltype ty)



(** [let rec f : t = e]: while [e] is checked, [f : t] is in the context. [t] must be [!(s -o t)]. *)
let check_rec ctx f ty e =
  match ty with
  | LBang (LLolli _) -> check (define ctx f ty) ty e
  | _ -> typing_error "a recursive function must have a type of the form !(s -o t), but it has type %s"
           (string_of_ltype ty)
