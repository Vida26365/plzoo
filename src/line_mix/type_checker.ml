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
let remove ctx x = Context.remove x ctx







let rec string_of_ltype = function
  | LInt -> "Int"
  | LBool -> "Bool"
  | LBang t -> Printf.sprintf "!%s" (string_of_ltype t)
  | LLolli (t1, t2) -> Printf.sprintf "(%s -o %s)"    (string_of_ltype t1) (string_of_ltype t2)
  | LAnd (t1, t2) -> Printf.sprintf "(%s * %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LWith (t1, t2) -> Printf.sprintf "(%s & %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LPlus (t1, t2) -> Printf.sprintf "(%s + %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LArr t -> Printf.sprintf "(%s Array)" (string_of_ltype t)


let array_error ty = typing_error "this expression has type %s but is used as if it has type Array" (string_of_ltype ty)

let is_linear = function
  | LBang _ -> false
  | _ -> true

  let unwrap = function
  | LBang ty -> ty
  | ty -> ty

let remove_bound before after x = (
  (match Context.find_opt x after with
   | Some ty when is_linear ty -> linear_error "linear variable %s is never used" x
   | _ -> ());
  match Context.find_opt x before with
  | Some ty -> define after x ty
  | None -> remove after x
)


let consumes_linear before after =
  Context.exists (fun x ty -> is_linear ty && not (Context.mem x after)) before

let require_same_context ctx1 ctx2 =
  if not (Context.equal (=) ctx1 ctx2) then
    linear_error
      "the two branches must use at the end have the same context"

let require_sameish_type ty1 ty2 =
  match ty1, ty2 with
  | LBang ta, LBang tb 
  | LBang ta, tb 
  | ta, LBang tb 
  | ta, tb -> 
    if ta <> tb then
      typing_error "expected %s and %s to be the same or diferrented by !" (string_of_ltype ty1) (string_of_ltype ty2)


let rec merge_types ty1 ty2 =
  require_sameish_type ty1 ty2;
  match ty1, ty2 with
  | _, LBang _  -> ty1
  | _, _ -> ty2

and check ctx e ty : context =
  (match e, ty with
  | Inl e', LPlus (lty1, _) -> check ctx e' lty1
  | Inr e', LPlus (_, lty2) -> check ctx e' lty2
  | Inl _, lty' -> typing_error "inl is used at type %s, which is not a sum type" (string_of_ltype lty')
  | Inr _, lty' -> typing_error "inr is used at type %s, which is not a sum type" (string_of_ltype lty')
  | Array [], LArr _ -> ctx
  | _ -> 
    let ety, ctx1 = infer ctx e in
    if ety = ty 
      then
        ctx1
      else
      typing_error "expected type %s but got %s" (string_of_ltype ty) (string_of_ltype ety)
  )

and checkish ctx e ty =
  let ety, ctx' = infer ctx e in
  require_sameish_type ety ty;
  ety, ctx'

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
  | Annot (e, ty) -> ty, check ctx e ty
  | Int _ -> LBang LInt, ctx
  | Bool _ -> LBang LBool, ctx

  | Times (e1, e2) | Plus (e1, e2) | Divide (e1, e2) | Minus (e1, e2) | Mod (e1, e2) ->
      let ty1, ctx1 = checkish ctx e1 LInt in
      let ty2, ctx2 = checkish ctx1 e2 ty1 in
      merge_types ty1 ty2, ctx2

  | Less (e1, e2) ->
      let ty1, ctx1 = checkish ctx e1 LInt in
      let ty2, ctx2 = checkish ctx1 e2 ty1 in
      let ty = merge_types ty1 ty2 in
      if is_linear ty 
        then LBool, ctx2
        else LBang LBool, ctx2
  | Equal (e1, e2) -> 
      let ty1, ctx1 = infer ctx e1 in
      let ty2, ctx2 = checkish ctx1 e2 ty1 in
      let ty = merge_types ty1 ty2 in
      (match unwrap ty with
      | LInt | LBool | LAnd _| LPlus _ | LArr _ -> ()
      | _ -> typing_error "cannot compare %s" (string_of_ltype ty));
      if is_linear ty 
        then LBool, ctx2
        else LBang LBool, ctx2

  | If (cond, e1, e2) ->
      let _, ctx' = checkish ctx cond LBool in
      let ty1, ctx_1 = infer ctx' e1 in
      let ty2, ctx_2 = checkish ctx' e2 ty1 in
      require_same_context ctx_1 ctx_2;
      merge_types ty1 ty2, ctx_1

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
      let pair_ty, ctx1 = infer ctx pair in
      (match pair_ty with
      | LAnd (ty1, ty2) -> 
          let ctx' = define (define ctx1 x ty1) y ty2 in
          let ty, ctx2 = infer ctx' body in
          let ctx3 = remove_bound ctx1 (remove_bound ctx1 ctx2 x ) y in 
          ty, ctx3
        | ty -> typing_error "expected pair type but got %s" (string_of_ltype ty)
      )
  
  | Fun (x, ty, body) -> 
      let ctx1 = define ctx x ty in
      let bty, ctx2 = infer ctx1 body in
      let ctx2 = remove_bound ctx ctx2 x in
      if consumes_linear ctx ctx2 
        then 
          LLolli (ty, bty), ctx2
        else
          LBang (LLolli (ty, bty)), ctx2
  | Apply (f, a) -> 
      let fty, ctx1 = infer ctx f in
      (match fty with
        | LLolli (ty1, ty2) | LBang (LLolli (ty1, ty2)) -> 
          ty2, check ctx1 a ty1
        | ty -> typing_error "this expression has type %s but is applied as if it were a function" (string_of_ltype ty))

  | Inl _ | Inr _ ->
      typing_error
        "cannot infer the type of %s: add a type annotation, e.g. (%s : Int + Bool)"
        (match e with Inl _ -> "inl e" | _ -> "inr e")
        (match e with Inl _ -> "inl e" | _ -> "inr e")
  | Match (scrut, x, e1, y, e2) ->
      let sty, ctx1 = infer ctx scrut in
      (match sty with
        | LPlus (ty1, ty2) -> 
           let ctx_left1 = define ctx1 x ty1 in
           let ctx_right1 = define ctx1 y ty2 in
           let ty_left, ctx_left2 = infer ctx_left1 e1 in
           let ty_right, ctx_right2 = checkish ctx_right1 e2 ty_left in
           let ctx_left3 = remove_bound ctx1 ctx_left2 x in 
           let ctx_right3 = remove_bound ctx1 ctx_right2 y in 

           require_same_context ctx_left3 ctx_right3;
           merge_types ty_left ty_right, ctx_left3
        | ty -> typing_error "this expression has type %s but match expects a Sum type" (string_of_ltype ty)
      )

  | Bundle (e1, e2) -> 
      let ty1, ctx1 = infer ctx e1 in
      let ty2, ctx2 = infer ctx e2 in
      require_same_context ctx1 ctx2;
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
  
  | Array lst -> 
      (match lst with
      | [] -> typing_error "cannot infer the type of an empty array"
      | e::es -> 
          let ty, ctx1 = infer ctx e in
          let rec aux ctx = function
            | [] -> LArr ty, ctx
            | e::es ->
                let ctx2 = check ctx e ty in
                aux ctx2 es
          in
          aux ctx1 es
      )
  | Make (n, e) ->
      let _, ctx1 = checkish ctx n LInt in
      let ty, ctx2 = infer ctx1 e in
      (match ty with
      | LBang _ -> LArr ty, ctx2
      | ty -> typing_error "expectd !-type, but got %s" (string_of_ltype ty))
  | Length e -> 
      let ty, ctx1 = infer ctx e in
      (match ty with
      | LArr _ -> LAnd (LBang LInt, ty), ctx1
      | _ -> array_error ty)
  | Lookup (a, i) ->
      let aty, ctx1 = infer ctx a in
      (match aty with
      | LArr ety -> 
          let _, ctx2 = checkish ctx1 i LInt in
          LAnd (ety, aty), ctx2
      | _ -> array_error aty
      )
  | Swap (a, i, v) ->
      let aty, ctx1 = infer ctx a in
      (match aty with
      | LArr ety -> 
          let _, ctx2 = checkish ctx1 i LInt in
          let _, ctx3 = checkish ctx2 v ety in
          LAnd (ety, aty), ctx3
      | _ -> array_error aty;
      )




let check_rec ctx f a b e =
  let ty = LBang (LLolli (a, b)) in
  let ctx1 = define ctx f ty in
  ty, check ctx1 e ty