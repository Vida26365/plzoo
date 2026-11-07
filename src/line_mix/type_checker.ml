open Syntax

let typing_error ~loc = Zoo.error ~kind:"Type error" ~loc
let linear_error ~loc = Zoo.error ~kind:"Linear error" ~loc


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
  | A -> "A"
  | B k -> Printf.sprintf "(A%s)" (string_of_int k)




let array_error ty = typing_error "this expression has type %s but is used as if it has type Array" (string_of_ltype ty)

let is_linear = function
  | LBang _ -> false
  | _ -> true

  let unwrap = function
  | LBang ty -> ty
  | ty -> ty

let remove_bound ~loc before after x = (
  (match Context.find_opt x after with
   | Some ty when is_linear ty -> linear_error ~loc "linear variable %s is never used" x
   | _ -> ());
  match Context.find_opt x before with
  | Some ty -> define after x ty
  | None -> remove after x
)


let consumes_linear before after =
  Context.exists (fun x ty -> is_linear ty && not (Context.mem x after)) before

let require_same_context ~loc ctx1 ctx2 =
  if not (Context.equal (=) ctx1 ctx2) then
    linear_error ~loc
      "the two branches must use at the end have the same context"

let require_sameish_type ~loc ty1 ty2 =
  match ty1, ty2 with
  | LBang ta, LBang tb 
  | LBang ta, tb 
  | ta, LBang tb 
  | ta, tb -> 
    if ta <> tb then
      typing_error ~loc "expected %s and %s to be the same or diferrented by !" (string_of_ltype ty1) (string_of_ltype ty2)


let merge_types ~loc ty1 ty2 =
  require_sameish_type ~loc ty1 ty2;
  match ty1, ty2 with
  | _, LBang _  -> ty1
  | _, _ -> ty2



  let rec indentify ~loc option want got =
    match want, got with
    | (A | B _), _ ->
        (match List.assoc_opt want option with
        | None -> (want, got)::option
        | Some t when t = got -> option
        | Some t -> typing_error ~loc "expected %s but got %s" (string_of_ltype t) (string_of_ltype got))
    | LInt, LInt | LBool, LBool -> option
    | LArr t1, LArr t2 | LBang t1, LBang t2 -> indentify ~loc option t1 t2
    | LLolli (a1, b1), LLolli (a2, b2)
    | LAnd (a1, b1), LAnd (a2, b2)
    | LWith (a1, b1), LWith (a2, b2)
    | LPlus (a1, b1), LPlus (a2, b2) -> indentify ~loc (indentify ~loc option a1 a2) b1 b2
    | _ -> typing_error ~loc "expected %s but got %s" (string_of_ltype want) (string_of_ltype got)



let rec subst_type option = function
  | (A | B _) as v -> (match List.assoc_opt v option with Some t -> t | None -> v)
  | LInt | LBool as t -> t
  | LArr t -> LArr (subst_type option t)
  | LBang t -> LBang (subst_type option t)
  | LLolli (a, b) -> LLolli (subst_type option a, subst_type option b)
  | LAnd (a, b)   -> LAnd   (subst_type option a, subst_type option b)
  | LWith (a, b)  -> LWith  (subst_type option a, subst_type option b)
  | LPlus (a, b)  -> LPlus  (subst_type option a, subst_type option b)


let rec check (ctx : context) ({Zoo.data = e'; loc} as e) ty cnt : context * int = 
  (match e', ty with
  | Inl e', LPlus (lty1, _) -> check ctx e' lty1 cnt
  | Inr e', LPlus (_, lty2) -> check ctx e' lty2 cnt
  | Inl _, lty' -> typing_error ~loc "inl is used at type %s, which is not a sum type" (string_of_ltype lty')
  | Inr _, lty' -> typing_error ~loc "inr is used at type %s, which is not a sum type" (string_of_ltype lty')
  | Array [], LArr _ -> ctx, cnt
  | _ -> 
    let ety, ctx1, cnt = infer ctx e cnt in
    if ety = ty 
      then
        ctx1, cnt
      else
      typing_error ~loc "expected type %s but got %s" (string_of_ltype ty) (string_of_ltype ety)
  )

and checkish ctx ({Zoo.data = _; loc} as e) ty cnt =
  let ety, ctx', cnt = infer ctx e cnt in
  require_sameish_type ~loc ety ty;
  ety, ctx', cnt


and infer (ctx : context) ({Zoo.data = e'; loc}) (cnt : int)  : ltype * context * int = 
  match e' with
  | Var x ->
      (match Context.find_opt x ctx with
       | Some ty when is_linear ty -> ty, Context.remove x ctx, cnt
       | Some ty -> ty, ctx, cnt
       | None -> typing_error ~loc "unbound variable %s" x)
  | Int _ -> LBang LInt, ctx, cnt
  | Bool _ -> LBang LBool, ctx, cnt
  | Annot (e1, ty) -> 
      let ctx, cnt = check ctx e1 ty cnt in
      ty, ctx, cnt

  | Inl e -> 
      let cnt = cnt + 1 in
      let ty, ctx1, cnt = infer ctx e cnt in
      LPlus (ty, B cnt), ctx1, cnt
  | Inr e -> 
      let cnt = cnt + 1 in
      let ty, ctx1, cnt = infer ctx e cnt in
      LPlus (B cnt, ty), ctx1, cnt
  | Match (scrut, x, e1, y, e2) ->
      let sty, ctx1, cnt = infer ctx scrut cnt in
      (match sty with
        | LPlus (ty1, ty2) -> 
           let ctx_left1 = define ctx1 x ty1 in
           let ctx_right1 = define ctx1 y ty2 in
           let ty_left, ctx_left2, cnt = infer ctx_left1 e1 cnt in
           let ctx_right2, cnt = check ctx_right1 e2 ty_left cnt in
           let ctx_left3 = remove_bound ~loc ctx1 ctx_left2 x in 
           let ctx_right3 = remove_bound ~loc ctx1 ctx_right2 y in 

           require_same_context ~loc ctx_left3 ctx_right3;
           ty_left, ctx_left3, cnt
        | ty -> typing_error ~loc "this expression has type %s but match expects a Sum type" (string_of_ltype ty)
      )

  | Fun (x, ty, body) -> 
      let ctx1 = define ctx x ty in
      let bty, ctx2, cnt = infer ctx1 body cnt in
      let ctx2 = remove_bound ~loc ctx ctx2 x in
      if consumes_linear ctx ctx2 
        then 
          LLolli (ty, bty), ctx2, cnt
        else
          LBang (LLolli (ty, bty)), ctx2, cnt
  | Apply (f, a) ->
    let fty, ctx1, cnt = infer ctx f cnt in
    (match fty with
     | LLolli (ty1, ty2) | LBang (LLolli (ty1, ty2)) ->
         let aty, ctx2, cnt = infer ctx1 a cnt in
         let s = indentify ~loc [] ty1 aty in
         subst_type s ty2, ctx2, cnt
     | ty -> typing_error ~loc "this expression has type %s but is applied as if it were a function" (string_of_ltype ty))

  
  | Array lst -> 
      let cnt = cnt + 1 in
      (match lst with
      | [] -> LArr (B cnt), ctx, cnt
      | e::es -> 
          let ty, ctx1, cnt = infer ctx e cnt in
          let rec aux ctx cnt = function
            | [] -> LArr ty, ctx, cnt
            | e::es ->
                let ctx2, cnt = check ctx e ty cnt in
                aux ctx2 cnt es
          in
          aux ctx1 cnt es)
  | Make -> 
      let cnt = cnt + 1 in
      LBang (LLolli (LInt, LLolli (LBang (B cnt), LArr (LBang (B cnt))))), ctx, cnt
  | Length ->
      let cnt = cnt + 1 in
      LBang (LLolli (LArr (B cnt), LAnd (LBang LInt, LArr (B cnt)))), ctx, cnt
  | Lookup ->
      let cnt = cnt + 1 in
      LBang (LLolli (LInt, LLolli (LArr (LBang (B cnt)), LAnd (LBang (B cnt), LArr (LBang (B cnt)))))), ctx, cnt
  | Modify -> 
      let cnt = cnt + 1 in
      LBang (LLolli (LInt, LLolli (LLolli (B cnt, B cnt) , LLolli (LArr (B cnt), LArr (B cnt))))), ctx, cnt
  | Get ->
      let cnt = cnt + 2 in
      LBang (LLolli (LInt, LLolli (LLolli (B cnt, LAnd (B (cnt + 1), B cnt)), LLolli (LArr (B cnt), LAnd (B (cnt + 1), LArr (B cnt)))))), ctx, cnt

  | Times (e1, e2) | Plus (e1, e2) | Divide (e1, e2) | Minus (e1, e2) | Mod (e1, e2) ->
      let ty1, ctx1, cnt = checkish ctx e1 LInt cnt in
      let ty2, ctx2, cnt = checkish ctx1 e2 ty1 cnt in
      merge_types ~loc ty1 ty2, ctx2, cnt

      (* VPRASAJ: Ali naj uporabim LBang LBool *)
  | Less (e1, e2) ->
      let _, ctx1, cnt = checkish ctx e1 LInt cnt in
      let _, ctx2, cnt = checkish ctx1 e2 LInt cnt in
      LBang LBool, ctx2  , cnt
  | Equal (e1, e2) -> 
      let ty, ctx1, cnt = infer ctx e1 cnt in
      let ctx2, cnt = check ctx1 e2 ty cnt in
      (match unwrap ty with
      | LInt | LBool | LAnd _| LPlus _ | LArr _ -> ()
      | _ -> typing_error ~loc "cannot compare %s" (string_of_ltype ty));
      LBang LBool, ctx2, cnt

  | If (cond, e1, e2) ->
      let ctx', cnt = check ctx cond LBool cnt in
      let ty1, ctx1, cnt = infer ctx' e1 cnt in
      let ctx2, cnt = check ctx' e2 ty1 cnt in
      require_same_context ~loc ctx1 ctx2;
      ty1, ctx1 , cnt

  | Demote e -> 
      let ty, ctx1, cnt = infer ctx e cnt in
      (match ty with
      | LBang ty' -> ty', ctx1, cnt
      | ty -> typing_error ~loc "expected !-type but got %s" (string_of_ltype ty))

  | Pair (e1, e2) -> 
      let ty1, ctx1, cnt = infer ctx e1 cnt in
      let ty2, ctx2, cnt = infer ctx1 e2 cnt in
      LAnd (ty1, ty2), ctx2, cnt
  | Split (pair, x, y, body) -> 
      let pair_ty, ctx1, cnt = infer ctx pair cnt in
      (match pair_ty with
      | LAnd (ty1, ty2) -> 
          let ctx' = define (define ctx1 x ty1) y ty2 in
          let ty, ctx2, cnt = infer ctx' body cnt in
          let ctx3 = remove_bound ~loc ctx1 (remove_bound ~loc ctx1 ctx2 x ) y in 
          ty, ctx3, cnt
        | ty -> typing_error ~loc "expected pair type but got %s" (string_of_ltype ty)
      )

  | Bundle (e1, e2) -> 
      let ty1, ctx1, cnt = infer ctx e1 cnt in
      let ty2, ctx2, cnt = infer ctx e2 cnt in
      require_same_context ~loc ctx1 ctx2;
      LWith (ty1, ty2), ctx1, cnt
  | Fst e -> (
      match infer ctx e cnt with
      | LWith (ta, _), ctx', cnt -> ta, ctx', cnt
      | ty, _, _ ->
        typing_error ~loc "this expression has type %s but fst expects a with-pair" (string_of_ltype ty)
    )
  | Snd e -> (
      match infer ctx e cnt with
      | LWith (_, tb), ctx', cnt -> tb, ctx', cnt
      | ty, _, _ ->
        typing_error ~loc "this expression has type %s but snd expects a with-pair" (string_of_ltype ty)
    )  



let check_rec ctx f a b e cnt =
  let ty = LBang (LLolli (a, b)) in
  let ctx1 = define ctx f ty in
  let ctx, cnt = check ctx1 e ty cnt in
  ty, ctx, cnt