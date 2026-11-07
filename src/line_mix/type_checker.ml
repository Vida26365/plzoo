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




(** Global counter for fresh type variables. *)
let counter = ref 0
let fresh () = incr counter; B !counter

(** Global substitution: B n ↦ type. *)
let subst : (int, ltype) Hashtbl.t = Hashtbl.create 16

(** Apply the substitution all the way down. *)
let rec resolve = function
  | B n as t ->
      (match Hashtbl.find_opt subst n with
       | Some t' -> resolve t'
       | None -> t)
  | LInt | LBool | A as t -> t
  | LArr t -> LArr (resolve t)
  | LBang t -> LBang (resolve t)
  | LLolli (a, b) -> LLolli (resolve a, resolve b)
  | LAnd (a, b) -> LAnd (resolve a, resolve b)
  | LWith (a, b) -> LWith (resolve a, resolve b)
  | LPlus (a, b) -> LPlus (resolve a, resolve b)

let rec occurs n t =
  match resolve t with
  | B m -> n = m
  | LArr t | LBang t -> occurs n t
  | LLolli (a, b) | LAnd (a, b) | LWith (a, b) | LPlus (a, b) -> occurs n a || occurs n b
  | LInt | LBool | A -> false

(** [unify ~loc want got]: [got] may be used where [want] is expected.
    A !t may be used where t is expected. *)
let rec unify ~loc want got =
  match resolve want, resolve got with
  | B n, B m when n = m -> ()
  | B n, t | t, B n ->
      if occurs n t then
        typing_error ~loc "cannot construct infinite type %s = %s"
          (string_of_ltype (B n)) (string_of_ltype t);
      Hashtbl.replace subst n t
  | LInt, LInt | LBool, LBool | A, A -> ()
  | LBang a, LBang b -> unify ~loc a b
  | a, LBang b -> unify ~loc a b
  | LArr a, LArr b -> unify ~loc a b
  | LLolli (a1, b1), LLolli (a2, b2)
  | LAnd (a1, b1), LAnd (a2, b2)
  | LWith (a1, b1), LWith (a2, b2)
  | LPlus (a1, b1), LPlus (a2, b2) -> unify ~loc a1 a2; unify ~loc b1 b2
  | w, g ->
      typing_error ~loc "expected type %s but got %s"
        (string_of_ltype (resolve w)) (string_of_ltype (resolve g))


let array_error ty = typing_error "this expression has type %s but is used as if it has type Array" (string_of_ltype ty)

let is_linear ty =
  match resolve ty with
  | LBang _ -> false
  | _ -> true

let unwrap ty =
  match resolve ty with
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
  if not (Context.equal (fun t1 t2 -> resolve t1 = resolve t2) ctx1 ctx2) then
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


let rec check (ctx : context) ({Zoo.data = e'; loc} as e) ty : context =
  match e', resolve ty with
  | Inl e', LPlus (lty1, _) -> check ctx e' lty1
  | Inr e', LPlus (_, lty2) -> check ctx e' lty2
  | Inl _, lty' -> typing_error ~loc "inl is used at type %s, which is not a sum type" (string_of_ltype lty')
  | Inr _, lty' -> typing_error ~loc "inr is used at type %s, which is not a sum type" (string_of_ltype lty')
  | Array [], LArr _ -> ctx
  | _ ->
      let ety, ctx1 = infer ctx e in
      unify ~loc ty ety;
      ctx1

and checkish ctx ({Zoo.data = _; loc} as e) ty =
  let ety, ctx' = infer ctx e in
  unify ~loc (unwrap ty) (unwrap ety);
  resolve ety, ctx'

and infer (ctx : context) ({Zoo.data = e'; loc}) : ltype * context =
  match e' with
  | Var x ->
      (match Context.find_opt x ctx with
       | Some ty when is_linear ty -> resolve ty, Context.remove x ctx
       | Some ty -> resolve ty, ctx
       | None -> typing_error ~loc "unbound variable %s" x)
  | Int _ -> LBang LInt, ctx
  | Bool _ -> LBang LBool, ctx
  | Annot (e1, ty) ->
      let ctx = check ctx e1 ty in
      ty, ctx

  | Inl e ->
      let ty, ctx1 = infer ctx e in
      LPlus (ty, fresh ()), ctx1
  | Inr e ->
      let ty, ctx1 = infer ctx e in
      LPlus (fresh (), ty), ctx1
  | Match (scrut, x, e1, y, e2) ->
      let sty, ctx1 = infer ctx scrut in
      (match resolve sty with
       | LPlus (ty1, ty2) ->
           let ctx_left1 = define ctx1 x ty1 in
           let ctx_right1 = define ctx1 y ty2 in
           let ty_left, ctx_left2 = infer ctx_left1 e1 in
           let ctx_right2 = check ctx_right1 e2 ty_left in
           let ctx_left3 = remove_bound ~loc ctx1 ctx_left2 x in
           let ctx_right3 = remove_bound ~loc ctx1 ctx_right2 y in
           require_same_context ~loc ctx_left3 ctx_right3;
           resolve ty_left, ctx_left3
       | ty -> typing_error ~loc "this expression has type %s but match expects a Sum type" (string_of_ltype ty))

  | Fun (x, ty, body) ->
      let ctx1 = define ctx x ty in
      let bty, ctx2 = infer ctx1 body in
      let ctx2 = remove_bound ~loc ctx ctx2 x in
      if consumes_linear ctx ctx2
      then LLolli (ty, bty), ctx2
      else LBang (LLolli (ty, bty)), ctx2
  | Apply (f, a) ->
      let fty, ctx1 = infer ctx f in
      (match resolve fty with
       | LLolli (ty1, ty2) | LBang (LLolli (ty1, ty2)) ->
           let aty, ctx2 = infer ctx1 a in
           unify ~loc ty1 aty;
           resolve ty2, ctx2
       | ty -> typing_error ~loc "this expression has type %s but is applied as if it were a function" (string_of_ltype ty))

  | Array lst ->
      (match lst with
       | [] -> LArr (fresh ()), ctx
       | e :: es ->
           let ty, ctx1 = infer ctx e in
           let rec aux ctx = function
             | [] -> LArr (resolve ty), ctx
             | e :: es ->
                 let ctx2 = check ctx e ty in
                 aux ctx2 es
           in
           aux ctx1 es)
  | Make ->
      let a = fresh () in
      LBang (LLolli (LInt, LLolli (LBang a, LArr (LBang a)))), ctx
  | Length ->
      let a = fresh () in
      LBang (LLolli (LArr a, LAnd (LBang LInt, LArr a))), ctx
  | Lookup ->
      let a = fresh () in
      LBang (LLolli (LInt, LLolli (LArr (LBang a), LAnd (LBang a, LArr (LBang a))))), ctx
  | Modify ->
      let a = fresh () in
      LBang (LLolli (LInt, LLolli (LLolli (a, a), LLolli (LArr a, LArr a)))), ctx
  | Get ->
      let a = fresh () in
      let b = fresh () in
      LBang (LLolli (LInt, LLolli (LLolli (a, LAnd (b, a)), LLolli (LArr a, LAnd (b, LArr a))))), ctx

  | Times (e1, e2) | Plus (e1, e2) | Divide (e1, e2) | Minus (e1, e2) | Mod (e1, e2) ->
      let ty1, ctx1 = checkish ctx e1 LInt in
      let ty2, ctx2 = checkish ctx1 e2 ty1 in
      merge_types ~loc ty1 ty2, ctx2

  | Less (e1, e2) ->
      let _, ctx1 = checkish ctx e1 LInt in
      let _, ctx2 = checkish ctx1 e2 LInt in
      LBang LBool, ctx2
  | Equal (e1, e2) ->
      let ty, ctx1 = infer ctx e1 in
      let ctx2 = check ctx1 e2 ty in
      (match unwrap ty with
       | LInt | LBool | LAnd _ | LPlus _ | LArr _ -> ()
       | _ -> typing_error ~loc "cannot compare %s" (string_of_ltype (resolve ty)));
      LBang LBool, ctx2

  | If (cond, e1, e2) ->
      let ctx' = check ctx cond LBool in
      let ty1, ctx1 = infer ctx' e1 in
      let ctx2 = check ctx' e2 ty1 in
      require_same_context ~loc ctx1 ctx2;
      resolve ty1, ctx1

  | Demote e ->
      let ty, ctx1 = infer ctx e in
      (match resolve ty with
       | LBang ty' -> ty', ctx1
       | ty -> typing_error ~loc "expected !-type but got %s" (string_of_ltype ty))

  | Pair (e1, e2) ->
      let ty1, ctx1 = infer ctx e1 in
      let ty2, ctx2 = infer ctx1 e2 in
      LAnd (ty1, ty2), ctx2
  | Split (pair, x, y, body) ->
      let pair_ty, ctx1 = infer ctx pair in
      (match resolve pair_ty with
       | LAnd (ty1, ty2) ->
           let ctx' = define (define ctx1 x ty1) y ty2 in
           let ty, ctx2 = infer ctx' body in
           let ctx3 = remove_bound ~loc ctx1 (remove_bound ~loc ctx1 ctx2 x) y in
           ty, ctx3
       | ty -> typing_error ~loc "expected pair type but got %s" (string_of_ltype ty))

  | Bundle (e1, e2) ->
      let ty1, ctx1 = infer ctx e1 in
      let ty2, ctx2 = infer ctx e2 in
      require_same_context ~loc ctx1 ctx2;
      LWith (ty1, ty2), ctx1
  | Fst e ->
      let ty, ctx' = infer ctx e in
      (match resolve ty with
       | LWith (ta, _) -> ta, ctx'
       | ty -> typing_error ~loc "this expression has type %s but fst expects a with-pair" (string_of_ltype ty))
  | Snd e ->
      let ty, ctx' = infer ctx e in
      (match resolve ty with
       | LWith (_, tb) -> tb, ctx'
       | ty -> typing_error ~loc "this expression has type %s but snd expects a with-pair" (string_of_ltype ty))


let check_rec ctx f a b e =
  let ty = LBang (LLolli (a, b)) in
  let ctx1 = define ctx f ty in
  let ctx = check ctx1 e ty in
  ty, ctx
