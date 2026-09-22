(** Type checking for Line, a small linear-logic-inspired language.

    Linear typing means every variable in scope must be used *exactly
    once*: not zero times, not twice. We track this with a context that
    maps in-scope variables to their type, and remove a variable the
    moment it is used ([infer] on [Var]); a binder ([Fun], [Split],
    [Match]) then checks that its own variable is gone from the leftover
    context once its body has been checked, via [with_var] below. If the
    binder's name shadows an outer variable of the same name, [with_var]
    restores the outer binding afterwards, so it remains available to
    the rest of the program.

    The two additive connectives, [LWith] (&) and sum elimination
    ([Match]), do not thread the context between their branches: instead
    both branches start from the *same* context (only one of them will
    ever run), and must leave behind the *same* leftover
    ([require_same_leftovers]). *)

open Utils.Syntax

let typing_error fmt = Zoo.error ~kind:"Type error" fmt
let linear_error fmt = Zoo.error ~kind:"Linear error" fmt

module Context = Map.Make (struct
  type t = name
  let compare = compare
end)

type context = ltype Context.t

let empty : context = Context.empty

(** Add (or overwrite) a binding without any linearity bookkeeping; used
    for global, top-level definitions, which are not required to be
    consumed by any single command. *)
let define ctx x ty = Context.add x ty ctx

let rec string_of_ltype = function
  | LInt -> "Int"
  | LBool -> "Bool"
  | LLolli (t1, t2) -> Printf.sprintf "(%s -o %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LAnd (t1, t2) -> Printf.sprintf "(%s * %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LWith (t1, t2) -> Printf.sprintf "(%s & %s)" (string_of_ltype t1) (string_of_ltype t2)
  | LPlus (t1, t2) -> Printf.sprintf "(%s + %s)" (string_of_ltype t1) (string_of_ltype t2)

(** [with_var ctx x ty f] binds [x : ty], runs [f] on the extended
    context to typecheck whatever [x] is in scope for, and checks that
    [x] was consumed (i.e. is no longer in the resulting context). If
    [x] was already bound in [ctx] (shadowing), that outer binding is
    restored in the leftover context once [x] itself has been checked
    off. *)
let with_var ctx x ty f =
  let shadowed = Context.find_opt x ctx in
  let result, leftover = f (Context.add x ty ctx) in
  if Context.mem x leftover then
    linear_error "linear variable %s is never used" x ;
  let leftover =
    match shadowed with
    | Some old_ty -> Context.add x old_ty leftover
    | None -> Context.remove x leftover
  in
  result, leftover

let require_same_context ctx1 ctx2 =
  if not (Context.equal (=) ctx1 ctx2) then
    linear_error
      "the two branches must use exactly the same resources from the surrounding context"

(** [infer ctx e] computes the type of [e] and the context left over once
    [e]'s resources have been consumed. *)
let rec infer ctx e : ltype * context =
  match e with
  | Var x ->
    (match Context.find_opt x ctx with
     | Some ty -> ty, Context.remove x ctx
     | None -> typing_error "unbound variable %s (or it was already used)" x)

  | Int _ -> LInt, ctx
  | Bool _ -> LBool, ctx

  | Times (e1, e2) | Divide (e1, e2) | Mod (e1, e2)
  | Plus (e1, e2) | Minus (e1, e2) ->
    let ctx1 = check ctx LInt e1 in
    let ctx2 = check ctx1 LInt e2 in
    LInt, ctx2

  | Equal (e1, e2) | Less (e1, e2) ->
    let ctx1 = check ctx LInt e1 in
    let ctx2 = check ctx1 LInt e2 in
    LBool, ctx2

  | Pair (e1, e2) ->
    let ty1, ctx1 = infer ctx e1 in
    let ty2, ctx2 = infer ctx1 e2 in
    LAnd (ty1, ty2), ctx2

  | Split (p, x, y, body) ->
    (match infer ctx p with
     | LAnd (tx, ty), ctx1 ->
       let ty_body, ctx2 =
         with_var ctx1 x tx (fun ctx ->
           with_var ctx y ty (fun ctx -> infer ctx body))
       in
       ty_body, ctx2
     | ty, _ ->
       typing_error "this expression has type %s but split expects a tensor product s * t"
         (string_of_ltype ty))

  | Fun (x, ty1, body) ->
    let ty2, ctx' = with_var ctx x ty1 (fun ctx -> infer ctx body) in
    LLolli (ty1, ty2), ctx'

  | Apply (f, a) ->
    (match infer ctx f with
     | LLolli (ty1, ty2), ctx1 ->
       let ctx2 = check ctx1 ty1 a in
       ty2, ctx2
     | ty, _ ->
       typing_error "this expression has type %s but is applied as if it were a function"
         (string_of_ltype ty))

  | Inl _ | Inr _ ->
    typing_error
      "cannot infer the type of %s: its type must be known from context (e.g. as a function argument, or inside split/match)"
      (match e with Inl _ -> "inl e" | _ -> "inr e")

  | Match (scrut, x, e1, y, e2) ->
    (match infer ctx scrut with
     | LPlus (ta, tb), ctx1 ->
       let ty1, ctx_left = with_var ctx1 x ta (fun ctx -> infer ctx e1) in
       let ctx_right = with_var ctx1 y tb (fun ctx -> (), check ctx ty1 e2) |> snd in
       require_same_context ctx_left ctx_right ;
       ty1, ctx_left
     | ty, _ ->
       typing_error "this expression has type %s but match expects a sum type s + t"
         (string_of_ltype ty))

  | Bundle (e1, e2) ->
    let ty1, ctx1 = infer ctx e1 in
    let ty2, ctx2 = infer ctx e2 in
    require_same_context ctx1 ctx2 ;
    LWith (ty1, ty2), ctx1

  | Fst e ->
    (match infer ctx e with
     | LWith (ta, _), ctx' -> ta, ctx'
     | ty, _ ->
       typing_error "this expression has type %s but fst expects a with-pair s & t"
         (string_of_ltype ty))

  | Snd e ->
    (match infer ctx e with
     | LWith (_, tb), ctx' -> tb, ctx'
     | ty, _ ->
       typing_error "this expression has type %s but snd expects a with-pair s & t"
         (string_of_ltype ty))

(** [check ctx ty e] verifies that [e] has type [ty], returning the
    leftover context. Most expressions are handled by inferring their
    type and comparing it to [ty], exactly like miniml's [check]. The
    sum injections [Inl]/[Inr] are the exception: they carry no type
    annotation of their own; the "other side" of the sum can only come
    from the expected type, so they must be checked, never inferred. *)
and check ctx ty e : context =
  match e, ty with
  | Inl e', LPlus (ta, _) -> check ctx ta e'
  | Inl _, ty -> typing_error "inl is used at type %s, which is not a sum type" (string_of_ltype ty)
  | Inr e', LPlus (_, tb) -> check ctx tb e'
  | Inr _, ty -> typing_error "inr is used at type %s, which is not a sum type" (string_of_ltype ty)
  | _, _ ->
    let ty', ctx' = infer ctx e in
    if ty' <> ty then
      typing_error "this expression has type %s but is used as if it has type %s"
        (string_of_ltype ty') (string_of_ltype ty)
    else
      ctx'
