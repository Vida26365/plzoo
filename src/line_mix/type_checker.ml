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


let string_of_ltype t =
    let rec aux p t =
    let lvl, s =
        match t with
        | LInt -> 6, "Int"
        | LBool -> 6, "Bool"
        | B k -> 6, "A" ^ string_of_int k
        | LBang t -> 6, "!" ^ aux 6 t
        | LArr t -> 5, aux 5 t ^ " Array"
        | LAnd (a, b) -> 4, aux 5 a ^ " * " ^ aux 4 b
        | LWith (a, b) -> 3, aux 4 a ^ " & " ^ aux 3 b
        | LPlus (a, b) -> 2, aux 3 a ^ " + " ^ aux 2 b
        | LLolli (a, b) -> 0, aux 2 a ^ " -o " ^ aux 0 b
    in
    if lvl < p then "(" ^ s ^ ")" else s
    in
    aux 0 t


(** Builtin functions (lookup, modify, ...) can accept any type of array.
Solution: Each time a builtin function is used a new type is created, 
that gets later substituted to the intended type
*)

(** Global counter for fresh type variables. *)
let counter = ref 0
let fresh () = incr counter; B !counter

(** Hash table holds what type unknown type is *)
let subst : (int, ltype) Hashtbl.t = Hashtbl.create 16

(** Apply the substitution of types all the way down. *)
let rec substitute = function
  | B n as t ->
      (match Hashtbl.find_opt subst n with
       | Some t' -> substitute t'
       | None -> t)
  | LInt | LBool  as t -> t
  | LArr t -> LArr (substitute t)
  | LBang t -> LBang (substitute t)
  | LLolli (a, b) -> LLolli (substitute a, substitute b)
  | LAnd (a, b) -> LAnd (substitute a, substitute b)
  | LWith (a, b) -> LWith (substitute a, substitute b)
  | LPlus (a, b) -> LPlus (substitute a, substitute b)

let rec occurs n t =
  match substitute t with
  | B m -> n = m
  | LArr t | LBang t -> occurs n t
  | LLolli (a, b) | LAnd (a, b) | LWith (a, b) | LPlus (a, b) -> occurs n a || occurs n b
  | LInt | LBool  -> false

(** Tries to identify if the type that we have is the type that we want.
    A !t may be used where t is expected. *)
let rec identify ~loc want got =
    match substitute want, substitute got with
    | B n, B m when n = m -> ()
    | B n, t | t, B n ->
        if occurs n t then (* this never happens*)
            typing_error ~loc "cannot construct infinite type %s = %s"
                (string_of_ltype (B n)) (string_of_ltype t);
        (* Got idea what the unkown type should be? Add it to hashtable. *)
        Hashtbl.replace subst n t
    | LInt, LInt | LBool, LBool  -> ()
    | LBang a, LBang b -> identify ~loc a b
    | a, LBang b -> identify ~loc a b
    | LArr a, LArr b -> identify ~loc a b
    | LLolli (a1, b1), LLolli (a2, b2)
    | LAnd (a1, b1), LAnd (a2, b2)
    | LWith (a1, b1), LWith (a2, b2)
    | LPlus (a1, b1), LPlus (a2, b2) -> identify ~loc a1 a2; identify ~loc b1 b2
    | w, g ->
        typing_error ~loc "expected type %s but got %s"
        (string_of_ltype (substitute w)) (string_of_ltype (substitute g))


let is_linear ty =
    match substitute ty with
    | LBang _ -> false
    | _ -> true

let unwrap ty =
    match substitute ty with
    | LBang ty -> ty
    | ty -> ty


(** Removes bounded variable. If linear variable isnt used (for example in a function or a match) raise error *)
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
    if not (Context.equal (fun t1 t2 -> substitute t1 = substitute t2) ctx1 ctx2) then
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



let rec check (ctx : context) ({Zoo.data = e'; loc} as e) ty : context =
    match e', substitute ty with
    | Inl e', LPlus (lty1, _) -> check ctx e' lty1
    | Inr e', LPlus (_, lty2) -> check ctx e' lty2
    | Inl _, lty' -> typing_error ~loc "inl is used at type %s, which is not a sum type" (string_of_ltype lty')
    | Inr _, lty' -> typing_error ~loc "inr is used at type %s, which is not a sum type" (string_of_ltype lty')
    | Array [], LArr _ -> ctx
    | _ ->
        let ety, ctx1 = infer ctx e in
        identify ~loc ty ety;
        ctx1

and checkish ctx ({Zoo.data = _; loc} as e) ty =
    let ety, ctx' = infer ctx e in
    identify ~loc (unwrap ty) (unwrap ety);
    substitute ety, ctx'

(** Tryes to infer type of an expression *)
and infer (ctx : context) ({Zoo.data = e'; loc}) : ltype * context =
    match e' with
    | Var x ->
        (match Context.find_opt x ctx with
        | Some ty when is_linear ty -> substitute ty, Context.remove x ctx
        | Some ty -> substitute ty, ctx
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
        (match substitute sty with
        | LPlus (ty1, ty2) ->
            let ctx_left1 = define ctx1 x ty1 in
            let ty_left, ctx_left2 = infer ctx_left1 e1 in
            let ctx_left3 = remove_bound ~loc ctx1 ctx_left2 x in

            let ctx_right1 = define ctx1 y ty2 in
            let ctx_right2 = check ctx_right1 e2 ty_left in
            let ctx_right3 = remove_bound ~loc ctx1 ctx_right2 y in
            
            require_same_context ~loc ctx_left3 ctx_right3;
            substitute ty_left, ctx_left3
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
        (match substitute fty with
        | LLolli (ty1, ty2) | LBang (LLolli (ty1, ty2)) ->
            let aty, ctx2 = infer ctx1 a in
            identify ~loc ty1 aty;
            substitute ty2, ctx2
        | ty -> typing_error ~loc "this expression has type %s but is applied as if it were a function" (string_of_ltype ty))
    | Let (x, e1, e2) ->
        let ty1, ctx1 = infer ctx e1 in
        let ctx2 = define ctx1 x ty1 in
        let ty2, ctx3 = infer ctx2 e2 in
        let ctx4 = remove_bound ~loc ctx1 ctx3 x in
        ty2, ctx4


    | Array lst ->
        (match lst with
        | [] -> LArr (fresh ()), ctx
        | e :: es ->
            let ty, ctx1 = infer ctx e in
            let rec aux ctx = function
                | [] -> LArr (substitute ty), ctx
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
        | _ -> typing_error ~loc "cannot compare %s" (string_of_ltype (substitute ty)));
        LBang LBool, ctx2

    | If (cond, e1, e2) ->
        let ctx' = check ctx cond LBool in
        let ty1, ctx1 = infer ctx' e1 in
        let ctx2 = check ctx' e2 ty1 in
        require_same_context ~loc ctx1 ctx2;
        substitute ty1, ctx1

    | Demote e ->
        let ty, ctx1 = infer ctx e in
        (match substitute ty with
        | LBang ty' -> ty', ctx1
        | ty -> typing_error ~loc "expected !-type but got %s" (string_of_ltype ty))

    | Pair (e1, e2) ->
        let ty1, ctx1 = infer ctx e1 in
        let ty2, ctx2 = infer ctx1 e2 in
        LAnd (ty1, ty2), ctx2
    | Split (pair, x, y, body) ->
        let pair_ty, ctx1 = infer ctx pair in
        (match substitute pair_ty with
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
        (match substitute ty with
        | LWith (ta, _) -> ta, ctx'
        | ty -> typing_error ~loc "this expression has type %s but fst expects a with-pair" (string_of_ltype ty))
    | Snd e ->
        let ty, ctx' = infer ctx e in
        (match substitute ty with
        | LWith (_, tb) -> tb, ctx'
        | ty -> typing_error ~loc "this expression has type %s but snd expects a with-pair" (string_of_ltype ty))

    | Kill (x, e) -> 
        let ctx1 = remove ctx x in
        infer ctx1 e

let check_rec ctx f a b e =
    let ty = LBang (LLolli (a, b)) in
    let ctx1 = define ctx f ty in
    let ctx = check ctx1 e ty in
    ty, ctx


    (** Raise an error if any linear variable is still in the context, i.e. was never used. *)
let check_all_used ?loc ctx =
  let unused = Context.filter (fun _ ty -> is_linear ty) ctx in
  if not (Context.is_empty unused) then
    let names = List.map fst (Context.bindings unused) in
    Zoo.error ~kind:"Linear error" ?loc "linear variables never used: %s" (String.concat ", " names)
