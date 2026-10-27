module Line = Zoo.Main(struct
  let name = "line_mix"

  type command = Syntax.toplevel_cmd

  (** The type context of globally defined values. *)
  type context = Type_checker.context

  (** The runtime environment of globally defined values. *)
  type runtime = Interpret.environment

  type environment = context * runtime


  let options = []

  let initial_environment = (Type_checker.empty, Interpret.Environment.empty)

  let file_parser = Some (fun _ -> Parser.file Lexer.token)

  let toplevel_parser = Some (fun _ -> Parser.toplevel Lexer.token)

    let check_redefinition ctx x =
    match Type_checker.Context.find_opt x ctx with
    | Some ty -> if Type_checker.is_linear ty then
      Type_checker.linear_error "linear variable %s is redefined before it is used" x
    | _ -> ()


(* What to do in exec: *)
(* - type check *)
(* - evaluate *)
(* - return the updated environment *)
  let exec (ctx, env) = function
    | Syntax.Expr e ->
        let ty, ctx' = Type_checker.infer ctx e in 
        let value = Interpret.interp env e in
        Zoo.print_info "%s : %s@." (Interpret.str_of_value value) (Type_checker.string_of_ltype ty) ;
      (ctx', env)

    | Syntax.Def (x, e) -> 
        check_redefinition ctx x ;
        let ty, ctx' = Type_checker.infer ctx e in
        let v = Interpret.interp env e in
        Zoo.print_info "%s : %s@." x (Type_checker.string_of_ltype ty) ;
        (Type_checker.define ctx' x ty, Interpret.define x v env)
    
      
    | Syntax.DefRec (f, a, b, e) -> 
    (* add !(A -o B) in context and check if the function is of type !(A -o B) *)
        check_redefinition ctx f ;
        let ty, ctx1 = Type_checker.check_rec ctx f a b e in
        let value = Interpret.interp env e in
        let v = Interpret.make_rec f value in
        Zoo.print_info "%s : %s@." f (Type_checker.string_of_ltype ty) ;
        (Type_checker.define ctx1 f ty, Interpret.define f v env)
      
    | Syntax.DefHold (x, e) ->
        check_redefinition ctx x ;
        let ty, ctx' = Type_checker.infer ctx e in
        let ty = match ty with
        | LBang ty -> ty
        | ty -> ty in
        let v = Interpret.interp env e in
        Zoo.print_info "%s : %s@." x (Type_checker.string_of_ltype ty) ;
        (Type_checker.define ctx' x ty, Interpret.define x v env)

    | Syntax.Skip -> (ctx, env)

    | Syntax.Quit -> exit 0




end) ;;


Line.main ()