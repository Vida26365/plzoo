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

  (** A linear variable may not be redefined before it is used. *)
  let check_redefinition ctx x =
    match Type_checker.Context.find_opt x ctx with
    | Some ty when not (Type_checker.is_bang_type ty) ->
      Type_checker.linear_error "linear variable %s is redefined before it is used" x
    | _ -> ()

  let exec (ctx, env) = function
    | Syntax.Expr e ->
      let ty, ctx' = Type_checker.infer ctx e in
      let value = Interpret.interp env e in
      Zoo.print_info "%s : %s@." (Interpret.str_of_value value) (Type_checker.string_of_ltype ty) ;
      (ctx', env)
    | Syntax.DefRec (x, ty, e) ->
      check_redefinition ctx x ;
      let ctx' = Type_checker.check_rec ctx x ty e in
      let v = Interpret.make_rec x (Interpret.interp env e) in
      Zoo.print_info "%s : %s@." x (Type_checker.string_of_ltype ty) ;
      (Type_checker.define ctx' x ty, Interpret.Environment.add x v env)
    | Syntax.Def (x, e) ->
      check_redefinition ctx x ;
      let ty, ctx' = Type_checker.infer ctx e in
      let ty = Type_checker.default_bang e ty in
      let v = Interpret.interp env e in
      Zoo.print_info "%s : %s@." x (Type_checker.string_of_ltype ty) ;
      (Type_checker.define ctx' x ty, Interpret.Environment.add x v env)
    | Syntax.Quit -> exit 0

end) ;;


Line.main ()