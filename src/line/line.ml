module Line = Zoo.Main(struct
  let name = "line"

  type command = Utils.Syntax.toplevel_cmd

  (** The type context of globally defined values. *)
  type context = Type_checker.context

  (** The runtime environment of globally defined values. *)
  type runtime = Interpret.environment

  type environment = context * runtime

  let options = []

  let initial_environment = (Type_checker.empty, Interpret.Environment.empty)

  let file_parser = Some (fun _ -> Parser.file Lexer.token)

  let toplevel_parser = Some (fun _ -> Parser.toplevel Lexer.token)

  let exec (ctx, env) = function
    | Utils.Syntax.Expr e ->
      let ty, _ = Type_checker.infer ctx e in
      let _ = Interpret.interp env e in
      Zoo.print_info "- : %s@." (Type_checker.string_of_ltype ty) ;
      (ctx, env)
    | Utils.Syntax.Def (x, e) ->
      let ty, _ = Type_checker.infer ctx e in
      let v = Interpret.interp env e in
      Zoo.print_info "%s : %s@." x (Type_checker.string_of_ltype ty) ;
      (Type_checker.define ctx x ty, Interpret.Environment.add x v env)
    | Utils.Syntax.Quit -> exit 0

end) ;;


Line.main ()