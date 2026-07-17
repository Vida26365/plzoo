module Line = Zoo.Main(struct

  let name = "Line"

  type command = Syntax.toplevel_cmd

  type environment = (string * Syntax.ltype) list * Interpret.environment

  let print_depth = ref 100

  let options = [("-p", Arg.Int (fun n -> print_depth := n), "set print depth")]

  let initial_environment = ([], [])

  let file_parser = Some (fun _ -> Parser.file Lexer.token)

  let toplevel_parser = Some (fun _ -> Parser.toplevel Lexer.token)

  let exec (ctx, env) = function
    | Syntax.Expr e ->
      (* evaluate and print result *)
       let v = Interpret.interp env e in
       Zoo.print_info "- : %s = " (Syntax.string_of_expression v) ;
       Zoo.print_info "@." ;
       (ctx, env)
    | Syntax.Def (x, e) ->
       (* store definition *)
       Zoo.print_info "val %s@." x ;
       ((x, Syntax.LInt)::ctx, (x, ref (Interpret.VClosure (env,e)))::env)
    | Syntax.Quit -> raise End_of_file

end) ;;

MiniLinear.main () ;;