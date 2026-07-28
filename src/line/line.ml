module Line = Zoo.Main(struct
  let name = "line"

  type command = Utils.Syntax.toplevel_cmd

  type environment = Utils.Interpret.environment

  let options = []

  let initial_environment = Utils.Interpret.Environment.empty

  let file_parser = Some (fun _ -> Parser.file Lexer.token)

  let toplevel_parser = Some (fun _ -> Parser.toplevel Lexer.token)

  let exec _ _ = initial_environment (* TODO *)

end) ;;


Line.main ()