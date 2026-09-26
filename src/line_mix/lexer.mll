{
  open Parser
  open Lexing
}

let var = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
    '#' [^'\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']      { token lexbuf }
  | '-'? ['0'-'9']+ { INT (int_of_string(lexeme lexbuf)) }

  | "true"          { TRUE }
  | "false"         { FALSE }

  | "Int"           { TYPE_INT }
  | "Bool"          { TYPE_BOOL }
  | "Arr"           { TYPE_ARR }

  | "fst"           { FST }
  | "snd"           { SND }

  | "let"           { LET } 

  | "match"         { MATCH }
  | "with"          { WITH }
  | "inl"           { INL }
  | "inr"           { INR }

  | "fun"           { FUN }

  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }

  | "make"          { MAKE }
  | "length"        { LENGTH }
  | "lookup"        { LOOKUP }
  | "set"           { SET }
  


  | "!"             { BANG }
  | '%'             { MOD }
  | '&'             { AMP }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '*'             { TIMES }
  | '+'             { PLUS }
  | ','             { COMMA }
  | "-o"            { LOLLI }
  | "->"            { ARROW }
  | '-'             { MINUS }
  | '/'             { DIVIDE }
  | '<'             { LESS }
  | '='             { EQUAL }
  | '|'             { ALTERNATIVE }
  | ':'             { COLON }
  | ';'             { SEMICOLON }
  | '['             { LBRACKET } 
  | ']'             { RBRACKET }
  | var             { VAR (lexeme lexbuf) }  (* Zakaj je to *)
  | eof             { EOF }

