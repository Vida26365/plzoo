{
  open Parser
  open Lexing
}

let var = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
    "--" [^'\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t']      { token lexbuf }
  | ['0'-'9']+      { INT (int_of_string(lexeme lexbuf)) }
  | "int"           { LINT }
  | "ephemeral"     { LEPHBOOL }
  | "persistant"    { LPERBOOL }
  | "let"           { LET }
  | ":quit"         { QUIT }
  | "with"          { WITH }
  | ";;"            { SEMICOLON2 }
  | '%'             { MOD }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '*'             { TIMES }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '/'             { DIVIDE }
  | ':'             { COLON }
  | '<'             { LESS }
  | '='             { EQUAL }
  | "&"             { AND }
  | "⊗"             { AND }
  | "⊕"             { OPLUS }
  | "⅋"             { PAR }
  | "⊸"             { LOLI }
  | "Τ"|"T"         { TOP }
  | "𝈜"|"_|_"       { BOT }
  | "1"             { UNIT }
  | "0"             { ZERO }
  | var             { VAR (lexeme lexbuf) }
  | eof             { EOF }

{
}