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

  | "fst"           { FST }
  | "snd"           { SND }

  | "let"           { LET } 

  | "match"         { MATCH }
  | "with"          { WITH }
  | "inl"           { INL }
  | "inr"           { INR }

  | "split"         {SPLIT}
  | "to"            { TO }
  | "in"            { IN }

  | "lambda"        { LAMBDA }
  


  
  | '%'             { MOD }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '*'             { TIMES }
  | '+'             { PLUS }
  | ','             { COMMA }
  | '-'             { MINUS }
  | '/'             { DIVIDE }
  | '<'             { LESS }
  | '='             { EQUAL }
  | '|'             { ALTERNATIVE }
  | ':'             { COLON }
  | var             { VAR (lexeme lexbuf) }
  | eof             { EOF }
