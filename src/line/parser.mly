%{
  open Syntax
%}

%token LINT LEPHBOOL LPERBOOL LLOLI LAND LWITH LPLUS LPAR
%token <Syntax.name> VAR
%token <int> INT
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD
%token EQUAL LESS
%token AND
%token WITH
%token OPLUS
%token PAR
%token LOLI
%token TOP BOT
%token UNIT ZERO
%token LET
%token COLON
%token SEMICOLON2
%token LPAREN RPAREN
%token FST
%token SND
%token QUIT
%token EO


%start toplevel file
%type <Syntax.toplevel_cmd list> file
%type <Syntax.toplevel_cmd> toplevel

%%

file:
  | EOF         { [] }

