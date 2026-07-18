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

%token COLON
%token LPAREN RPAREN

%token SPLIT TO IN
%token LET

%token INL
%token INR

%token BUNDLE FROM 

%token FST
%token SND

%token QUIT
%token EOF


%start toplevel file
%type <Syntax.toplevel_cmd list> file
%type <Syntax.toplevel_cmd> toplevel

%%

file:
  | EOF         { [] }

