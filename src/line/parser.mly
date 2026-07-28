%{
  open Utils.Syntax
%}

%token <Utils.Syntax.name> VAR
%token <int> INT
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE MOD EQUAL LESS
%token COMMA COLON
%token LPAREN RPAREN
%token SPLIT TO IN LET
%token MATCH WITH INL INR ALTERNATIVE
%token LAMBDA
%token FST SND
%token EOF

%start file
%type <Utils.Syntax.toplevel_cmd list> file

%start toplevel
%type <Utils.Syntax.toplevel_cmd> toplevel

%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc EQUAL LESS

%%

file:
  | EOF
    { [] }
  | cmd = toplevel EOF
    { [cmd] }

toplevel:
  | LET x = VAR EQUAL e = expr
    { Def (x, e) }
  | e = expr
    { Expr e }

expr: plain_expr { $1 }

plain_expr:
  | n = INT
    { Int n }
  | TRUE
    { Bool true }
  | FALSE
    { Bool false }
  | x = VAR
    { Var x }
  | LAMBDA x = VAR IN e = expr
  { Fun (x, e) }
  | MATCH scrut = expr WITH INL x = VAR EQUAL left = expr ALTERNATIVE INR y = VAR EQUAL right = expr
    { Match (scrut, x, left, y, right) }
  | SPLIT pair = expr TO x = VAR y = VAR IN body = expr
    { Split (pair, x, y, body) }
  | INL e = expr
    { Inl e }
  | INR e = expr
    { Inr e }
  | FST e = expr
    { Fst e }
  | SND e = expr
    { Snd e }
  | e1 = expr EQUAL e2 = expr
    { Equal (e1, e2) }
  | e1 = expr LESS e2 = expr
    { Less (e1, e2) }
  | e1 = expr PLUS e2 = expr
    { Plus (e1, e2) }
  | e1 = expr MINUS e2 = expr
    { Minus (e1, e2) }
  | e1 = expr TIMES e2 = expr
    { Times (e1, e2) }
  | e1 = expr DIVIDE e2 = expr
    { Divide (e1, e2) }
  | e1 = expr MOD e2 = expr
    { Mod (e1, e2) }
  | f = app_expr a = atom_expr
    { Apply (f, a) }
  | e = app_expr
    { e }

app_expr:
  | e = atom_expr
    { e }
  | f = app_expr a = atom_expr
    { Apply (f, a) }

atom_expr:
  | LPAREN e = expr RPAREN
    { e }
  | LPAREN e1 = expr COMMA e2 = expr RPAREN
    { Pair (e1, e2) }
  | x = VAR
    { Var x }
  | n = INT
    { Int n }
  | TRUE
    { Bool true }
  | FALSE
    { Bool false }

