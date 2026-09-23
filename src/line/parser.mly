%{
  open Utils.Syntax
%}

%token <Utils.Syntax.name> VAR
%token <int> INT
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE MOD EQUAL LESS
%token COMMA COLON SEMICOLON
%token LPAREN RPAREN
%token SPLIT TO IN LET
%token MATCH WITH INL INR ALTERNATIVE
%token LAMBDA
%token IF THEN ELSE
%token FST SND
%token TYPE_INT BOOL LOLLI AMP
%token EOF

%start file
%type <Utils.Syntax.toplevel_cmd list> file

%start toplevel
%type <Utils.Syntax.toplevel_cmd> toplevel

%type <Utils.Syntax.ltype> ltype

%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc EQUAL LESS

%%

file:
  | EOF
    { [] }
  | cmd = toplevel lst = file
    { cmd :: lst }

toplevel:
  | LET x = VAR EQUAL e = expr SEMICOLON
    { Def (x, e) }
  | e = expr SEMICOLON
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
  | LAMBDA x = VAR COLON t = ltype IN e = expr
  { Fun (x, t, e) }
  | MATCH scrut = expr WITH INL x = VAR EQUAL left = expr ALTERNATIVE INR y = VAR EQUAL right = expr
    { Match (scrut, x, left, y, right) }
  | IF cond = expr THEN e1 = expr ELSE e2 = expr
    { If (cond, e1, e2) }
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

(* Types, from loosest to tightest binding: s -o t (right assoc) ; s + t ; s & t ; s * t *)
ltype:
  | t1 = plus_type LOLLI t2 = ltype
    { LLolli (t1, t2) }
  | t = plus_type
    { t }

plus_type:
  | t1 = with_type PLUS t2 = plus_type
    { LPlus (t1, t2) }
  | t = with_type
    { t }

with_type:
  | t1 = tensor_type AMP t2 = with_type
    { LWith (t1, t2) }
  | t = tensor_type
    { t }

tensor_type:
  | t1 = atom_type TIMES t2 = tensor_type
    { LAnd (t1, t2) }
  | t = atom_type
    { t }

atom_type:
  | TYPE_INT
    { LInt }
  | BOOL
    { LBool }
  | LPAREN t = ltype RPAREN
    { t }
