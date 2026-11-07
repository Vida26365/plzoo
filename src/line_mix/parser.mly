%{
  open Syntax
%}

%token <Syntax.name> VAR
%token <int> INT
%token NEWLINE

%token TRUE FALSE

%token PLUS MINUS TIMES DIVIDE MOD EQUAL LESS

%token COLON COMMA
%token LPAREN RPAREN LBRACKET RBRACKET
%token BANG CARET
%token LET REC TO FROM
%token MATCH WITH INL INR ALTERNATIVE
%token FUN ARROW
%token IF THEN ELSE
%token FST SND
%token MAKE LENGTH LOOKUP MODIFY GET
%token TYPE_INT TYPE_BOOL TYPE_ARR LOLLI AMP
%token EOF

%start file
%type <Syntax.toplevel_cmd list> file

%start toplevel
%type <Syntax.toplevel_cmd> toplevel
%type <Syntax.toplevel_cmd> command

%type <Syntax.ltype> ltype

%left AMP
%nonassoc EQUAL LESS
%left PLUS MINUS
%left TIMES DIVIDE MOD

%%

file:
  | EOF                              { [] }
  | NEWLINE lst = file               { lst }
  | cmd = command EOF                { [cmd] }
  | cmd = command NEWLINE lst = file { cmd :: lst }

toplevel:
  | cmd = command NEWLINE   { cmd }
  | NEWLINE                 { Skip }   (* empty line in the REPL *)

command:
  | LET x = VAR EQUAL e = expr 
    { Def (x, e) }
  | LET x = VAR COLON t = ltype EQUAL e = expr 
    { Def (x, Zoo.locate ~loc:e.Zoo.loc (Annot (e, t))) }
  | LET REC x = VAR FROM t1 = ltype TO t2 = ltype EQUAL e = expr 
    { DefRec (x, t1, t2, e) }
  | e = expr 
    { Expr e }


expr: mark_position(plain_expr) { $1 }
plain_expr:
  | FUN x = VAR COLON t = ltype ARROW e = expr
    { Fun (x, t, e) }
  | IF cond = expr THEN e1 = expr ELSE e2 = expr
    { If (cond, e1, e2) }
  | MATCH scrut = expr WITH INL x = VAR ARROW left = infix_expr ALTERNATIVE INR y = VAR ARROW right = expr
    { Match (scrut, x, left, y, right) }
  | MATCH pair = expr WITH LPAREN x = VAR COMMA y = VAR RPAREN ARROW body = expr
    { Split (pair, x, y, body) }
  | e = plain_infix_expr
    { e }

infix_expr: mark_position(plain_infix_expr) { $1 }
plain_infix_expr:
  | e1 = infix_expr AMP e2 = infix_expr     { Bundle (e1, e2) }
  | e1 = infix_expr EQUAL e2 = infix_expr   { Equal (e1, e2) }
  | e1 = infix_expr LESS e2 = infix_expr    { Less (e1, e2) }
  | e1 = infix_expr PLUS e2 = infix_expr    { Plus (e1, e2) }
  | e1 = infix_expr MINUS e2 = infix_expr   { Minus (e1, e2) }
  | e1 = infix_expr TIMES e2 = infix_expr   { Times (e1, e2) }
  | e1 = infix_expr DIVIDE e2 = infix_expr  { Divide (e1, e2) }
  | e1 = infix_expr MOD e2 = infix_expr     { Mod (e1, e2) }
  | e = plain_app_expr { e }

app_expr: mark_position(plain_app_expr) { $1 }
plain_app_expr:
  | e = plain_atom_expr                                 { e }
  | f = app_expr a = atom_expr                    { Apply (f, a) }
  | MAKE                            { Make  }
  | LENGTH                          { Length  }
  | LOOKUP                          { Lookup  }
  | GET                             { Get  }
  | MODIFY                          { Modify  }
  | INL e = atom_expr { Inl e }
  | INR e = atom_expr { Inr e }
  | FST e = atom_expr { Fst e }
  | SND e = atom_expr { Snd e }

atom_expr: mark_position(plain_atom_expr) { $1 }
plain_atom_expr:
  | LPAREN e = plain_expr RPAREN { e }
  | LPAREN e1 = expr COMMA e2 = expr RPAREN { Pair (e1, e2) }
  | LPAREN e = expr COLON t = ltype RPAREN  { Annot (e, t) }
  | x = VAR     { Var x }
  | n = INT     { Int n }
  | TRUE        { Bool true }
  | FALSE       { Bool false }
  | LBRACKET es = separated_list(COMMA, expr) RBRACKET { Array es }
  | CARET e = atom_expr { Demote e }

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
  | TYPE_BOOL
    { LBool }
  | BANG t = atom_type
    { LBang t }
  | TYPE_ARR t = atom_type
    { LArr t }
  | LPAREN t = ltype RPAREN
    { t }


mark_position(X):
  x = X
  { Zoo.locate ~loc:(Zoo.make_location $startpos $endpos) x }
