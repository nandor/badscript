%{ open Ast %}

%token EOF
%token LPAREN, RPAREN, LBRACE, RBRACE, COMMA, SEMI
%token FUNC, WHILE
%token ADD SUB MUL DIV
%token ASSIGN
%token NE
%token <string> IDENT
%token <int>    INT
%token <float>  FLOAT

%left ADD SUB
%left MUL DIV
%nonassoc NE
%nonassoc NEG

%start <Ast.program> bs_program

%%

bs_program:
  | func = bs_func funcs = bs_program { func :: funcs }
  | func = bs_func EOF { [func] }

bs_func:
  | FUNC name = IDENT LPAREN args = bs_names RPAREN LBRACE body = bs_stats RBRACE
    { { name; args; body } }

bs_names:
  | arg = IDENT COMMA args = bs_names { arg :: args }
  | arg = IDENT { [arg] }
  | { [] }

bs_stats:
  | stat = bs_stat stats = bs_stats { Seq(stat, stats) }
  | stat = bs_stat { stat }

bs_stat:
  | stat = bs_expr SEMI { Expr(stat) }
  | name = IDENT ASSIGN e = bs_expr SEMI { Assign(name, e) }
  | WHILE LPAREN c = bs_expr RPAREN LBRACE b = bs_stats RBRACE { While(c, b) }

bs_expr:
  | e1 = bs_expr ADD e2 = bs_expr { Binop(Add, e1, e2) }
  | e1 = bs_expr SUB e2 = bs_expr { Binop(Sub, e1, e2) }
  | e1 = bs_expr MUL e2 = bs_expr { Binop(Mul, e1, e2) }
  | e1 = bs_expr DIV e2 = bs_expr { Binop(Div, e1, e2) }
  | e1 = bs_expr NE  e2 = bs_expr { Binop(Ne, e1, e2) }
  | SUB e = bs_expr { Unop(Neg, e) } %prec NEG
  | e = bs_atom { e }

bs_atom:
  | e = bs_atom LPAREN args = bs_args RPAREN { Call(e, args) }
  | v = INT { Int v }
  | v = IDENT { Ident v }
  | v = FLOAT { Float v }

bs_args:
  | arg = bs_expr COMMA args = bs_args { arg :: args }
  | arg = bs_expr { [arg] }
  | { [] }
