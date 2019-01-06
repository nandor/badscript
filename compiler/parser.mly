%{ open Ast %}

%token EOF
%token LPAREN, RPAREN, LBRACE, RBRACE, COMMA, SEMI
%token FUNC
%token <float>  FLOAT
%token <string> IDENT
%token <int>    INT

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
  | stat = bs_stat SEMI stats = bs_stats { Seq(stat, stats) }
  | stat = bs_stat { stat }

bs_stat:
  | stat = bs_expr { Expr(stat) }

bs_expr:
  | e = bs_expr LPAREN args = bs_args RPAREN { Call(e, args) }
  | v = INT { Int v }
  | v = IDENT { Ident v }

bs_args:
  | arg = bs_expr COMMA args = bs_args { arg :: args }
  | arg = bs_expr { [arg] }
  | { [] }
