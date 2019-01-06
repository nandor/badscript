{
  open Parser
  open Lexing

  exception Error of string
}

let id = ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9''_']*
let num = ['1'-'9']['0'-'9']*

rule token = parse
  | [' ' '\t']+             { token lexbuf }
  | '\n'                    { Lexing.new_line lexbuf; token lexbuf }
  | eof                     { EOF }
  | '('                     { LPAREN }
  | ')'                     { RPAREN }
  | '{'                     { LBRACE }
  | '}'                     { RBRACE }
  | ','                     { COMMA }
  | ';'                     { SEMI }
  | '+'                     { ADD }
  | '-'                     { SUB }
  | '*'                     { MUL }
  | '/'                     { DIV }
  | '='                     { ASSIGN }
  | "!="                    { NE }
  | "func"                  { FUNC }
  | "while"                 { WHILE }
  | id as name              { IDENT name }
  | '0'                     { INT 0 }
  | num '.' ['0'-'9']+      { FLOAT (float_of_string (Lexing.lexeme lexbuf))}
  | num                     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | _ { raise (Error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
