
type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Ne

type unop =
  | Neg

type expr =
  | Int of int
  | Float of float
  | Ident of string
  | Call of expr * expr list
  | Binop of binop * expr * expr
  | Unop of unop * expr

type stat =
  | Expr of expr
  | Assign of string * expr
  | Seq of stat * stat
  | While of expr * stat

type func =
  { name: string
  ; args: string list
  ; body: stat
  }

type program = func list
