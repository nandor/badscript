
type expr =
  | Int of int
  | Ident of string
  | Call of expr * expr list

type stat =
  | Expr of expr
  | Seq of stat * stat

type func =
  { name: string
  ; args: string list
  ; body: stat
  }

type program = func list
