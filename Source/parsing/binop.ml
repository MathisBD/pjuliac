
type binop = Add | Sub | Mul | Mod | Div | Pow | Eq | Neq | Lt | Leq | Gt | Geq | And | Or

let binop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Mod -> "%"
  | Div -> "div"
  | Pow -> "^"
  | Eq  -> "=="
  | Neq -> "!="
  | Lt  -> "<"
  | Leq -> "<="
  | Gt ->  ">"
  | Geq -> ">="
  | And -> "&&"
  | Or ->  "||"



