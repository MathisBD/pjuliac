
type ty =
  | Tany
  | Tnothing
  | Tint64
  | Tbool
  | Tstring
  | Tstruct of string

let type_to_string = function
  | Tany -> "Any"
  | Tnothing -> "Nothing"
  | Tint64 -> "Int64"
  | Tbool -> "Bool"
  | Tstring -> "String"
  | Tstruct s -> "struct " ^ s 
