
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
  | Tstruct s -> s 

let type_equal t1 t2 = match t1, t2 with
  | Tany, Tany
  | Tnothing, Tnothing
  | Tint64, Tint64 
  | Tbool, Tbool
  | Tstring, Tstring -> true 
  | Tstruct s1, Tstruct s2 when s1 = s2 -> true
  | _ -> false