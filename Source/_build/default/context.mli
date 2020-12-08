
type t

val create : unit -> t
val open_scope : t -> unit
val close_scope : t -> unit

val has_local_scope : t -> bool
val outer_scope : t -> Type_ast.var list

(* ty is the return type of the function *)
val enter_func : t -> Type.ty -> unit
val leave_func : t -> unit
val is_in_func : t -> bool
val func_ret_type : t -> Type.ty
val add_func : t -> Pos_ast.func -> unit
val add_func_param : t -> string -> Type.ty -> int -> unit
val func_signatures : t -> string -> (Type.ty list * Type.ty) list

val add_struct : t -> Pos_ast.struc -> unit
val contains_struct : t -> string -> bool
val retrieve_struct : t -> string -> Pos_ast.struc
val struct_with_field : t -> string -> Pos_ast.struc option

(* re-uses an existing stack_local, func_param or loop_var *)
val add_local : t -> string -> unit
val add_loop_var : t -> string -> unit
val add_global : t -> string -> Type.ty -> unit
val retrieve_entry : t -> string -> Type_ast.var
