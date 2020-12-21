open Format
open Type
open Binop

type typed_expr =
  { ty : ty ; expr : expr }

and expr =
  | TEbool of bool
  | TEint of int64
  | TEstring of string

  | TEbinop of binop * typed_expr * typed_expr
  | TEnot of typed_expr

  | TEif of typed_expr * typed_expr * typed_expr
  (* for loops, we have to keep track of which
   * local variables are created in their inner scope 
   * to uninitialize them before every iteration *)
  | TEwhile of typed_expr * typed_expr * var list
  (* the loop variable is in the var list *)
  | TEfor of typed_expr * typed_expr * typed_expr * var list
  
  | TEblock of typed_expr list
  
  | TEassign_var of var * typed_expr
  (* the first field is the struct
   * the second is the field name *)
  | TEassign_field of typed_expr * string * typed_expr
  | TEaccess_var of var
  | TEaccess_field of typed_expr * string

  | TEcall of string * call_info list * typed_expr list
  | TEprint of typed_expr list
  | TEreturn of typed_expr option

(* data for an identifier *)
and var = 
  (* stack locals are always of type Tany *)
  | StackLocal of { name : string ; index : int }
  | FuncParam of { name : string ; index : int ; ty : ty }
  (* loop variables are always of type Tint64. 
   * they occupy 2 stack slots and are always new variables
   * i.e. they can't use the same slot as a previous variable *)
  | LoopVar of { name : string ; index : int }
  | Global of { name : string ; ty : ty }

(* the info for a function/struct compatible
 * with the arg types of a call *)
and call_info = 
  (* contains the function signature *)
  | FuncCall of ty list
  (* contains the struct field types *)
  | StructCreation of ty list

and decl =
  (* second field is the frame size for this expression *)
  | TDexpr of typed_expr * int
  | TDfunc of func
  | TDstruct of struc

and func = {
  fname : string ;
  param_types : ty list ;
  ret_type : ty ;
  code : typed_expr ;
  frame_size : int
}

and struc = {
  sname : string ;
  mutab : bool ;
  fields : (string * ty) list
}

let var_type = function
  | Global g -> g.ty
  | StackLocal _ -> Tany
  | FuncParam fp -> fp.ty
  | LoopVar _ -> Tint64

let var_name = function
  | Global g -> g.name
  | StackLocal sl -> sl.name
  | FuncParam fp -> fp.name
  | LoopVar lv -> lv.name

(*** PRINTING ***)

(* node indent *)
let ni is_last =
  if is_last then "└─" else "├─"

(* child indent *)
let ci is_last =
  if is_last then "   " else "|  "

let rec pp_expr fmt prefix is_last e = match e.expr with
  | TEbool b -> 
    fprintf fmt "%s %aTEbool %b\n" (prefix ^ ni is_last) pp_type e.ty b
  | TEint n -> 
    fprintf fmt "%s %aTEint %s\n" (prefix ^ ni is_last) pp_type e.ty (Int64.to_string n)
  | TEstring s -> 
    fprintf fmt "%s %aTEstring %s\n" (prefix ^ ni is_last) pp_type e.ty s
  | TEbinop (op, e1, e2) ->
    fprintf fmt "%s %aTEbinop %s\n" (prefix ^ ni is_last) pp_type e.ty (binop_to_string op);
    pp_multiple_expr fmt (prefix ^ ci is_last) true [e1; e2]
  | TEnot e1 ->
    fprintf fmt "%s %aTEnot\n" (prefix ^ ni is_last) pp_type e.ty;
    pp_expr fmt (prefix ^ ci is_last) true e1  
  | TEif (cond, b1, b2) ->
    fprintf fmt "%s %aTEif\n" (prefix ^ ni is_last) pp_type e.ty;
    pp_multiple_expr fmt (prefix ^ ci is_last) true [cond; b1; b2] 
  | TEwhile (cond, body, vars) ->
    fprintf fmt "%s %aTEwhile\n" (prefix ^ ni is_last) pp_type e.ty;
    fprintf fmt "%s Vars\n" (prefix ^ ci is_last ^ ni false);
    pp_multiple_vars fmt (prefix ^ ci is_last ^ ci false) true vars;
    pp_multiple_expr fmt (prefix ^ ci is_last) true [cond; body]
  | TEfor (e1, e2, body, vars) ->
    fprintf fmt "%s %aTEfor\n" (prefix ^ ni is_last) pp_type e.ty;
    fprintf fmt "%s Vars\n" (prefix ^ ci is_last ^ ni false);
    pp_multiple_vars fmt (prefix ^ ci is_last ^ ci false) true vars;
    pp_multiple_expr fmt (prefix ^ ci is_last) true [e1; e2; body]
  | TEblock b ->
    fprintf fmt "%s %aTEblock\n" (prefix ^ ni is_last) pp_type e.ty;
    pp_multiple_expr fmt (prefix ^ ci is_last) true b
  | TEaccess_var v ->
    fprintf fmt "%s %aTEaccess_var\n" (prefix ^ ni is_last) pp_type e.ty;
    pp_var fmt (prefix ^ ci is_last) true v
  | TEassign_var (v, e1) ->
    fprintf fmt "%s %aTEassign_var\n" (prefix ^ ni is_last) pp_type e.ty;
    pp_var fmt (prefix ^ ci is_last) false v;
    pp_expr fmt (prefix ^ ci is_last) true e1;
  | TEaccess_field (e1, f_name) ->
    fprintf fmt "%s %aTEaccess_field\n" (prefix ^ ni is_last) pp_type e.ty;
    fprintf fmt "%s Field %s\n" (prefix ^ ci is_last ^ ni false) f_name;
    pp_expr fmt (prefix ^ ci is_last) true e1;
  | TEassign_field (e1, f_name, e2) ->
    fprintf fmt "%s %aTEassign_field\n" (prefix ^ ni is_last) pp_type e.ty;
    fprintf fmt "%s Field %s\n" (prefix ^ ci is_last ^ ni false) f_name;
    pp_multiple_expr fmt (prefix ^ ci is_last) true [e1; e2]
  | TEcall (fname, call_infos, args) ->
    fprintf fmt "%s %aTEcall %s\n" (prefix ^ ni is_last) pp_type e.ty fname;
    List.iter (pp_call_info fmt (prefix ^ ci is_last) false) call_infos;
    pp_multiple_expr fmt (prefix ^ ci is_last) true args
  | TEprint (e_list) ->
    fprintf fmt "%s %aTEprint\n" (prefix ^ ni is_last) pp_type e.ty;
    pp_multiple_expr fmt (prefix ^ ci is_last) true e_list
  | TEreturn None ->
    fprintf fmt "%s %aTEreturn\n" (prefix ^ ni is_last) pp_type e.ty
  | TEreturn (Some e1) ->
    fprintf fmt "%s %aTEreturn\n" (prefix ^ ni is_last) pp_type e.ty;
    pp_expr fmt (prefix ^ ci is_last) true e1

and pp_var fmt prefix is_last = function
  | FuncParam fp ->
    fprintf fmt "%s FuncParam name=%s index=%d type=%s\n"
      (prefix ^ ni is_last) fp.name fp.index (type_to_string fp.ty)
  | StackLocal sl ->
    fprintf fmt "%s StackLocal name=%s index=%d\n"
      (prefix ^ ni is_last) sl.name sl.index
  | LoopVar lv ->
    fprintf fmt "%s LoopVar name=%s index=%d\n"
      (prefix ^ ni is_last) lv.name lv.index
  | Global g ->
    fprintf fmt "%s Global name=%s type=%s\n"
      (prefix ^ ni is_last) g.name (type_to_string g.ty)

and pp_call_info fmt prefix is_last = function
  | FuncCall param_types ->
    fprintf fmt "%s FuncCall (%s)\n" 
      (prefix ^ ni is_last) 
      (String_utils.join ", " (List.map type_to_string param_types))
  | StructCreation field_types ->
    fprintf fmt "%s StructCreation (%s)\n" 
      (prefix ^ ni is_last) 
      (String_utils.join ", " (List.map type_to_string field_types))

and pp_multiple_expr fmt prefix is_last e_list =
  let rec loop = function
    | [] -> ()
    | [e] -> pp_expr fmt prefix is_last e
    | e :: el ->
      pp_expr fmt prefix false e; 
      loop el
  in
  loop e_list

and pp_multiple_vars fmt prefix is_last var_list =
  let rec loop = function
    | [] -> ()
    | [v] -> pp_var fmt prefix is_last v
    | v :: vars ->
      pp_var fmt prefix false v;
      loop vars
  in
  loop var_list

and pp_func fmt prefix is_last f =
  fprintf fmt "%s Func name=%s frame_size=%d\n" 
    (prefix ^ ni is_last) 
    f.fname 
    f.frame_size;
  List.iter (pp_param_type fmt (prefix ^ ci is_last) false) f.param_types;
  fprintf fmt "%s ReturnType %s\n" (prefix ^ ci is_last ^ ni false) (type_to_string f.ret_type);
  pp_expr fmt (prefix ^ ci is_last) true f.code

and pp_param_type fmt prefix is_last ty =
  fprintf fmt "%s ParamType %s\n" (prefix ^ ni is_last) (type_to_string ty)
  
and pp_struct fmt prefix is_last s =
  fprintf fmt "%s Struct %s %s\n" 
    (prefix ^ ni is_last)
    s.sname
    (if s.mutab then " (mutable)" else "");
  let rec loop = function
    | [] -> ()
    | [f] -> pp_field fmt (prefix ^ ci is_last) true f
    | f :: fields ->
      pp_field fmt (prefix ^ ci is_last) false f;
      loop fields
  in
  loop s.fields

and pp_field fmt prefix is_last (x, ty) =
  fprintf fmt "%s Field %s :: %s\n"
    (prefix ^ ni is_last)
    x
    (type_to_string ty)

and pp_decl fmt prefix is_last = function
  | TDexpr (e, fs) -> 
    fprintf fmt "%s TDExpr frame_size=%d\n" (prefix ^ ni is_last) fs;
    pp_expr fmt (prefix ^ ci is_last) true e;
  | TDfunc f -> pp_func fmt prefix is_last f
  | TDstruct s -> pp_struct fmt prefix is_last s

and print_prog fmt prog =
  fprintf fmt "TYPE AST\n";
  List.iter (pp_decl fmt "" true) prog

and pp_type fmt ty =
  fprintf fmt "[%s] " (type_to_string ty)


