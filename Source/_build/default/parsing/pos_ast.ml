open Format
open Type
open Binop

(* a position in the source file.
 * the column in the current line is given by
 * col - bol (first column of a line is column 0) *)
type position = {
  line : int ;
  (* the column of the beggining of the line *)
  bol  : int ;
  col : int
}

(* a range of positions in the source file *)
type range = {
  first : position ;
  last : position
}

(* expressions with position ranges *)
type  pos_expr = 
  { pos : range ; expr : expr }

and expr =
  | PEbool of bool
  | PEint of int64
  | PEstring of string

  | PEbinop of binop * pos_expr * pos_expr
  | PEnot of pos_expr
  
  | PEblock of pos_expr list

  | PEif of pos_expr * pos_expr * pos_expr
  | PEwhile of pos_expr * pos_expr
  | PEfor of string * pos_expr * pos_expr * pos_expr
  
  | PEassign of pos_expr * pos_expr
  | PEident of string
  | PEdot of pos_expr * string

  | PEcall of string * pos_expr list
  | PEreturn of pos_expr option
  
and func = {
  pos : range ;
  fname : string ;
  params : (string * ty) list ;
  ret_type : ty ;
  code : pos_expr
}

and struc = {
  pos : range ;
  sname : string ;
  mutab : bool ;
  fields : (string * ty) list 
}

and decl = 
  | PDexpr of pos_expr
  | PDfunc of func
  | PDstruct of struc

and program = decl list

(*** PRINTING ***)

(* node indent *)
let ni is_last =
  if is_last then "└─" else "├─"

(* child indent *)
let ci is_last =
  if is_last then "   " else "|  "

let rec pp_expr fmt prefix is_last e = match e.expr with
  | PEbool b -> 
    fprintf fmt "%s PEbool %b %a \n" (prefix ^ ni is_last) b pp_pos e.pos
  | PEint n -> 
    fprintf fmt "%s PEint %s %a\n" (prefix ^ ni is_last) (Int64.to_string n) pp_pos e.pos
  | PEstring s -> 
    fprintf fmt "%s PEstring %s %a\n" (prefix ^ ni is_last) s pp_pos e.pos
  | PEbinop (op, e1, e2) ->
    fprintf fmt "%s PEbinop %s %a\n" (prefix ^ ni is_last) (binop_to_string op) pp_pos e.pos;
    pp_children_expr fmt prefix is_last [e1; e2]
  | PEnot e1 ->
    fprintf fmt "%s PEnot %a\n" (prefix ^ ni is_last) pp_pos e.pos;
    pp_expr fmt (prefix ^ ci is_last) true e1
  | PEif (cond, b1, b2) ->
    fprintf fmt "%s PEif %a\n" (prefix ^ ni is_last) pp_pos e.pos;
    pp_children_expr fmt prefix is_last [cond; b1; b2] 
  | PEwhile (cond, body) ->
    fprintf fmt "%s PEwhile %a\n" (prefix ^ ni is_last) pp_pos e.pos;
    pp_children_expr fmt prefix is_last [cond; body]
  | PEfor (i, e1, e2, body) ->
    fprintf fmt "%s PEfor %s %a\n" (prefix ^ ni is_last) i pp_pos e.pos;
    pp_children_expr fmt prefix is_last [e1; e2; body]
  | PEblock b ->
    fprintf fmt "%s PEblock %a\n" (prefix ^ ni is_last) pp_pos e.pos;
    pp_children_expr fmt prefix is_last b
  | PEcall (f, args) ->
    fprintf fmt "%s PEcall %s %a\n" (prefix ^ ni is_last) f pp_pos e.pos;
    pp_children_expr fmt prefix is_last args
  | PEreturn None -> 
    fprintf fmt "%s PEreturn %a\n" (prefix ^ ni is_last) pp_pos e.pos
  | PEreturn (Some e1) ->
    fprintf fmt "%s PEreturn %a\n" (prefix ^ ni is_last) pp_pos e.pos;
    pp_expr fmt (prefix ^ ci is_last) true e1
  | PEident i -> 
    fprintf fmt "%s PEident %s %a\n" (prefix ^ ni is_last) i pp_pos e.pos
  | PEdot (e1, i) ->
    fprintf fmt "%s PEdot %a\n" (prefix ^ ni is_last) pp_pos e.pos;
    pp_expr fmt (prefix ^ ci is_last) false e1;
    fprintf fmt "%s %s\n" (prefix ^ ci is_last ^ ni true) i
  | PEassign (e1, e2) ->
    fprintf fmt "%s PEassign %a\n" (prefix ^ ni is_last) pp_pos e.pos;
    pp_children_expr fmt prefix is_last [e1; e2]

and pp_children_expr fmt prefix is_last = function
  | [] -> ()
  | [e] -> pp_expr fmt (prefix ^ ci is_last) true e
  | e :: el ->
    pp_expr fmt (prefix ^ ci is_last) false e; 
    pp_children_expr fmt prefix is_last el

and pp_func fmt prefix is_last f =
  fprintf fmt "%s Func %s :: %s %a\n" 
    (prefix ^ ni is_last) 
    f.fname 
    (type_to_string f.ret_type)
    pp_pos f.pos;
  List.iter (pp_param fmt (prefix ^ ci is_last) false) f.params;
  pp_expr fmt (prefix ^ ci is_last) true f.code

and pp_param fmt prefix is_last (x, t) =
  fprintf fmt "%s Param %s :: %s\n" (prefix ^ ni is_last) x (type_to_string t)
  

and pp_struct fmt prefix is_last s =
  fprintf fmt "%s Struct %s%s %a\n"
    (prefix ^ ni is_last) 
    s.sname 
    (if s.mutab then " (mutable)" else "")
    pp_pos s.pos;
  let rec loop = function
    | [] -> ()
    | [(i, t)] -> pp_field fmt (prefix ^ ci is_last) true (i, t)
    | (i, t) :: fields ->
        pp_field fmt (prefix ^ ci is_last) false (i, t);
        loop fields
  in
  loop s.fields

and pp_field fmt prefix is_last (i, t) =
  fprintf fmt "%s Field %s :: %s\n" (prefix ^ ni is_last) i (type_to_string t)
  
and pp_decl fmt prefix is_last = function
  | PDexpr e -> pp_expr fmt prefix is_last e
  | PDfunc f -> pp_func fmt prefix is_last f
  | PDstruct s -> pp_struct fmt prefix is_last s

and print_prog fmt prog = 
  fprintf fmt "POS AST\n";
  List.iter (pp_decl fmt "" true) prog 

and pp_pos fmt range =
  fprintf fmt "\t\t\t\t%a %a" pp_single_pos range.first pp_single_pos range.last

and pp_single_pos fmt pos =
  (* add one to have one-indexed columns *)
  fprintf fmt "%d:%d" pos.line (pos.col - pos.bol + 1)

