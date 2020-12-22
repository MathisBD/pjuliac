open X86_64
open Code_basics
open Type
open Type_ast


(* offsets of the different fields in a value, in bytes *)
let ofs_type = 0
let ofs_data = 8

(* the different value types *)
let t_nothing = 0
let t_bool = 1
let t_int64 = 2
let t_string = 3
(* structs start at 4 and go upwards *)
let t_struct_start = 4

(* wrapper around the type program of X86_64 *)
type t = {
  mutable sl_num : int ;
  mutable cl_num : int ;

  globals : (string, unit) Hashtbl.t ;
  mutable enclosing_func : Type_ast.func option ;

  struct_type_number : (string, int) Hashtbl.t ;
  (* input : struct type number
   * output : struct size in bytes *)
  struct_size : (int, int) Hashtbl.t ;
  struct_by_name : (string, struc) Hashtbl.t ;
  struct_with_field : (string, struc) Hashtbl.t ;
  mutable struct_count : int ;

  mutable main_code : text ;
  mutable func_code : text ;
  mutable data : data
}

let create () =
{
  sl_num = 0 ; cl_num = 0 ;
  
  globals = Hashtbl.create 4 ;
  enclosing_func = None ;

  struct_type_number = Hashtbl.create 4 ;
  struct_size = Hashtbl.create 4 ;
  struct_with_field = Hashtbl.create 4 ;
  struct_by_name = Hashtbl.create 4 ;
  struct_count = 0;

  main_code = nop ; func_code = nop ; data = nop
}

let get_main_code prg = prg.main_code 

let get_func_code prg = prg.func_code

let get_data prg = prg.data

let add_code prg c =
  prg.main_code <- prg.main_code ++ c

let add_func_code prg c =
  prg.func_code <- prg.func_code ++ c ++ inline "\n"

let add_data prg d =
  prg.data <- prg.data ++ d

let string_label prg str = 
  let sl = Printf.sprintf ".S%d" prg.sl_num in
  (* TODO : reuse an existing string *)
  add_data prg (label sl ++ string str);
  prg.sl_num <- 1 + prg.sl_num;
  sl

let code_label prg =
  let cl = Printf.sprintf ".L%d" prg.cl_num in
  prg.cl_num <- 1 + prg.cl_num;
  cl

let size_of prg vtype =
  if vtype = t_nothing
  then 8
  else if vtype = t_bool || vtype = t_int64 || vtype = t_string
  then 16
  else begin
    assert (t_struct_start <= vtype && vtype < t_struct_start + prg.struct_count);
    Hashtbl.find prg.struct_size vtype
  end

let type_number prg = function
  | Tany -> assert false
  | Tnothing -> t_nothing
  | Tbool -> t_bool
  | Tint64 -> t_int64
  | Tstring -> t_string
  | Tstruct sname -> 
    begin 
      try Hashtbl.find prg.struct_type_number sname
      with Not_found -> failwith (Printf.sprintf "struct %s is not registered" sname)
    end

(* returns a pointer to the allocated block in %rax *)
let allocate prg vtype =
  let size = size_of prg vtype in
  movq (imm size) !%rdi ++
  call "malloc" ++
  movq (imm vtype) (ind ~ofs:ofs_type rax)

let register_global prg name =
  if not (Hashtbl.mem prg.globals name) then
  begin
    Hashtbl.add prg.globals name ();
    add_data prg (label name ++ dquad [uninit_value])
  end

let error prg msg =
  let msglabel = string_label prg msg in
  movq (ilab msglabel) !%rdi ++
  call "error"

let set_enclosing_func prg f =
  prg.enclosing_func <- f

(* check the type of the expression in %rax
 * matches the expected return type of the current function.
 * doesn't modify the contents of %rax *)
let check_return_type prg =
  begin match prg.enclosing_func with
    | None -> assert false
    | Some f when f.ret_type = Tany -> nop
    | Some f ->
      let lexit = code_label prg in
      movq (ind ~ofs:ofs_type rax) !%rcx ++
      cmpq (imm (type_number prg f.ret_type)) !%rcx ++
      je lexit ++
      error prg (Printf.sprintf 
        "invalid type of value returned in function %s (expected %s)" 
        f.fname
        (type_to_string f.ret_type)) ++
      label lexit
  end

let register_struct prg s =
  let size = 8 * (1 + (List.length s.fields)) in
  let t_num = t_struct_start + prg.struct_count in
  Hashtbl.add prg.struct_type_number s.sname t_num;
  Hashtbl.add prg.struct_size t_num size;
  Hashtbl.add prg.struct_by_name s.sname s;
  prg.struct_count <- 1 + prg.struct_count;
  List.iter
    (fun (fname, _) -> Hashtbl.add prg.struct_with_field fname s)
    s.fields

let get_struct_type_number prg sname =
  Hashtbl.find prg.struct_type_number sname

let get_struct_with_field prg fname =
  Hashtbl.find prg.struct_with_field fname


