open X86_64
open Code_basics


(* offsets of the different fields in a value, in bytes *)
let ofs_type = 0
let ofs_data = 8

(* the different value types *)
let t_nothing = 0
let t_bool = 1
let t_int64 = 2
let t_string = 3
(* structs start at 4 and go on increasing *)
let t_struct = 4

let size_of vtype =
  if vtype = t_nothing
  then 8
  else if vtype = t_bool || vtype = t_int64 || vtype = t_string
  then 16
  else failwith "not implemented"

(* wrapper around the type program of X86_64 *)
type t = {
  mutable sl_num : int ;
  mutable el_num : int ;
  mutable cl_num : int ;

  mutable globals : (string, unit) Hashtbl.t ;

  mutable main_code : text ;
  mutable func_code : text ;
  mutable data : data
}

let create () =
{
  sl_num = 0 ; el_num = 0 ; cl_num = 0 ;
  globals = Hashtbl.create 4 ;
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

let error_label prg =
  let el = Printf.sprintf ".Lerror%d" prg.el_num in
  prg.el_num <- 1 + prg.el_num;
  el

(* returns a pointer to the allocated block in %rax *)
let allocate vtype =
  let size = size_of vtype in
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