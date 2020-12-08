open X86_64

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

  mutable code : text ;
  mutable data : data
}

let create () =
{
  sl_num = 0 ; el_num = 0 ; cl_num = 0 ;
  code = nop ;
  data = nop
}

let get_code t = t.code

let get_data t = t.data

let add_code t c =
  t.code <- t.code ++ c

let add_data t d =
  t.data <- t.data ++ d

let string_label t str = 
  let sl = Printf.sprintf ".S%d" t.sl_num in
  (* TODO : reuse an existing string *)
  add_data t (label sl ++ string str);
  t.sl_num <- 1 + t.sl_num;
  sl

let code_label t =
  let cl = Printf.sprintf ".L%d" t.cl_num in
  t.cl_num <- 1 + t.cl_num;
  cl

let error_label t =
  let el = Printf.sprintf ".Lerror%d" t.el_num in
  t.el_num <- 1 + t.el_num;
  el

(* returns a pointer to the allocated block in %rax *)
let allocate vtype =
  let size = size_of vtype in
  movq (imm size) !%rdi ++
  call "malloc" ++
  movq (imm vtype) (ind ~ofs:ofs_type rax)

