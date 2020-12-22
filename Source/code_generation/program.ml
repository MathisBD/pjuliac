open X86_64
open Type
open Type_ast
open Asm_functions_labels


let syscall_exit = 60
let error_exit_code = 1

(* must be an invalid memory address *)
let uninit_value = 1

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


let nothing_lab = "nothing"

(* tables (in the data segment) that hold 
 * information about each struct type *)
let struct_m_lab = "struct_m_table"
(* size of an entry in the table *)
let struct_m_stride = 1
let struct_fc_lab = "struct_fc_table"
(* size of an entry in the table *)
let struct_fc_stride = 8


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

let popn n =
  addq (imm (8*n)) !%rsp

let pushn n =
  subq (imm (8*n)) !%rsp

let rec repeat n code =
  if n = 0 then nop
  else code ++ repeat (n-1) code

let rbp_offset = function
  | StackLocal sl -> -8 * (sl.index +1)
  | FuncParam fp -> 8 * (fp.index + 2)
  | LoopVar lv -> -8 * (lv.index + 1)
  | _ -> assert false

(* offset of the i-th field in a struct *)
let field_offset i =
  8 * (i + 1)

let uninitialize_vars vars = 
  let uninit_single = function
    | StackLocal _ as v ->
      movq (imm uninit_value) (ind ~ofs:(rbp_offset v) rbp)
    | LoopVar _ -> nop (* don't uninitialize loop variables *)
    | _ -> assert false
  in
  let rec loop = function
    | [] -> nop
    | v :: vars -> uninit_single v ++ loop vars
  in
  loop vars

(* don't use the return type to mangle *)
let mangle_name fname param_types =
  let ty_names = List.map type_to_string param_types in
  fname ^ "_" ^ (String_utils.join "_" ty_names)

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
    assert (t_struct_start <= vtype);
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

(* returns a pointer to a fresh value 
 * of type number vtype in %rax *)
let allocate prg vtype =
  if vtype = t_nothing then
    (* all values of type Nothing are the same,
     * so we can reuse the same memory location *)
    movq (lab nothing_lab) !%rax
  else
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
  call asm_error_lab

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

let register_structs prg s_list =
  (* fill the hashtables *)
  let register i s =
    let size = 8 * (1 + (List.length s.fields)) in
    let t_num = t_struct_start + i in
    Hashtbl.add prg.struct_type_number s.sname t_num;
    Hashtbl.add prg.struct_size t_num size;
    Hashtbl.add prg.struct_by_name s.sname s;
    List.iter
      (fun (fname, _) -> Hashtbl.add prg.struct_with_field fname s)
      s.fields
  in 
  List.iteri register s_list;
  (* create tables to hold information about structs
   * accessible at run-time (e.g. : number of args) *)
  let rec zero = function
    | 0 -> []
    | n -> 0 :: (zero (n-1))
  in
  let m_list = 
    (* pad with zeros at the start to make accessing
     * straightforward with the struct number *)
    (zero t_struct_start) @
    (List.map (fun s -> (if s.mutab then 1 else 0)) s_list)
  in
  add_data prg 
    (label struct_m_lab ++ 
    dbyte m_list ++ 
    inline "\n");
  let fc_list =
    (zero t_struct_start) @
    (List.map (fun s -> List.length s.fields) s_list)
  in
  (* I'm conservative and use a quad for each field count :
   * it's likely a word or even byte would suffice for hand
   * written programs, but may not be enough for computer-generated programs *)
  add_data prg 
    (label struct_fc_lab ++ 
    dquad fc_list ++ 
    inline "\n")

let get_struct_type_number prg sname =
  Hashtbl.find prg.struct_type_number sname

let get_struct_with_field prg fname =
  Hashtbl.find prg.struct_with_field fname


