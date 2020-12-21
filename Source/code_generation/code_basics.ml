open X86_64
open Type_ast

let syscall_exit = 60
let error_exit_code = 1

(* must be an invalid memory address *)
let uninit_value = 1

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
