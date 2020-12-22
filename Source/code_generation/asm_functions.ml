open X86_64
open Program
open Asm_functions_labels

(* runtime error : the error message is in %rdi *)
let asm_error prg =
  (* the newline is important to flush stdout 
  * (otherwise call fflush maybe) *)
  let msglabel = 
    string_label prg "runtime error: %s\n" 
  in
  label asm_error_lab ++ 
    (* print the error message *) 
    movq !%rdi !%rsi ++
    movq (ilab msglabel) !%rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++
    (* exit the program *)
    movq (imm error_exit_code) !%rdi ++
    (* exit is a libc function *)
    call "exit"

(* function to compute x**y where
 * x is in %rdi and y is in %rsi
 * (x and y are not addresses but are true numbers here) *)
let asm_pow prg =
  (* at the start of iteration i : 
   * - %rdi holds x**i
   * - %rax holds x**(first i bits of y)
   * - %rsi holds y>>i *)
  let l0 = code_label prg in
  let l1 = code_label prg in
  let l2 = code_label prg in
  label asm_pow_lab ++
    movq (imm 1) !%rax ++
    jmp l2 ++
  label l0 ++
    testq (imm 1) !%rsi ++
    jz l1 ++
    imulq !%rdi !%rax ++
  label l1 ++ 
    imulq !%rdi !%rdi ++
    shrq (imm 1) !%rsi ++
  label l2 ++
    testq !%rsi !%rsi ++
    jnz l0 ++
    ret 


(* function to print a value.
 * the value to print is in %rdi *)
let asm_print prg = 
  let lnothing = code_label prg in
  let lbool = code_label prg in
  let lint64 = code_label prg in
  let lstring = code_label prg in
  label asm_print_lab ++
    movq (ind ~ofs:ofs_type rdi) !%rcx ++ 
    cmpq (imm t_nothing) !%rcx ++ je lnothing ++
    cmpq (imm t_bool) !%rcx ++ je lbool ++
    cmpq (imm t_int64) !%rcx ++ je lint64 ++
    cmpq (imm t_string) !%rcx ++ je lstring ++
    error prg "can't print value" ++
  label lnothing ++
  begin
    let sl_nothing = string_label prg "nothing" in
    movq (ilab sl_nothing) !%rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++ ret
  end ++
  label lbool ++
  begin
    let sl_true = string_label prg "true" in
    let sl_false = string_label prg "false" in
    let l1 = code_label prg in
    movq (ind ~ofs:ofs_data rdi) !%rax ++
    
    movq (ilab sl_true) !%rdi ++
    testq !%rax !%rax ++
    jnz l1 ++
    movq (ilab sl_false) !%rdi ++

    label l1 ++
    movq (imm 0) !%rax ++
    call "printf" ++ ret
  end ++
  label lint64 ++
  begin
    let sl_format = string_label prg "%lld" in
    movq (ind ~ofs:ofs_data rdi) !%rsi ++
    movq (ilab sl_format) !%rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++ ret
  end ++
  label lstring ++
  begin
    movq (ind ~ofs:ofs_data rdi) !%rdi ++
    movq (imm 0) !%rax ++
    call "printf" ++ ret
  end

(* the two values to compare are in %rdi and %rsi *)
let asm_equal prg =
  let ltrue = code_label prg in
  let lfalse = code_label prg in
  let lbool = code_label prg in 
  let lint64 = code_label prg in 
  let lstring = code_label prg in
  let lstruct = code_label prg in
  label asm_equal_lab ++
    movq (ind ~ofs:ofs_type rdi) !%rcx ++
    movq (ind ~ofs:ofs_type rsi) !%rdx ++
    cmpq !%rcx !%rdx ++ jne lfalse ++
    cmpq (imm t_nothing) !%rcx ++ je ltrue ++
    cmpq (imm t_bool) !%rcx ++ je lbool ++
    cmpq (imm t_int64) !%rcx ++ je lint64 ++
    cmpq (imm t_string) !%rcx ++ je lstring ++
    jmp lstruct ++

  label lbool ++
  label lint64 ++
    movq (ind ~ofs:ofs_data rdi) !%rdi ++
    movq (ind ~ofs:ofs_data rsi) !%rsi ++
    cmpq !%rdi !%rsi ++
    je ltrue ++
    jmp lfalse ++

  label lstring ++ 
  begin
    let lbody = code_label prg in 
      movq (ind ~ofs:ofs_data rdi) !%rdi ++
      movq (ind ~ofs:ofs_data rsi) !%rsi ++
    label lbody ++
      (* the i-th character of the strings are in %r8 and %r9 *)
      movzbq (ind rdi) r8 ++
      movzbq (ind rsi) r9 ++
      (* the characters differ OR exactly one string ended *)
      cmpq !%r8 !%r9 ++ jne lfalse ++
      (* both strings ended *)
      testq !%r8 !%r8 ++ jz ltrue ++
      incq !%rdi ++
      incq !%rsi ++
      jmp lbody
  end ++

  label lstruct ++
    error prg "equality not implemented" ++

  label ltrue ++
    allocate prg t_bool ++
    movq (imm 1) (ind ~ofs:ofs_data rax) ++
    ret ++
  label lfalse ++
    allocate prg t_bool ++
    movq (imm 0) (ind ~ofs:ofs_data rax) ++
    ret

(* call this function to add all the asm functions
 * of this file to the program *)
let add_asm_functions prg =
  add_func_code prg (asm_error prg);
  add_func_code prg (asm_pow prg);
  add_func_code prg (asm_print prg);
  add_func_code prg (asm_equal prg)
