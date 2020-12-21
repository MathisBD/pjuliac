open Type_ast
open X86_64
open Binop
open Program
open Code_basics
open Asm_functions

(* global program *)
let prg = Program.create ()


let rec compile_binop op te1 te2 =
  (* compile And and Or as 'if' expressions *)
  if op = And then 
    let te_false = { ty = Tbool ; expr = TEbool false } in
    compile_expr { ty = Tbool ; expr = TEif (te1, te2, te_false) } 
  else if op = Or then
    let te_true = { ty = Tbool ; expr = TEbool true } in
    compile_expr { ty = Tbool ; expr = TEif (te1, te_true, te2) }
  else   
    (* te1 in %r8/%rcx
     * te2 in %r9/%rdx *)
    compile_expr te1 ++
    compile_expr te2 ++
    popq r9 ++ movq (ind ~ofs:ofs_type r9) !%rdx ++     
    popq r8 ++ movq (ind ~ofs:ofs_type r8) !%rcx ++ 
    begin match op with
      | Eq | Neq -> failwith "not implemented"
      | Add | Sub | Mul | Mod | Pow ->
        let l1 = code_label prg in
        let lerror = error_label prg in
        let error_msg = Printf.sprintf
          "invalid argument types for operator %s"
          (binop_to_string op)
        in
        cmpq (imm t_int64) !%rcx ++ jne lerror ++
        cmpq (imm t_int64) !%rdx ++ jne lerror ++
        jmp l1 ++
        label lerror ++ error prg error_msg ++
        (* result in %r8 *)
        label l1 ++
        movq (ind ~ofs:ofs_data r9) !%r9 ++
        movq (ind ~ofs:ofs_data r8) !%r8 ++  
        begin match op with
          | Add -> addq !%r9 !%r8
          | Sub -> subq !%r9 !%r8
          | Mul -> imulq !%r9 !%r8
          | Mod -> 
            let l2 = code_label prg in 
              testq !%r9 !%r9 ++ jnz l2 ++
              error prg "modulo by zero" ++
            label l2 ++
              movq (imm 0) !%rdx ++
              movq !%r8 !%rax ++
              idivq !%r9 ++ 
              movq !%rdx !%r8
          | Pow -> 
            let l2 = code_label prg in
              cmpq (imm 0) !%r9 ++ jge l2 ++
              error prg "negative exponent" ++
            label l2 ++
              movq !%r8 !%rdi ++
              movq !%r9 !%rsi ++
              call "pow" ++
              movq !%rax !%r8 
          | _ -> assert false
        end ++
        (* box the result *)
        pushq !%r8 ++
        allocate t_int64 ++
        popq r8 ++
        movq !%r8 (ind ~ofs:ofs_data rax) ++
        pushq !%rax
      | Leq | Lt | Geq | Gt ->
        let l1 = code_label prg in
        let l2 = code_label prg in
        let error_msg = Printf.sprintf 
          "invalid argument types for operator %s"
          (binop_to_string op)
        in
        (* check argument types *)
        cmpq (imm t_bool) !%rcx ++ je l1 ++
        cmpq (imm t_int64) !%rcx ++ je l1 ++
        error prg error_msg ++
        label l1 ++
        cmpq (imm t_bool) !%rdx ++ je l2 ++
        cmpq (imm t_int64) !%rdx ++ je l2 ++
        error prg error_msg ++       
        (* order of arguments matters here *)
        label l2 ++
        movq (ind ~ofs:ofs_data r9) !%r9 ++
        movq (ind ~ofs:ofs_data r8) !%r8 ++
        cmpq !%r9 !%r8 ++
        (* result in %r8 *)
        begin match op with
          | Leq -> setle !%r8b
          | Lt -> setl !%r8b
          | Geq -> setge !%r8b
          | Gt -> setg !%r8b
          | _ -> assert false
        end ++
        movzbq !%r8b r8 ++
        (* box the result *)
        pushq !%r8 ++
        allocate t_bool ++
        popq r8 ++
        movq !%r8 (ind ~ofs:ofs_data rax) ++
        pushq !%rax 
      | _ -> assert false
    end

    
(* returns the code to push te on the stack.
 * can add to the data section of the program *)
and compile_expr te = match te.expr with
  | TEbool b -> 
    let value = if b then 1 else 0 in
    allocate t_bool ++
    movq (imm value) (ind ~ofs:ofs_data rax) ++
    pushq !%rax
  | TEint i ->
    allocate t_int64 ++
    movq (imm64 i) (ind ~ofs:ofs_data rax) ++
    pushq !%rax
  | TEstring s ->
    let slabel = string_label prg s in
    allocate t_string ++
    movq (ilab slabel) (ind ~ofs:ofs_data rax) ++
    pushq !%rax
  | TEnot te1 ->
    let l1 = code_label prg in
    compile_expr te1 ++
    popq r8 ++
    movq (ind ~ofs:ofs_type r8) !%rcx ++
    cmpq (imm t_bool) !%rcx ++ je l1 ++
    error prg "invalid argument type for operator !" ++
    (* result goes in %r9 *)
    label l1 ++
    movq (imm 1) !%r9 ++
    xorq (ind ~ofs:ofs_data r8) !%r9 ++
    (* box the result *)
    pushq !%r9 ++
    allocate t_bool ++
    popq r9 ++
    movq !%r9 (ind ~ofs:ofs_data rax) ++
    pushq !%rax
  | TEbinop (op, te1, te2) -> compile_binop op te1 te2
  | TEprint te_list ->
    let rec push_args = function
      | [] -> nop
      | te :: te_list ->
        compile_expr te ++ push_args te_list
    in
    push_args (List.rev te_list) ++
    repeat 
      (List.length te_list) 
      (popq rdi ++ call "print") ++
    allocate t_nothing ++
    pushq !%rax
  | TEblock te_list ->
    let rec push_pop_args = function
      | [] -> allocate t_nothing ++ pushq !%rax
      | [te1] -> 
        (* don't pop the last expression *)
        compile_expr te1
      | te1 :: te_list ->
        compile_expr te1 ++
        popn 1 ++
        push_pop_args te_list
    in
    push_pop_args te_list
  | TEif (cond, te1, te2) ->
    let l1 = code_label prg in
    let lfalse = code_label prg in
    let lend = code_label prg in
    compile_expr cond ++
    popq r8 ++ movq (ind ~ofs:ofs_type r8) !%rcx ++
    cmpq (imm t_bool) !%rcx ++ je l1 ++
    error prg "invalid condition type in 'if' expression" ++
    label l1 ++
      movq (ind ~ofs:ofs_data r8) !%r8 ++
      testq !%r8 !%r8 ++
      jz lfalse ++
      compile_expr te1 ++
      jmp lend ++
    label lfalse ++
      compile_expr te2 ++
    label lend
  | TEwhile (cond, body, vars) ->
    let lbody = code_label prg in
    let lcond = code_label prg in
    let l1 = code_label prg in
    jmp lcond ++
    label lbody ++
      compile_expr body ++
      popn 1 ++
      (* this way the variables are uninitialized after the loop finished.
       * they were already uninitialized before the loop started *)
      uninitialize_vars vars ++
    label lcond ++
      compile_expr cond ++
      popq r8 ++
      movq (ind ~ofs:ofs_type r8) !%rcx ++
      cmpq (imm t_bool) !%rcx ++ je l1 ++
      error prg "invalid condition type in 'while' expression" ++
    label l1 ++  
      movq (ind ~ofs:ofs_data r8) !%r8 ++
      testq !%r8 !%r8 ++
      jnz lbody ++
      allocate t_nothing ++
      pushq !%rax

  | TEfor (te1, te2, te3, vars) ->
    let l1 = code_label prg in
    let l2 = code_label prg in
    let lbody = code_label prg in
    let lcond = code_label prg in
    let lv = 
      List.find (function LoopVar _ -> true | _ -> false) vars 
    in
    compile_expr te1 ++
    compile_expr te2 ++
    popq r9 ++ popq r8 ++
    movq (ind ~ofs:ofs_type r8) !%rcx ++
    movq (ind ~ofs:ofs_type r9) !%rdx ++
      cmpq (imm t_int64) !%rcx ++ je l1 ++
      error prg "invalid 'for' loop bound type" ++
    label l1 ++
      cmpq (imm t_int64) !%rdx ++ je l2 ++
      error prg "invalid 'for' loop bound type" ++
    label l2 ++
      movq (ind ~ofs:ofs_data r8) !%r8 ++
      movq (ind ~ofs:ofs_data r9) !%r9 ++
      (* the loop bounds are on the stack
       * (upper bound on top) *)
      pushq !%r8 ++ pushq !%r9 ++ 
      (* initialize the "visible" loop variable *)
      allocate t_int64 ++
      movq !%rax (ind ~ofs:(rbp_offset lv) rbp) ++
      jmp lcond ++
    label lbody ++
      (* update the "visible" loop variable *)
      movq (ind ~ofs:8 rsp) !%r8 ++
      movq (ind ~ofs:(rbp_offset lv) rbp) !%rcx ++
      movq !%r8 (ind ~ofs:ofs_data rcx) ++
      movq !%rcx (ind ~ofs:(rbp_offset lv) rbp) ++
      (* loop body *)
      compile_expr te3 ++
      popn 1 ++
      uninitialize_vars vars ++
      (* increment the lower bound *)
      incq (ind ~ofs:8 rsp) ++
    label lcond ++ 
      movq (ind ~ofs:8 rsp) !%r8 ++
      movq (ind rsp) !%r9 ++
      cmpq !%r8 !%r9 ++
      jge lbody ++
      allocate t_nothing ++
      pushq !%rax

  | TEaccess_var v ->
    begin match v with
      | StackLocal _ 
      | LoopVar _ ->
        let l1 = code_label prg in
        movq (ind ~ofs:(rbp_offset v) rbp) !%r8 ++
        cmpq (imm uninit_value) !%r8 ++ jne l1 ++
        error prg (Printf.sprintf "unitinialized variable %s" (var_name v)) ++
        label l1 ++
        pushq !%r8
      | Global g ->
        let l1 = code_label prg in
        movq (lab g.name) !%r8 ++
        cmpq (imm uninit_value) !%r8 ++ jne l1 ++
        error prg (Printf.sprintf "uninitialized variable %s" (var_name v)) ++
        label l1 ++
        pushq !%r8
      | _ -> failwith "not implemented"
    end
  | TEassign_var (v, te1) ->
    compile_expr te1 ++
    (* don't pop the expression *)
    movq (ind rsp) !%r8 ++
    begin match v with
      | StackLocal _
      | LoopVar _ ->
        movq !%r8 (ind ~ofs:(rbp_offset v) rbp)
      | Global g -> 
        register_global prg g.name;
        movq !%r8 (lab g.name)
      | _ -> failwith "not implemented"
    end
  | _ -> failwith "not implemented"

let compile_decl = function
  | TDexpr (te, frame_size) -> 
    let code = 
      repeat frame_size (pushq (imm uninit_value)) ++
      compile_expr te ++
      popn (1 + frame_size) 
    in
    add_code prg code
  | _ -> failwith "not implemented"

let compile_prog decls = 
  List.iter compile_decl decls;
  let text = 
    globl "main" ++
    label "main" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    get_code prg ++
    leave ++
    movq (imm 0) !%rax ++
    ret ++ inline "\n" ++
    label "pow" ++ asm_pow prg ++ inline "\n" ++
    label "error" ++ asm_error prg ++ inline "\n" ++
    label "print" ++ asm_print prg ++ inline "\n" in
  let data =
    (* do this after having built the whole text *) 
    get_data prg
  in 
  { text = text ; data = data }




