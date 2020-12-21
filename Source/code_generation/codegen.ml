open Type_ast
open X86_64
open Binop
open Program
open Code_basics

(* When compiling an expression, the result is in %rax.
 * As a general rule : values aren't modified, 
 * we only create new ones (except for mutable structs of course) :
 * this makes assigning very easy. *)


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
    (* te2 in %r8/%rcx
     * te1 in %rax/%rdx *)
    compile_expr te2 ++ pushq !%rax ++
    compile_expr te1 ++ popq r8 ++
    movq (ind ~ofs:ofs_type r8) !%rcx ++     
    movq (ind ~ofs:ofs_type rax) !%rdx ++ 
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
        (* result in %rax *)
        label l1 ++
        movq (ind ~ofs:ofs_data rax) !%rax ++
        movq (ind ~ofs:ofs_data r8) !%r8 ++  
        begin match op with
          | Add -> addq !%r8 !%rax
          | Sub -> subq !%r8 !%rax
          | Mul -> imulq !%r8 !%rax
          | Mod -> 
            let l2 = code_label prg in 
              testq !%r8 !%r8 ++ jnz l2 ++
              error prg "modulo by zero" ++
            label l2 ++
              movq (imm 0) !%rdx ++
              idivq !%r8 ++ 
              movq !%rdx !%rax
          | Pow -> 
            let l2 = code_label prg in
              cmpq (imm 0) !%r8 ++ jge l2 ++
              error prg "negative exponent" ++
            label l2 ++
              movq !%rax !%rdi ++
              movq !%r8 !%rsi ++
              call "pow"
          | _ -> assert false
        end ++
        (* box the result *)
        movq !%rax !%rbx ++ (* %rbx is callee-saved *)
        allocate t_int64 ++
        movq !%rbx (ind ~ofs:ofs_data rax)
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
        movq (ind ~ofs:ofs_data rax) !%rax ++
        movq (ind ~ofs:ofs_data r8) !%r8 ++
        cmpq !%r8 !%rax ++
        (* result in %rbx (callee-saved) *)
        begin match op with
          | Leq -> setle !%bl
          | Lt -> setl !%bl
          | Geq -> setge !%bl
          | Gt -> setg !%bl
          | _ -> assert false
        end ++
        movzbq !%bl rbx ++
        (* box the result *)
        allocate t_bool ++
        movq !%rbx (ind ~ofs:ofs_data rax) 
      | _ -> assert false
    end

  
and compile_expr te = match te.expr with
  | TEbool b -> 
    let value = if b then 1 else 0 in
    allocate t_bool ++
    movq (imm value) (ind ~ofs:ofs_data rax)
  | TEint i ->
    allocate t_int64 ++
    movq (imm64 i) (ind ~ofs:ofs_data rax)
  | TEstring s ->
    let slabel = string_label prg s in
    allocate t_string ++
    movq (ilab slabel) (ind ~ofs:ofs_data rax)
  | TEnot te1 ->
    let l1 = code_label prg in
    compile_expr te1 ++
    movq (ind ~ofs:ofs_type rax) !%rcx ++
    cmpq (imm t_bool) !%rcx ++ je l1 ++
    error prg "invalid argument type for operator !" ++
    (* result in %rbx (callee-saved) *)
    label l1 ++
    movq (imm 1) !%rbx ++
    xorq (ind ~ofs:ofs_data rax) !%rbx ++
    (* box the result *)
    allocate t_bool ++
    movq !%rbx (ind ~ofs:ofs_data rax)
  | TEbinop (op, te1, te2) -> compile_binop op te1 te2
  | TEprint te_list ->
    let rec print_list = function
      | [] -> nop
      | te :: te_list ->
        compile_expr te ++ 
        movq !%rax !%rdi ++
        call "print" ++
        print_list te_list
    in
    print_list te_list ++
    allocate t_nothing
  | TEblock te_list ->
    let rec eval_args = function
      | [] -> allocate t_nothing
      | [te1] -> 
        (* don't overwrite %rax with 'nothing' *)
        compile_expr te1
      | te1 :: te_list ->
        compile_expr te1 ++
        eval_args te_list
    in
    eval_args te_list
  | TEif (cond, te1, te2) ->
    let l1 = code_label prg in
    let lfalse = code_label prg in
    let lend = code_label prg in
    compile_expr cond ++
    movq (ind ~ofs:ofs_type rax) !%rcx ++
    cmpq (imm t_bool) !%rcx ++ je l1 ++
    error prg "invalid condition type in 'if' expression" ++
    label l1 ++
      movq (ind ~ofs:ofs_data rax) !%rax ++
      testq !%rax !%rax ++
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
      (* this way the variables are uninitialized after the loop finished.
       * they were already uninitialized before the loop started *)
      uninitialize_vars vars ++
    label lcond ++
      compile_expr cond ++
      movq (ind ~ofs:ofs_type rax) !%rcx ++
      cmpq (imm t_bool) !%rcx ++ je l1 ++
      error prg "invalid condition type in 'while' expression" ++
    label l1 ++  
      movq (ind ~ofs:ofs_data rax) !%rax ++
      testq !%rax !%rax ++
      jnz lbody ++
      allocate t_nothing

  | TEfor (te1, te2, te3, vars) ->
    let l1 = code_label prg in
    let l2 = code_label prg in
    let lbody = code_label prg in
    let lcond = code_label prg in
    let lv = 
      List.find (function LoopVar _ -> true | _ -> false) vars 
    in
    (* lower bound in %r8/%rcx 
     * upper bound in %rax/%rdx *)
    compile_expr te1 ++ pushq !%rax ++
    compile_expr te2 ++
    popq r8 ++
    movq (ind ~ofs:ofs_type r8) !%rcx ++
    movq (ind ~ofs:ofs_type rax) !%rdx ++
      cmpq (imm t_int64) !%rcx ++ je l1 ++
      error prg "invalid 'for' loop bound type" ++
    label l1 ++
      cmpq (imm t_int64) !%rdx ++ je l2 ++
      error prg "invalid 'for' loop bound type" ++
    label l2 ++
      movq (ind ~ofs:ofs_data r8) !%r8 ++
      movq (ind ~ofs:ofs_data rax) !%rax ++
      (* the loop bounds are on the stack
       * (upper bound on top) *)
      pushq !%r8 ++ pushq !%rax ++ 
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
      uninitialize_vars vars ++
      (* increment the lower bound *)
      incq (ind ~ofs:8 rsp) ++
    label lcond ++ 
      movq (ind ~ofs:8 rsp) !%r8 ++
      movq (ind rsp) !%r9 ++
      cmpq !%r8 !%r9 ++
      jge lbody ++
      allocate t_nothing

  | TEaccess_var v ->
    begin match v with
      | StackLocal _ 
      | LoopVar _ ->
        let l1 = code_label prg in
        movq (ind ~ofs:(rbp_offset v) rbp) !%rax ++
        cmpq (imm uninit_value) !%rax ++ jne l1 ++
        error prg (Printf.sprintf "unitinialized variable %s" (var_name v)) ++
        label l1
      | Global g ->
        let l1 = code_label prg in
        movq (lab g.name) !%rax ++
        cmpq (imm uninit_value) !%rax ++ jne l1 ++
        error prg (Printf.sprintf "uninitialized variable %s" (var_name v)) ++
        label l1
      | _ -> failwith "not implemented"
    end
  | TEassign_var (v, te1) ->
    compile_expr te1 ++
    begin match v with
      | StackLocal _
      | LoopVar _ ->
        movq !%rax (ind ~ofs:(rbp_offset v) rbp)
      | Global g -> 
        register_global prg g.name;
        movq !%rax (lab g.name)
      | _ -> failwith "not implemented"
    end
  | _ -> failwith "not implemented"

let compile_decl = function
  | TDexpr (te, frame_size) -> 
    let code = 
      repeat frame_size (pushq (imm uninit_value)) ++
      compile_expr te ++
      popn frame_size 
    in
    add_code prg code
  | TDfunc f ->
    (* the return value is in %rax *)
    let code = 
      label (mangle_name f.fname f.param_types) ++
      pushq !%rbp ++
      movq !%rsp !%rbp ++
      repeat f.frame_size (pushq (imm uninit_value)) ++
      compile_expr f.code ++
      leave ++
      ret
    in
    add_func_code prg code
  | _ -> failwith "not implemented"

let compile_prog decls = 
  List.iter compile_decl decls;
  Asm_functions.add_asm_functions prg;
  let text = 
    globl "main" ++
    label "main" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    get_main_code prg ++
    leave ++
    movq (imm 0) !%rax ++
    ret ++ inline "\n" ++
    get_func_code prg
  in
  let data =
    (* do this after having built the whole text *) 
    get_data prg
  in 
  { text = text ; data = data }




