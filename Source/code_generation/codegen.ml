open Type_ast
open X86_64
open Binop
open Program
open Type
open Asm_functions_labels

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
  else if op = Eq then
    compile_expr te1 ++ pushq !%rax ++
    compile_expr te2 ++
    popq rdi ++
    movq !%rax !%rsi ++
    call asm_equal_lab
  else if op = Neq then
    (* compile Neq as (Not Eq) *)
    let te_eq = { ty = Tbool ; expr = TEbinop (Eq, te1, te2) } in
    compile_expr { ty = Tbool ; expr = TEnot te_eq }
  else  
    (* te2 in %r8/%rcx
     * te1 in %rax/%rdx *)
    compile_expr te2 ++ pushq !%rax ++
    compile_expr te1 ++ popq r8 ++
    movq (ind ~ofs:ofs_type r8) !%rcx ++     
    movq (ind ~ofs:ofs_type rax) !%rdx ++ 
    begin match op with
      | Add | Sub | Mul | Mod | Div | Pow ->
        let l1 = code_label prg in
        let lerror = code_label prg in
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
              (* sign extend %rax into %rdx *)
              cqto ++
              idivq !%r8 ++ 
              movq !%rdx !%rax
          | Div ->
            let l2 = code_label prg in 
              testq !%r8 !%r8 ++ jnz l2 ++
              error prg "division by zero" ++
            label l2 ++
              (* sign extend %rax into %rdx *)
              cqto ++
              (* quotient is in %rax *)
              idivq !%r8
          | Pow -> 
            let l2 = code_label prg in
              cmpq (imm 0) !%r8 ++ jge l2 ++
              error prg "negative exponent" ++
            label l2 ++
              movq !%rax !%rdi ++
              movq !%r8 !%rsi ++
              call asm_pow_lab
          | _ -> assert false
        end ++
        (* box the result *)
        pushq !%rax ++ 
        allocate prg t_int64 ++
        popq r8 ++
        movq !%r8 (ind ~ofs:ofs_data rax)
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
        (* result in %rax *)
        begin match op with
          | Leq -> setle !%al
          | Lt -> setl !%al
          | Geq -> setge !%al
          | Gt -> setg !%al
          | _ -> assert false
        end ++
        movzbq !%al rax ++
        pushq !%rax ++
        (* box the result *)
        allocate prg t_bool ++
        popq r8 ++
        movq !%r8 (ind ~ofs:ofs_data rax) 
      | _ -> assert false
    end

  
and compile_expr te = match te.expr with
  | TEbool b -> 
    let value = if b then 1 else 0 in
    allocate prg t_bool ++
    movq (imm value) (ind ~ofs:ofs_data rax)
  | TEint i ->
    allocate prg t_int64 ++
    movq (imm64 i) (ind ~ofs:ofs_data rax)
  | TEstring s ->
    let slabel = string_label prg s in
    allocate prg t_string ++
    movq (ilab slabel) (ind ~ofs:ofs_data rax)
  | TEnot te1 ->
    let l1 = code_label prg in
    compile_expr te1 ++
    movq (ind ~ofs:ofs_type rax) !%rcx ++
    cmpq (imm t_bool) !%rcx ++ je l1 ++
    error prg "invalid argument type for operator !" ++
    (* result in %r8 *)
    label l1 ++
    movq (imm 1) !%r8 ++
    xorq (ind ~ofs:ofs_data rax) !%r8 ++
    (* box the result *)
    pushq !%r8 ++
    allocate prg t_bool ++
    popq r8 ++
    movq !%r8 (ind ~ofs:ofs_data rax)
  | TEbinop (op, te1, te2) -> compile_binop op te1 te2
  | TEprint te_list ->
    let rec print_list = function
      | [] -> nop
      | te :: te_list ->
        compile_expr te ++ 
        movq !%rax !%rdi ++
        call asm_print_lab ++
        print_list te_list
    in
    print_list te_list ++
    allocate prg t_nothing
  | TEblock te_list ->
    let rec eval_args = function
      | [] -> allocate prg t_nothing
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
      allocate prg t_nothing

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
      allocate prg t_int64 ++
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
      (* pop the bounds *)
      popn 2 ++
      allocate prg t_nothing

  | TEaccess_var v ->
    begin match v with
      | StackLocal _ 
      | LoopVar _ ->
        let l1 = code_label prg in
        movq (ind ~ofs:(rbp_offset v) rbp) !%rax ++
        cmpq (imm uninit_value) !%rax ++ jne l1 ++
        error prg (Printf.sprintf "unitinialized variable %s" (var_name v)) ++
        label l1
      | FuncParam _ ->
        (* no need to check for uninitialized value *)
        movq (ind ~ofs:(rbp_offset v) rbp) !%rax
      | Global g ->
        let l1 = code_label prg in
        movq (lab g.name) !%rax ++
        cmpq (imm uninit_value) !%rax ++ jne l1 ++
        error prg (Printf.sprintf "uninitialized variable %s" (var_name v)) ++
        label l1
    end

  | TEaccess_field (te1, field) ->
    let s = get_struct_with_field prg field in 
    let field_index = 
      let rec loop i = function
        | [] -> assert false
        | (fname, _) :: _ when fname = field -> i 
        | _ :: fields -> loop (i+1) fields
      in 
      loop 0 s.fields
    in
    let l1 = code_label prg in
    compile_expr te1 ++
    (* check te1 is of the right type *)
    movq (ind ~ofs:ofs_type rax) !%rcx ++
    cmpq (imm (get_struct_type_number prg s.sname)) !%rcx ++
    je l1 ++
    error prg (Printf.sprintf 
      "can't access field %s: expected an instance of struct %s" field s.sname) ++
    label l1 ++
    movq (ind ~ofs:(field_offset field_index) rax) !%rax

  | TEassign_var (v, te1) ->
    compile_expr te1 ++
    begin match v with
      | StackLocal _
      | LoopVar _ 
      | FuncParam _ ->
        movq !%rax (ind ~ofs:(rbp_offset v) rbp)
      | Global g -> 
        register_global prg g.name;
        movq !%rax (lab g.name)
    end

  | TEassign_field (te1, field, te2) ->
    let s = get_struct_with_field prg field in 
    let field_index = 
      let rec loop i = function
        | [] -> assert false
        | (fname, _) :: _ when fname = field -> i 
        | _ :: fields -> loop (i+1) fields
      in 
      loop 0 s.fields
    in
    let ftype = List.assoc field s.fields in
    let l1 = code_label prg in
    (* compile te1 and check it is of the right type *)
    compile_expr te1 ++
    movq (ind ~ofs:ofs_type rax) !%rcx ++
    cmpq (imm (get_struct_type_number prg s.sname)) !%rcx ++
    je l1 ++
    error prg (Printf.sprintf 
      "can't assign to field %s: expected an instance of struct %s" field s.sname) ++
    label l1 ++
    pushq !%rax ++
    (* compile te2 and check it is of the right type *)
    compile_expr te2 ++
    begin match ftype with
      | Tany -> nop
      | _ -> 
        let l2 = code_label prg in
        movq (ind ~ofs:ofs_type rax) !%rcx ++
        cmpq (imm (type_number prg ftype)) !%rcx ++
        je l2 ++
        error prg (Printf.sprintf 
          "can't assign to field %s : expected a value of type %s"
          field
          (type_to_string ftype)) ++
        label l2
    end ++
    popq r8 ++
    movq !%rax (ind ~ofs:(field_offset field_index) r8)

  | TEcall (fname, call_infos, te_args) ->
    assert (List.length call_infos > 0);
    let lend = code_label prg in
    let rec loop = function
      | [] -> nop
      | te1 :: te_args ->
        compile_expr te1 ++
        pushq !%rax ++
        loop te_args
    in
    (* push the args : first arg is on top of the stack *)
    loop (List.rev te_args) ++
    dispatch_call fname call_infos 0 (List.length te_args) lend ++
    label lend ++
    (* pop the args *)
    popn (List.length te_args)

  | TEreturn te_opt ->
    (* put the return value in %rax *)
    begin match te_opt with
      | None -> allocate prg t_nothing
      | Some te1 -> 
        compile_expr te1
    end ++
    check_return_type prg ++
    (* exit the function *)
    leave ++
    ret


(* n is the number of arguments
 * i is the index of the argument we are dispatching against
 * lend is the label to jump to after the call is completed *)
and dispatch_call name call_infos i n lend =
  if List.length call_infos = 0 then
    error prg (Printf.sprintf 
      "no overload of function %s matches the given argument types" name)
  else if i >= n then
  begin
    match most_precise_method call_infos with
      | None -> 
        error prg (Printf.sprintf 
          "multiple overloads of function %s match the given argument types" name)
      | Some ci -> compile_call name ci lend
  end
  else
  begin
    (* split the call infos according to the 
    * type of their n-th parameter *)
    let ci_in_group = Hashtbl.create 4 in
    let t_any_group = ref [] in
    let assign_group ci =
      let ty = List.nth (call_info_sig ci) i in
      if ty = Tany
      then t_any_group := ci :: !t_any_group
      else
        let g = 
          try Hashtbl.find ci_in_group ty
          with Not_found -> [] 
        in
        Hashtbl.add ci_in_group ty (ci :: g)
    in
    List.iter assign_group call_infos;
    let buckets = 
      Seq.map 
        (* don't forget to add the call-infos for type Tany
         * to each group *)
        (fun (ty, g) -> (ty, g @ !t_any_group, code_label prg))
        (Hashtbl.to_seq ci_in_group)
      |> List.of_seq
    in
    (* actual assembly code *)
    (* get the type of the i-th argument into %rax *)
    movq (ind ~ofs:(8*i) rsp) !%rax ++
    movq (ind ~ofs:ofs_type rax) !%rax ++
    let rec compile_jumps = function
      | [] -> nop
      | (ty, _, la) :: buckets ->
        cmpq (imm (type_number prg ty)) !%rax ++
        je la ++
        compile_jumps buckets
    in
    compile_jumps buckets ++
    (* if every jump failed *)
    dispatch_call name !t_any_group (i+1) n lend ++
    (* labels *)
    let rec compile_labels = function
      | [] -> nop
      | (_, group, la) :: tl ->
        label la ++
        (* recurse, only with the call-infos
        * that have parameter i of type compatible with ty *)
        dispatch_call name group (i+1) n lend ++
        compile_labels tl 
    in 
    compile_labels buckets
  end

(* The call_infos all have distinct signatures,
 * and accept the same number of arguments.
 * All signatures are compatible (i.e. if they differ
 * in a parameter type then one must be Any).
 * Returns (if it exists) the minimum 
 * for the partial order (t_1, ..., t_k) <= (r_1, ..., r_k)
 * iff for all i, r_i = t_i or r_i = Any *)
and most_precise_method call_infos =
  (* decide if ci1 <= ci2 *)
  let is_more_precise ci1 ci2 =
    let sig1 = call_info_sig ci1 in 
    let sig2 = call_info_sig ci2 in
    (List.for_all 
      (fun (t, r) -> type_equal t r || r = Tany)
      (List.combine sig1 sig2))
  in
  List.find_opt
    (fun ci -> List.for_all (is_more_precise ci) call_infos)
    call_infos

and compile_call name ci lend = match ci with
  | FuncCall param_types ->
    call (mangle_name name param_types) ++
    jmp lend
  | StructCreation field_types ->
    allocate prg (get_struct_type_number prg name) ++
    let rec loop i = 
      if i >= List.length field_types then nop
      else begin
        movq (ind ~ofs:(8*i) rsp) !%r8 ++
        movq !%r8 (ind ~ofs:(field_offset i) rax) ++
        loop (i+1)
      end        
    in 
    loop 0 ++
    jmp lend


let compile_decl = function
  | TDexpr (te, frame_size) -> 
    let code = 
      repeat frame_size (pushq (imm uninit_value)) ++
      compile_expr te ++
      popn frame_size 
    in
    add_code prg code
  | TDfunc f ->
    set_enclosing_func prg (Some f);
    (* the return value is in %rax *)
    let code = 
      label (mangle_name f.fname f.param_types) ++
      pushq !%rbp ++
      movq !%rsp !%rbp ++
      repeat f.frame_size (pushq (imm uninit_value)) ++
      compile_expr f.code ++
      check_return_type prg ++
      leave ++
      ret
    in
    add_func_code prg code;
    set_enclosing_func prg None
  | TDstruct _ -> ()

let compile_prog decls = 
  (* init the program data structure *)
  set_enclosing_func prg None;
  let structs = 
    let rec loop acc = function
      | [] -> List.rev acc
      | TDstruct s :: decls -> loop (s :: acc) decls
      | _ :: decls -> loop acc decls
    in 
    loop [] decls 
  in
  register_structs prg structs;

  (* add the nothing variable *)
  add_data prg (label nothing_lab ++ dquad [0]);

  List.iter compile_decl decls;
  Asm_functions.add_asm_functions prg;
  let text = 
    globl "main" ++
    label "main" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    (* initialize the nothing variable
     * (we have to do it without calling allocate) *)
    movq (imm (size_of prg t_nothing)) !%rdi ++
    call "malloc" ++
    movq (imm t_nothing) (ind ~ofs:ofs_type rax) ++
    movq !%rax (lab nothing_lab) ++
    (* all the global expressions go into main *)
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




