open Type_ast
open Pos_ast
open Type

(* contains the position of the error *)
exception SemanticError of range * string
let error pos msg =
  raise (SemanticError (pos, msg))

let reserved_func_names = ["div"; "print"; "println"]

let type_equal t1 t2 = match t1, t2 with
  | Tany, Tany
  | Tnothing, Tnothing
  | Tint64, Tint64 
  | Tbool, Tbool
  | Tstring, Tstring -> true 
  | Tstruct s1, Tstruct s2 when s1 = s2 -> true
  | _ -> false

let type_list_equal tl_1 tl_2 =
  if List.length tl_1 <> List.length tl_2 then false else
  let tl_1_2 = List.combine tl_1 tl_2 in
  match List.find_opt (fun (t1, t2) -> not (type_equal t1 t2)) tl_1_2 with
    | None -> true
    | Some _ -> false

let type_compatible t1 t2 = match t1, t2 with
  | Tany, _
  | _, Tany 
  | Tnothing, Tnothing
  | Tint64, Tint64 
  | Tbool, Tbool
  | Tstring, Tstring -> true 
  | Tstruct s1, Tstruct s2 when s1 = s2 -> true
  | _ -> false

let type_list_compatible tl_1 tl_2 =
  if List.length tl_1 <> List.length tl_2 then false else
  let tl_1_2 = List.combine tl_1 tl_2 in
  match List.find_opt (fun (t1, t2) -> not (type_compatible t1 t2)) tl_1_2 with
    | None -> true
    | Some _ -> false


(* expect ty to be compatible with a type in ty_list *)
let expect ty ty_list pos =
  if not (List.exists (type_compatible ty) ty_list) then
  let msg = Printf.sprintf 
    "this expression is expected to have type %s but has type %s"
    (String_utils.join " or " (List.map type_to_string ty_list))
    (type_to_string ty)
  in
  error pos msg

(* entry point *)
let rec type_prog prg =
  let ctx = Context.create () in
  Context.add_global ctx "nothing" Tnothing;
  List.iter (extract_decl ctx) prg;
  List.map (type_decl ctx) prg
 
(* first pass : extract global variables 
 * and function/struct signatures *)
and extract_decl ctx = function
  | PDexpr e -> extract_vars ctx e
  | PDfunc f -> extract_func ctx f
  | PDstruct s -> extract_struct ctx s

(* second pass *)
and type_decl ctx = function
  | PDexpr e -> 
    Context.update_frame_size ctx;
    let te = type_expr ctx e in
    TDexpr (te, Context.frame_size ctx)
  | PDfunc f -> 
    Context.update_frame_size ctx;
    Context.open_scope ctx;
    Context.enter_func ctx f.ret_type;
    List.iteri 
      (fun i (name, ty) -> Context.add_func_param ctx name ty i)
      f.params;
    extract_vars ctx f.code;
    let te_code = type_expr ctx f.code in
    expect te_code.ty [f.ret_type] f.code.pos;
    Context.leave_func ctx;
    Context.close_scope ctx;
    TDfunc { 
      fname = f.fname ;
      param_types = List.map snd f.params ;
      ret_type = f.ret_type ;
      code = te_code ;
      frame_size = Context.frame_size ctx
    }
  | PDstruct s -> 
    TDstruct {
      sname = s.sname ;
      mutab = s.mutab ;
      fields = s.fields
    }

and type_expr ctx pe = match pe.expr with
  | PEbool b -> { ty = Tbool ; expr = TEbool b }
  | PEint n -> { ty = Tint64 ; expr = TEint n }
  | PEstring s -> { ty = Tstring ; expr = TEstring s }
  | PEnot pe -> 
    let te = type_expr ctx pe in
    expect te.ty [Tbool] pe.pos;
    { ty = te.ty ; expr = TEnot te }
  | PEbinop (op, pe1, pe2) -> 
    let te1 = type_expr ctx pe1 in
    let te2 = type_expr ctx pe2 in
    let ty =  match op with
      | Eq | Neq -> Tbool
      | Add | Sub | Mul | Mod | Pow ->
        expect te1.ty [Tint64] pe1.pos;
        expect te2.ty [Tint64] pe2.pos;
        Tint64
      | Leq | Lt | Geq | Gt ->
        expect te1.ty [Tint64; Tbool] pe1.pos;
        expect te2.ty [Tint64; Tbool] pe2.pos;
        Tbool
      | And | Or ->
        expect te1.ty [Tbool] pe1.pos;
        expect te2.ty [Tbool] pe2.pos;
        Tbool
      | Div -> assert false (* the parser produces a function call instead *)
    in
    { ty = ty ; expr = TEbinop (op, te1, te2) }
  (* If doesn't introduce a local scope *)
  | PEif (pe1, pe2, pe3) ->
    let te1 = type_expr ctx pe1 in
    let te2 = type_expr ctx pe2 in
    let te3 = type_expr ctx pe3 in
    expect te1.ty [Tbool] pe1.pos;
    let ty = 
      if type_equal te2.ty te3.ty then te2.ty else Tany
    in
    { ty = ty ; expr = TEif (te1, te2, te3) }
  | PEwhile (pe1, pe2) ->
    let te1 = type_expr ctx pe1 in
    Context.open_scope ctx;
    extract_vars ctx pe2;
    let vars = Context.outer_scope ctx in
    let te2 = type_expr ctx pe2 in
    Context.close_scope ctx;
    let _ = expect te1.ty [Tbool] pe1.pos in
    { ty = Tnothing ; expr = TEwhile (te1, te2, vars) }
  | PEfor (x, pe1, pe2, pe3) ->
    let te1 = type_expr ctx pe1 in
    let te2 = type_expr ctx pe2 in
    expect te1.ty [Tint64] pe1.pos;
    expect te2.ty [Tint64] pe2.pos;
    Context.open_scope ctx;
    Context.add_loop_var ctx x;
    extract_vars ctx pe3;
    let vars = Context.outer_scope ctx in
    let te3 = type_expr ctx pe3 in
    Context.close_scope ctx;
    { ty = Tnothing ; expr = TEfor (te1, te2, te3, vars) }
  | PEblock [] -> { ty = Tnothing ; expr = TEblock [] }
  | PEblock pe_list -> 
    let te_list = List.map (type_expr ctx) pe_list in 
    let ty = (List.hd (List.rev te_list)).ty in 
    { ty = ty; expr = TEblock te_list }
  (* handles both function calls and struct creation *)
  | PEcall (fname, pe_args) ->
    let te_args = List.map (type_expr ctx) pe_args in
    let arg_types = List.map (fun te -> te.ty) te_args in
    begin match fname with
      | "div" ->  
        if List.length te_args <> 2
        then error pe.pos "function div expects two arguments" else
        let pe1 = List.nth pe_args 0 in
        let te1 = List.nth te_args 0 in
        let pe2 = List.nth pe_args 1 in
        let te2 = List.nth te_args 1 in
        expect te1.ty [Tint64] pe1.pos;
        expect te2.ty [Tint64] pe2.pos;
        { ty = Tint64 ; expr = TEbinop (Div, te1, te2) }
      | "print" ->
        { ty = Tnothing ; expr = TEprint te_args }
      | "println" ->
        let te_newline = { ty = Tstring ; expr = TEstring "\n" } in
        { ty = Tnothing ; expr = TEprint (te_args @ [te_newline]) } 
      | _ -> begin
        assert (not (List.mem fname reserved_func_names));
        (* check if there is a function or struct with
         * the right name that exists *)
        if not (Context.contains_struct ctx fname) && (Context.func_signatures ctx fname) = []
        then error pe.pos (Printf.sprintf "undefined function or structure %s" fname);
        (* determine all the functions/structs that have a compatible signature *)
        let call_infos = ref [] in
        (* struct instantiation *)
        if Context.contains_struct ctx fname then
        begin
          let s = Context.retrieve_struct ctx fname in
          let s_field_types = List.map snd s.fields in
          if type_list_compatible s_field_types arg_types 
          then call_infos := (StructCreation s_field_types) :: !call_infos
        end;
        (* function calls *)
        let func_sigs = Context.func_signatures ctx fname in
        let is_sig_compatible (p_types, _) = 
        type_list_compatible p_types arg_types
        in
        let comp_sigs = List.filter is_sig_compatible func_sigs in
        List.iter
          (fun (p_types, _) -> call_infos := (FuncCall p_types) :: !call_infos)
          comp_sigs;
        (* no compatible function/struct *)
        if !call_infos = []
        then error pe.pos (Printf.sprintf 
          "no function or struct named %s is compatible with the given argument types" fname);
        (* determine the return type *)
        let ret_types = ref (List.map snd comp_sigs) in
        List.iter
          (function StructCreation _ -> ret_types := (Tstruct fname) :: !ret_types | _ -> ())
          !call_infos;
        let upper_bound ty_list =
          if List.mem Tany ty_list then Tany else
          let first = List.hd ty_list in
          let diff = ref false in
          List.iter
            (fun ty -> if not (type_equal ty first) then diff := true)
            ty_list;
          if !diff then Tany else first
        in
        (* build the expression *)
        { ty = upper_bound !ret_types ; expr = TEcall (fname, !call_infos, te_args) }
      end
    end
  | PEreturn pe_opt ->
    if not (Context.is_in_func ctx) 
    then error pe.pos "return can only be used within a function"
    else 
    let f_ret_type = Context.func_ret_type ctx in
    let te_opt = match pe_opt with
      | Some pe1 -> 
        let te1 = type_expr ctx pe1 in
        expect te1.ty [f_ret_type] pe1.pos;
        Some te1
      | None ->
        if not (type_compatible Tnothing f_ret_type)
        then error pe.pos 
            ("can't return nothing in function with return type " ^ (type_to_string f_ret_type))
        else None
    in
    { ty = Tany ; expr = TEreturn te_opt }   
  | PEassign (pe_left, pe_right) ->
    let te_right = type_expr ctx pe_right in
    begin match pe_left.expr with
      | PEident x -> 
        let entry = 
          try Context.retrieve_entry ctx x
          with Not_found -> 
            failwith ("entry for variable " ^ x ^ " should have been added to the context")
        in
        (* we must assign a value compatible with the type of x *)
        expect te_right.ty [var_type entry] pe_right.pos;
        { ty = te_right.ty ; expr = TEassign_var (entry, te_right) }
      | PEdot (pe1, f_name) ->
        let te1 = type_expr ctx pe1 in
        let s = match Context.struct_with_field ctx f_name with
          | None -> error pe_left.pos (Printf.sprintf "there is no struct with field %s" f_name)
          | Some s -> expect te1.ty [Tstruct s.sname] pe1.pos; s
        in
        (* don't forget to check s is mutable *)
        if not s.mutab 
        then error pe.pos (Printf.sprintf "struct %s is not mutable" s.sname) else
        let field_ty = List.assoc f_name s.fields in
        expect te_right.ty [field_ty] pe_right.pos;
        (* careful : here we use the type of the field as the overall type,
         * not the type of the right hand side *)
        { ty = field_ty ; expr = TEassign_field (te1, f_name, te_right) }
      | _ -> error pe_left.pos "expexted an lvalue on left side of equal"
    end
  (* this has to be an access : if it were an assign,
   * we wouldn't have called type_expr on PEident *)
  | PEident x -> 
    let entry =
      try Context.retrieve_entry ctx x
      with Not_found -> error pe.pos ("undefined variable " ^ x)
    in
    { ty = var_type entry ; expr = TEaccess_var entry }
  (* this has to be an access: if it were an assign,
   * we wouldn't have called type_expr on this PEdot *)
  | PEdot (pe1, f_name) ->
    let te1 = type_expr ctx pe1 in
    let s = match Context.struct_with_field ctx f_name with
      | None -> error pe.pos (Printf.sprintf "there is no struct with field %s" f_name)
      | Some s -> expect te1.ty [Tstruct s.sname] pe1.pos; s
    in
    let ty = List.assoc f_name s.fields in
    { ty = ty ; expr = TEaccess_field (te1, f_name) }


(* adds all the variables defined in 
 * the outer-most scope of an expression
 * to the context, with type Any *)
and extract_vars ctx pe = match pe.expr with
  | PEnot pe1 -> extract_vars ctx pe1
  | PEbinop (_, pe1, pe2) ->
    extract_vars ctx pe1;
    extract_vars ctx pe2
  (* If doesn't introduce a local scope *)
  | PEif (pe1, pe2, pe3) ->
    extract_vars ctx pe1;
    extract_vars ctx pe2;
    extract_vars ctx pe3
  | PEfor (_, pe1, pe2, _) ->
    extract_vars ctx pe1;
    extract_vars ctx pe2
  | PEwhile (pe1, _) ->
    extract_vars ctx pe1
  | PEblock pe_list ->
    List.iter (extract_vars ctx) pe_list
  | PEassign (pe1, pe2) ->
    begin match pe1.expr with
      | PEident x ->
        if Context.has_local_scope ctx
        then Context.add_local ctx x
        else 
        begin
          (* if we are in the global scope, variable nothing is the global nothing
           * so we can't assign to it *)
          if x = "nothing" then error pe.pos "can't assign to variable nothing"
          else Context.add_global ctx x Tany
        end
      | PEdot (pe1, _) ->
        extract_vars ctx pe1
      | _ -> failwith "parser should generate PEdot or PEident on left hand side of PEassign"
    end;
    extract_vars ctx pe2
  | PEdot (pe1, _) ->
    extract_vars ctx pe1
  | PEcall (_, args) ->
    List.iter (extract_vars ctx) args
  | PEreturn (Some pe1) ->
    extract_vars ctx pe1
  | _ -> ()
  
(* adds the function f to the context *)
and extract_func ctx f =
  if List.mem f.fname reserved_func_names
  then error f.pos (f.fname ^ " is a reserved identifier")
  else
  (* check for duplicate param names *)
  let param_names = List.map fst f.params in
  let rec loop acc = function
    | [] -> ()
    | p_name :: param_names ->
      if List.mem p_name acc
      then error f.pos ("duplicate parameter " ^ p_name)
      else loop (p_name :: acc) param_names
  in
  loop [] param_names;
  (* check for ill-formed types *)
  let check_type = function
    | p_name, Tstruct s when not (Context.contains_struct ctx s) ->
      error f.pos (Printf.sprintf "unknown type %s of parameter %s" s p_name)
    | _ -> ()
  in
  List.iter check_type f.params;
  begin match f.ret_type with
    | Tstruct s when not (Context.contains_struct ctx s) ->
      error f.pos (Printf.sprintf "unknown return type %s" s)
    | _ -> ()
  end;
  (* check for another function with exactly the same signature
   * (not just a compatible signature) *)
  let func_param_types = 
    List.map fst (Context.func_signatures ctx f.fname)
  in
  let param_types = List.map snd f.params in
  begin match List.find_opt (type_list_equal param_types) func_param_types with
    | Some _ -> error f.pos "a function with the same name and argument types already exists"
    | None -> ()
  end;
  (* check for a structure with exactly the same fields *)
  if Context.contains_struct ctx f.fname then
  begin
    let s = Context.retrieve_struct ctx f.fname in
    let s_param_types = List.map snd s.fields in
    if type_list_equal s_param_types param_types
    then error f.pos "a structure with the same name and field types already exists"
  end;
  Context.add_func ctx f

and extract_struct ctx s =
  (* don't forget to check for reserved function names *)
  if List.mem s.sname reserved_func_names
  then error s.pos (s.sname ^ " is a reserved identifier");
  (* and duplicate structure names *)
  if Context.contains_struct ctx s.sname 
  then error s.pos ("struct " ^ s.sname ^ " is already defined");
  (* check for duplicate field names *)
  let field_names = List.map fst s.fields in
  let rec loop acc = function
    | [] -> ()
    | f_name :: field_names ->
      if List.mem f_name acc
      then error s.pos ("duplicate struct field " ^ f_name)
      else match Context.struct_with_field ctx f_name with
        | Some s2 -> 
          error s.pos (Printf.sprintf 
          "redefinition of field %s from struct %s" f_name s2.sname)
        | None -> loop (f_name :: acc) field_names
  in
  loop [] field_names;
  (* check for ill-formed types *)
  let check_field = function
    | f_name, Tstruct s2 -> 
      if s2 <> s.sname && not (Context.contains_struct ctx s2) 
      then error s.pos (Printf.sprintf 
        "unknwon type %s for field %s of structure %s" s2 f_name s.sname)
    | _ -> ()
  in
  List.iter check_field s.fields;
  (* check for a function with the exact same signature *)
  let s_param_types = List.map snd s.fields in
  let func_param_types =
    List.map fst (Context.func_signatures ctx s.sname)
  in
  begin match List.find_opt (type_list_equal s_param_types) func_param_types with
    | Some _ -> error s.pos "a function with the same name and argument types already exists"
    | None -> ()
  end;
  Context.add_struct ctx s 

