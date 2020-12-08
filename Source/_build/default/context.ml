open Type_ast

type local_scope = {
  locals : (string, var) Hashtbl.t ;
  (* total stack size under %rbp used by local variables. 
   * doesn't account for function params which are stored above %rbp.
   * not all variables occupy only one stack slot,
   * for instance a loop variable occupies 2 slots *)
  mutable stack_size : int
}

type func_sig = Type.ty list * Type.ty

type t = { 
  (* the return type of the enclosing function *)
  mutable func_ret_type : Type.ty option ;
  funcs_by_name : (string, func_sig list) Hashtbl.t ;

  structs : (string, Pos_ast.struc) Hashtbl.t;
  structs_by_field : (string, Pos_ast.struc) Hashtbl.t;

  globals : (string, var) Hashtbl.t ;
  mutable local_scopes : local_scope list ;
  (* sum of the stack sizes of local scopes *)
  mutable stack_size : int
}

let create () = {
  func_ret_type = None ;
  funcs_by_name = Hashtbl.create 4 ;

  structs = Hashtbl.create 4 ;
  structs_by_field = Hashtbl.create 4 ;

  globals = Hashtbl.create 4 ;
  local_scopes = [] ;
  stack_size = 0
}


(*** SCOPES ***)

let open_scope ctx =
  let new_scope = { 
    locals = Hashtbl.create 4 ;
    stack_size = 0 
  } in
  ctx.local_scopes <- new_scope :: ctx.local_scopes

let close_scope ctx =
  if ctx.local_scopes = [] 
  then raise (Invalid_argument "no local scope to close")
  else begin 
    ctx.stack_size <- ctx.stack_size - (List.hd ctx.local_scopes).stack_size;
    ctx.local_scopes <- List.tl ctx.local_scopes
  end
   
let has_local_scope ctx =
  ctx.local_scopes <> []

(* returns the list of variables that were created
 * in the outer-most local scope *)
let outer_scope ctx =
  let scope = List.hd ctx.local_scopes in
  List.of_seq (Hashtbl.to_seq_values scope.locals)


(*** FUNCTIONS ***)

let enter_func ctx ret_type =
  if ctx.func_ret_type <> None
  then raise (Failure "can't open a function in a function")
  else ctx.func_ret_type <- Some ret_type

let leave_func ctx =
  if ctx.func_ret_type = None 
  then raise (Failure "can't leave a function if we are not in a function")
  else ctx.func_ret_type <- None

let is_in_func ctx =
  ctx.func_ret_type <> None

let func_ret_type ctx = match ctx.func_ret_type with
  | Some ty -> ty
  | None -> raise (Failure "no function return type")

let add_func ctx (f : Pos_ast.func) =
  let sigs = 
    try Hashtbl.find ctx.funcs_by_name f.fname
    with Not_found -> []
  in
  let fsig = (List.map snd f.params, f.ret_type) in
  Hashtbl.add ctx.funcs_by_name f.fname (fsig :: sigs)

let func_signatures ctx fname =
  try Hashtbl.find ctx.funcs_by_name fname
  with Not_found -> []

let add_func_param ctx name ty index =
  let entry = FuncParam {
    name = name ;
    ty = ty ;
    index = index
  } in
  let scope = match ctx.local_scopes with
    | [scope] -> scope
    | _ -> raise (Invalid_argument 
      "can only add a function parameter when there is exactly one local scope")
  in 
  Hashtbl.add scope.locals name entry 


(*** STRUCTS ***)

let add_struct ctx (s : Pos_ast.struc) =
  Hashtbl.add ctx.structs s.sname s;
  let add_field (f_name, _) =
    Hashtbl.add ctx.structs_by_field f_name s
  in
  List.iter add_field s.fields

let contains_struct ctx sname = 
  Hashtbl.mem ctx.structs sname

let retrieve_struct ctx sname =
  Hashtbl.find ctx.structs sname

let struct_with_field ctx f_name =
  try Some (Hashtbl.find ctx.structs_by_field f_name)
  with Not_found -> None

(*** VARIABLES ***)

(* We can't easily determine the type of a variable at compile time.
 * This is related to several (strange) behaviours of Julia : 
 * the (complete) program x = x + 1 can compile
 * (but yields a run-time error) : this is because variables are created
 * (and left uninitialized) when their enclosing scope is opened. 
 * Also, the program x = 3; if <cond> x = "cat" end; print(x) 
 * compiles and runs : a same variable can have multiple types in different
 * parts of the program, and even have arbitrary (i.e. not known at compile time)
 * types in the same part of the program (here in the print statement).
 * Being more precise would require some additional work. *)

let retrieve_entry ctx name = 
  let rec loop = function
    | [] -> Hashtbl.find ctx.globals name
    | scope :: local_scopes ->
      begin 
        try Hashtbl.find scope.locals name
        with Not_found -> loop local_scopes
      end
  in
  loop ctx.local_scopes


let add_global ctx name ty =
  let entry = Global { name = name ; ty = ty } in
  (* TODO : handle multiple global variables with same name *)
  Hashtbl.add ctx.globals name entry

let add_local ctx name =
  let rec loop = function
     | []-> 
      (* create a new stack local *)
      let outer_scope = List.hd ctx.local_scopes in
      let new_entry = StackLocal {
        name = name ;
        index = ctx.stack_size
      } in
      Hashtbl.add outer_scope.locals name new_entry;
      outer_scope.stack_size <- outer_scope.stack_size + 1;
      ctx.stack_size <- ctx.stack_size + 1
    | scope :: local_scopes ->
      if Hashtbl.mem scope.locals name
      (* re-use an existing entry *)
      then () 
      else loop local_scopes 
  in
  loop ctx.local_scopes

let add_loop_var ctx name =
  (* loop variables are always new variables in the outer scope *)
  let outer_scope = List.hd ctx.local_scopes in
  let entry = LoopVar {
    name = name ; 
    index = ctx.stack_size
  } in
  Hashtbl.add outer_scope.locals name entry;
  outer_scope.stack_size <- outer_scope.stack_size + 2;
  ctx.stack_size <- ctx.stack_size + 2

