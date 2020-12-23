open Pos_ast
open Lexer
open Type
open Binop

(* READ ME : if you forget how the parser works,
 * parse_expr is a good example to look at *)

(* interface for the input of the parser,
 * i.e. encapsulates the behaviour of the lexer *)
type stream = {
  peek : unit -> token ;
  next : unit -> token ;
  (* returns the position of the last character of the last token produced *)
  last_pos : unit -> position ;
  (* returns the position of the first character of the next token to be produced, 
   * i.e. the token returned by the next call to peek () *)
  next_pos : unit -> position
}

exception SyntaxError of string
let error msg =
  raise (SyntaxError msg)

let make_range p1 p2 = { first = p1 ; last = p2 }

(* returns the matched token *)
let expect s tok msg =
  if s.peek () = tok then s.next ()
  else error msg

(* returns the matched identifier *)
let expect_ident s =
  match s.peek () with
    | IDENT i -> let _ = s.next () in i
    | _ -> error "expected an identifier"

(* returs the matched identifier string *)
let expect_ident_lp s =
  match s.peek () with
    | IDENT_LP i -> let _ = s.next () in i
    | _ -> error "expected an identifier followed by a right parenthesis"


let binop_of_token = function
  | ADD -> Add
  | SUB -> Sub
  | MUL -> Mul
  | MOD -> Mod
  | EQ -> Eq
  | NEQ -> Neq
  | LT -> Lt
  | LEQ -> Leq
  | GT -> Gt
  | GEQ -> Geq
  | AND -> And
  | OR -> Or
  | _ -> raise (Invalid_argument "token isn't a binop")

let type_of_string = function
  | "Any" -> Tany
  | "Nothing" -> Tnothing
  | "Bool" -> Tbool
  | "Int64" -> Tint64
  | "String" -> Tstring
  | t -> Tstruct t


(* entry point *)
let rec parse_prog s = parse_decls s
    
and parse_decls s = 
  if s.peek () = EOF 
  then let _ = s.next () in []
  else 
  let decl = match s.peek () with
    | (MUTABLE | STRUCT) -> 
      (* don't consume token *)
      let str = parse_struct s in
      PDstruct str
    | FUNCTION ->
      (* don't consume token *)
      let f = parse_function s in
      PDfunc f
    | _ ->
      let e = parse_expr s in 
      PDexpr e
  in
  let _ = expect s SEMICOLON "expected a semicolon" in
  decl :: (parse_decls s)

and parse_struct s =
  let first_p = s.next_pos () in
  let mutab = match s.peek () with
    | MUTABLE -> let _ = s.next () in true
    | _ -> false
  in
  let _ = expect s STRUCT "expected struct keyword" in
  let sname = expect_ident s in
  let fields = parse_struct_fields s in
  let _ = expect s END "expected end keyword" in
  let range = make_range first_p (s.last_pos ()) in
  {
    pos = range ;
    sname = sname ;
    mutab = mutab ;
    fields = fields
  }

(* fields ::= param ";" fields | ";" fields | param | epsilon *)
and parse_struct_fields s = match s.peek () with
  | SEMICOLON ->
    let _ = s.next () in
    parse_struct_fields s
  | _ ->
    try
      let p = parse_param s in
      match s.peek () with
        | SEMICOLON ->
          let _ = s.next () in
          p :: (parse_struct_fields s)
        | _ -> [p]
    with SyntaxError _ -> []


and parse_function s =
  let first_p = s.next_pos () in
  let _ = expect s FUNCTION "expected function keyword" in
  let name = expect_ident_lp s in
  let params = match s.peek () with
    | RP -> [] (* don't consume the token *)
    | _ -> parse_function_params s
  in
  let _ = expect s RP "expected a right parenthesis" in
  let ret_type = match s.peek () with
    | DOUBLECOLON ->
      let _ = s.next () in
      let t = expect_ident s in
      type_of_string t
    | _ -> Tany
  in
  let body = parse_block s in
  let _ = expect s END "expected end keyword" in
  let range = make_range first_p (s.last_pos ()) in
  {
    pos = range ;
    fname = name ;
    params = params ;
    ret_type = ret_type ;
    code = body
  }

(* there must be at least one param here
 * params ::= p "," params | p *)
and parse_function_params s =
  let p = parse_param s in
  match s.peek () with
    | COMMA ->
      let _ = s.next () in
      p :: (parse_function_params s)
    | _ -> [p]
       

and parse_param s = 
  let i = expect_ident s in
  match s.peek () with
    | DOUBLECOLON ->
      let _ = s.next () in
      let t = expect_ident s in
        i, (type_of_string t)
    | _ -> i, Tany
    
(* this HAS to return a PEblock (for instance concat_blocks assumes this)
 * block ::= expr ";" block | ";" block | expr | epsilon *)
and parse_block s = 
  match s.peek () with
    | SEMICOLON ->
      let _ = s.next () in
      (* don't count the first semicolon in the block *)
      parse_block s
    | _ ->
      try 
        let e = parse_expr s in
        begin match s.peek () with
          | SEMICOLON -> 
            let _ = s.next () in
            let b = parse_block s in
            if b.expr = PEblock [] then { pos = e.pos ; expr = PEblock [e] } else 
            let range = make_range e.pos.first b.pos.last in
            { pos = range ; expr = concat_blocks (PEblock [e]) b.expr }
          | _ -> { pos = e.pos ; expr = PEblock [e] }
        end
      (* relies on the assumption that parse_expr
       * doesn't consume input when there is no expression
       * to read : it just raises a syntax error *)
      with SyntaxError _ ->
        (* here we can't use s.last_pos () as
         * the last position of the range.
         * we arbitrarily choose the range containing
         * the first character of the token following the empty block *)
        let p = s.next_pos () in
        let range = make_range p p in
        { pos = range ; expr = PEblock [] }
      
and parse_expr s = 
  let first_p = s.next_pos () in
  let e = match s.peek () with
    | RETURN ->
      let _ = s.next () in
      let e = try Some (parse_expr s) with SyntaxError _ -> None
      in PEreturn e
    | IF ->
      let _ = s.next () in
      let cond = parse_expr s in
      let body = parse_block s in
      let el = parse_else s in
      PEif (cond, body, el)
    | WHILE ->
      let _ = s.next () in
      let cond = parse_expr s in
      let body = parse_block s in
      let _ = expect s END "expected end keyword" in
      PEwhile (cond, body)
    | FOR ->
      let _ = s.next () in
      let i = expect_ident s in
      let _ = expect s EQUAL "expected an equal sign" in
      let e1 = parse_expr s in
      let _ = expect s COLON "expected a colon" in
      let e2 = parse_expr s in
      let body = parse_block s in
      let _ = expect s END "expected end keyword" in
      PEfor (i, e1, e2, body)
    | _ -> (parse_equal s).expr
  in
  let range = make_range first_p (s.last_pos ()) in 
  { pos = range ; expr = e }

and parse_else s = 
  match s.peek () with
    | END -> 
      let _ = s.next () in
      let first_p = s.next_pos () in
      let range = make_range first_p (s.last_pos ()) in
      { pos = range ; expr = PEblock [] }
    | ELSE -> 
      let _ = s.next () in
      let b = parse_block s in
      let _ = expect s END "expected end keyword" in
      b
    | ELSEIF ->
      let first_p = s.next_pos () in
      let _ = s.next () in
      let cond = parse_expr s in
      let body = parse_block s in
      let el = parse_else s in
      let range = make_range first_p (s.last_pos ()) in
      { pos = range ; expr = PEif (cond, body, el) }
    | _ -> error "expected end, else or elseif keyword" 

and parse_equal s =
  let first_p = s.next_pos () in
  let left = parse_or s in
  let e = match s.peek () with
    | EQUAL ->
      let _ = s.next () in
      (* check for an lvalue on the left.
       * this is to pass the syntax tests,
       * eventhough I personally would have checked this
       * during semantic analysis *)
      begin match left.expr with
        | PEident _ | PEdot _ -> ()
        | _ -> error "left hand side of equal must be an lvalue"
      end;
      let right = parse_equal s in
      PEassign (left, right)
    | _ -> left.expr
  in
  let range = make_range first_p (s.last_pos ()) in
  { pos = range ; expr = e }

(* constructs a parser for terms (parsed with "parse_term")
 * separated with binary operators from "op_list",
 * with left associativity *)
and binop_parser op_list parse_term s =
  let first_p = s.next_pos () in  
  let left = ref (parse_term s) in
  let rec loop () = 
    if List.mem (s.peek ()) op_list then
      let op = s.next () in
      let right = parse_term s in
      let range = make_range first_p (s.last_pos ()) in
      left := { pos = range ; expr = PEbinop (binop_of_token op, !left, right) };
      loop ()
    else !left.expr
  in 
  let e = loop () in
  let range = make_range first_p (s.last_pos ()) in
  { pos = range ; expr = e }

and parse_or s = binop_parser [OR] parse_and s
and parse_and s = binop_parser [AND] parse_comp s
and parse_comp s = binop_parser [EQ; NEQ; LT; LEQ; GT; GEQ] parse_add_sub s
and parse_add_sub s = binop_parser [ADD; SUB] parse_mul_mod s
and parse_mul_mod s = binop_parser [MUL; MOD] parse_unary s

and parse_unary s = 
  let first_p = s.last_pos () in
  let e = match s.peek () with
    | SUB -> 
      let _ = s.next () in
      let e = parse_unary s in
      (* dummy range for the imaginary '0' we substract from *)
      let range = make_range first_p first_p in 
      PEbinop (Sub, { pos = range ; expr = PEint Int64.zero }, e)
    | NOT ->
      let _ = s.next () in
      let e = parse_unary s in
        PEnot e
    | _ -> (parse_pow s).expr
  in
  let range = make_range first_p (s.last_pos ()) in
  { pos = range ; expr = e }

(* right associative *)
and parse_pow s =
  let first_p = s.next_pos () in
  let left = parse_dot s in
  let e = match s.peek () with
    | POW -> 
      let _ = s.next () in
      let right = parse_dot s in
        PEbinop (Pow, left, right)
    | _ -> left.expr
  in
  let range = make_range first_p (s.last_pos ()) in
  { pos = range ; expr = e }

(* left associative *)
and parse_dot s =
  let first_p = s.next_pos () in
  let left = ref (parse_atom s) in
  let rec loop () = match s.peek () with
    | DOT ->
      let _ = s.next () in
      let i = expect_ident s in
      let range = make_range first_p (s.last_pos ()) in
      left := { pos = range ; expr = PEdot (!left, i) };
      loop ()
    | _ -> !left.expr
  in
  let e = loop () in
  let range = make_range first_p (s.last_pos ()) in
  { pos = range ; expr = e }

and parse_atom s = 
  let first_p = s.next_pos () in
  let e = match s.peek () with
    | INT n -> let _ = s.next () in PEint n
    | TRUE -> let _ = s.next () in PEbool true
    | FALSE -> let _ = s.next () in PEbool false
    | STRING ss -> let _ = s.next () in PEstring ss
    | IDENT i -> let _ = s.next () in PEident i
    | INT_IDENT (n, i) ->
      let _ = s.next () in
      (* range containing only the INT_IDENT token *)
      let range = make_range first_p (s.last_pos ()) in
      let left = { pos = range ; expr = PEint n } in
      let right = { pos = range ; expr = PEident i } in
      PEbinop (Mul, left, right)
    (* expr ::= "(" expr ")"-ident
     * expr ::= "(" bloc1 ")" *)
    | LP ->
      let _ = s.next () in
      let e = parse_expr s in
      begin match s.peek () with
        | SEMICOLON ->
          let _ = s.next () in
          let b = parse_block s in
          let _ = expect s RP "expected a right parenthesis" in
          concat_blocks (PEblock [e]) b.expr
        | RP -> let _ = s.next () in e.expr
        | RP_IDENT i -> 
          let first_p = s.next_pos () in
          let _ = s.next () in 
          (* range containing only the RP_IDENT token *)
          let range = make_range first_p (s.last_pos ()) in
          let right = { pos = range ; expr = PEident i } in
          PEbinop (Mul, e, right)
        | _ -> error "expected a right parenthesis or a semicolon"
      end
    | INT_LP n ->
      let _ = s.next () in
      let last_p = s.last_pos () in
      let new_first_p = s.next_pos () in
      let e = parse_expr s in
      let b = match s.peek () with
        | SEMICOLON ->
          let _ = s.next () in
          (parse_block s).expr
        | _ -> PEblock []
      in
      let new_last_p = s.last_pos () in
      let _ = expect s RP "expected a right parenthesis" in
      (* range containing only the INT_LP token *)
      let left_range = make_range first_p last_p in
      let left = { pos = left_range ; expr = PEint n } in
      let right_range = make_range new_first_p new_last_p in
      let right = { pos = right_range ; expr = concat_blocks (PEblock [e]) b } in
      PEbinop (Mul, left, right)
    | IDENT_LP f -> 
      let _ = s.next () in
      let args = match s.peek () with
        | RP -> [] (* don't consume token *)
        | _ -> parse_function_args s 
      in
      let _ = expect s RP "expected a right parenthesis" in
      PEcall (f, args)
    | _ -> error "expected an expression"
  in
  let range = make_range first_p (s.last_pos ()) in
  { pos = range ; expr = e }

and concat_blocks b1 b2 = match b1, b2 with
  | PEblock el1, PEblock el2 -> PEblock (el1 @ el2)
  | _ -> raise (Invalid_argument "arguments of concat_block should be Eblocks")


(* there must be at least one arg here *)
and parse_function_args s =
  let e = parse_expr s in
  match s.peek () with
    | COMMA -> 
      let _ = s.next () in
      e :: (parse_function_args s)
    | _ -> [e]
  
 
