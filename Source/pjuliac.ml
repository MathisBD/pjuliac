open Lexing

let parse_only = ref false
let type_only = ref false
let verbose = ref false
let file = ref ""

let correct_format f =
  let r = Str.regexp ".+\\.jl$" in
  Str.string_match r f 0

let anon_param f =
  if !file <> "" then 
    raise (Arg.Bad "expected a single julia file")
  else if not (correct_format f) then 
    raise (Arg.Bad "expected a julia file")
  else if not (Sys.file_exists f) then
    raise (Arg.Bad ("no such file : " ^ f))
  else
    file := f

let specs = [
  ("--type-only", Arg.Set type_only, ": stop after typing");
  ("--parse-only", Arg.Set parse_only, ": stop after parsing");
  ("--verbose", Arg.Set verbose, ": if used with --type-only or --parse-only, print the AST")
]

let usage =
  Printf.sprintf "usage: %s [options] <file>" Sys.argv.(0)

let parser_pos_of_lexing_pos pos =
  let open Pos_ast in
  { 
    line = pos.pos_lnum ; 
    col = pos.pos_cnum ;
    bol = pos.pos_bol
  }


let create_stream lexbuf =
  let t = ref Lexer.EOF in
  let open Pos_ast in
  let last_p = ref { line = -1 ; col = -1 ; bol = -1 } in
  let next () =
    let curr_t = !t in
    last_p := parser_pos_of_lexing_pos lexbuf.lex_curr_p;
    (* decrement the column : lex_curr_p points to the character 
     * right after the last token produced *)
    last_p := { line = !last_p.line ; col = !last_p.col - 1 ; bol = !last_p.bol };
    begin try 
      t := Lexer.lex lexbuf
    (* for some weird and annoying reason,
     * I have to catch the exception here and
     * can't catch it at the same place as the other exceptions 
     * (syntax/typing exceptions, etc.) *)
    with Lexer.LexicalError msg -> 
      Format.eprintf "File \"%s\", line %d, characters %d-%d:\nlexical error: %s@."
        !file
        lexbuf.lex_start_p.pos_lnum 
        (* add one to have one-indexed columns *)
        (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol + 1) 
        (* don't add one since lex_curr_p points to the next blank character *)
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
        msg;
      exit 1
    end;
    curr_t
  in
  let peek () =
    !t
  in
  let next_pos () =
    (* points to the first character of !t *)
    parser_pos_of_lexing_pos lexbuf.lex_start_p
  in
  let last_pos () =
    !last_p
  in
  (* initialize t *)
  let _ = next () in
  let open Parser in 
  { next = next ; peek = peek ; next_pos = next_pos ; last_pos = last_pos }


let pp_pos fmt pos =
  let open Pos_ast in
  Format.fprintf fmt "%d:%d" pos.line pos.col

let compile () = 
  Arg.parse specs anon_param usage;
  if !file = "" then Arg.usage specs usage 
  else if !parse_only && !type_only 
  then Printf.eprintf "At most one of the options --parse-only and --type-only may be used" else
  let c = open_in !file in
  let lexbuf = Lexing.from_channel c in
  let s = create_stream lexbuf in
  try 
    (let prog = Parser.parse_prog s in
    if !parse_only then
    begin
      if !verbose then Pos_ast.print_prog Format.std_formatter prog;
      exit 0
    end;
    let prog = Type_checking.type_prog prog in
    if !type_only then
    begin
      if !verbose then Type_ast.print_prog Format.std_formatter prog;
      exit 0
    end;
    let prog = Codegen.compile_prog prog in
    if !verbose then X86_64.print_program Format.std_formatter prog;
    let asm_file = 
      (String.sub !file 0 (String.length !file - 3)) ^ ".s"
    in
    X86_64.print_in_file ~file:asm_file prog)
  with
    | Parser.SyntaxError msg ->
      Format.eprintf "File \"%s\", line %d, characters %d-%d:\nsyntax error: %s@." 
        !file 
        lexbuf.lex_start_p.pos_lnum 
        (* add one to have one-indexed columns *)
        (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol + 1) 
        (* don't add one since lex_curr_p points to the next blank character *)
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
        msg;
      exit 1
    | Type_checking.SemanticError (pos, msg) ->
      Format.eprintf "File \"%s\", line %d, characters %d-%d:\nerror: %s@."
        !file
        pos.first.line
        (* add one to have one-indexed columns *)
        (pos.first.col - pos.first.bol + 1)
        (pos.last.col - pos.first.bol + 1)
        msg;
      exit 1
  
let _ =
  (* TODO : this is only for debug *)
  compile ()
  (*try compile (); exit 0
  with _ -> exit 2*)
