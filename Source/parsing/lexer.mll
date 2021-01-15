{
  open Lexing
  
  exception LexicalError of string
  let error msg = 
    raise (LexicalError msg) 

  
  type token =
    | LP | RP | COLON | SEMICOLON | DOUBLECOLON | COMMA | EQUAL
    | ADD | SUB | MUL | MOD | POW | AND | OR | NOT | DOT
    | EQ | NEQ | LT | LEQ | GT | GEQ 
    | TRUE | FALSE | IF | ELSE | ELSEIF | END | WHILE | FOR 
    | FUNCTION | RETURN | STRUCT | MUTABLE
    | STRING of string
    | IDENT of string
    | INT of int64
    | INT_IDENT of int64 * string
    | INT_LP of int64
    | IDENT_LP of string
    | RP_IDENT of string
    | EOF

  let keywords = Hashtbl.create 16
  let () = List.iter
    (fun (str, tok) -> Hashtbl.add keywords str tok)
    [("true", TRUE);
    ("false", FALSE);
    ("if", IF);
    ("else", ELSE);
    ("elseif", ELSEIF);
    ("end", END);
    ("while", WHILE);
    ("for", FOR);
    ("function", FUNCTION);
    ("return", RETURN);
    ("struct", STRUCT);
    ("mutable", MUTABLE)]

  let prev_tok = ref None
  let inserted_semicolon = ref false

  (* assumes we stumbled upon a newline *)
  let should_insert_semicolon () = match !prev_tok with
    | None -> false
    | Some tok -> 
      begin match tok with
        | IDENT _ | INT _ | INT_IDENT _ | RP_IDENT _
        | STRING _ | TRUE | FALSE | RETURN | RP | END -> true
        | _ -> false
      end
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let num = digit+
let ident = alpha(alpha|digit)*

(* entry point for lexing is function lex
 * this way we can maintain the previous token recognized *)
rule token = parse
  | [' ' '\t'] { token lexbuf }
  | '\n' {
    if should_insert_semicolon () then begin
      (* postpone the new_line call so that the semicolon
       * counts as being on the previous line not the next line *)
      inserted_semicolon := true;
      SEMICOLON
    end
    else begin
      new_line lexbuf; 
      token lexbuf
    end
  }
  | '#' { comment lexbuf }
  
  | '"' { 
    let start_p = lexbuf.lex_start_p in
    let s = str [] lexbuf in 
    (* update the start_p so that the position of the string
     * is well calculated *)
    lexbuf.lex_start_p <- start_p;
    STRING s 
  }

  (* special case for -2^63 *)
  | "-9223372036854775808" as n { INT (Int64.of_string n) }
  | num as n { 
    try INT (Int64.of_string n)
    with _ -> error "invalid integer constant"
  }
  | ident as i {
    (* keyword *)
    try Hashtbl.find keywords i
    (* variable *)
    with Not_found -> IDENT i
  }
  | (num as n) (ident as i) {
    if Hashtbl.mem keywords i 
    then error "can't concat int and reserved keyword"
    else INT_IDENT ((Int64.of_string n), i)
  }
  | (ident as i) '(' { 
    if Hashtbl.mem keywords i
    then error "can't concat reserved keyword and left parenthesis"
    else IDENT_LP i
  }
  | ')' (ident as i) {
    if Hashtbl.mem keywords i
    then error "can't concat right parenthesis and reserved keyword"
    else RP_IDENT i
  }
  | (num as n) '(' { INT_LP (Int64.of_string n) }
  
  | '(' { LP }
  | ')' { RP }
  | ':' { COLON }
  | "::" { DOUBLECOLON }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | '.' { DOT }
  | '=' { EQUAL }
  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | '%' { MOD }
  | '^' { POW }
  | '!' { NOT }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<"  { LT }
  | "<=" { LEQ }
  | ">"  { GT }
  | ">=" { GEQ }
  | "&&" { AND }
  | "||" { OR }
  
  | _ as c { error ("Unexpected character " ^ (String.make 1 c)) }
  | eof { EOF }

and comment = parse
  | '\n' {
    (* don't forget to insert semicolons here too ! *)
    if should_insert_semicolon () then begin
      inserted_semicolon := true;
      SEMICOLON
    end
    else begin
      new_line lexbuf;
      token lexbuf
    end
  }
  | eof { error "eof in comment" }
  | _ { comment lexbuf }

and str acc = parse
  | '"' { 
    let chars = List.map (String.make 1) (List.rev acc) in 
    String.concat "" chars
  }
  | eof { error "eof in string" }
  | "\\\\" { str ('\\' :: acc) lexbuf }
  | "\\n" { str ('\n' :: acc) lexbuf }
  | "\\\"" { str ('"' :: acc) lexbuf }
  | "\\t" { str ('\t' :: acc) lexbuf }
  | "\\" { error "Backslash '\\' in string must be part of a valid espace sequence" } 
  | ['\032'-'\126'] as c { str (c :: acc) lexbuf }
  | _ as c { error "Unexpected character in string : " ^ String.make 1 c }

{ 
  let lex lexbuf =
    if !inserted_semicolon then
    begin
      new_line lexbuf;
      inserted_semicolon := false
    end;
    let t = token lexbuf in
    match t with
      | IF when !prev_tok = (Some ELSE) -> error "else and if keywords should be concatenated in elseif"
      | _ -> prev_tok := Some t; t
}
