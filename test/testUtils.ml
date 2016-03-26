open Lexer
open Parser
open Lexing
open Printf
open OUnit2
open ExtLib

let tok_list_printer = tok_list_to_string

let old_ast_printer ast =
  sprintf "%s" (List.fold_right (fun x acc -> x ^ "\n" ^ acc)
                  (PPrint.pretty (Ast.tosource ast) 40) "")

let ast_printer = Ast.prog_to_string

let list_printer elt_p lst =
  "[" ^ (PyretUtils.join_str (List.map elt_p lst) "; ") ^ "]"

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let either_printer vprinter e =
  match e with
  | Left(v) -> sprintf "Error: %s\n" v
  | Right(v) -> vprinter v

let either_ast_printer = either_printer ast_printer

let mappend (f : ('a -> 'b list)) (l : 'a list) : ('b list) =
  let ffun elt acc = (f elt) @ acc in
  List.fold_right ffun l []

let is_eof tok = match tok with
  | EOF -> true
  | _ -> false

let lex_string s =
  Lexer.reset();
  let lexbuf = Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = "test_file" };
  let lst = ref [] in
  let eof_found = ref false in
  while (not !eof_found) do
    let nexttok = (Lexer.token lexbuf) in
    lst := nexttok::!lst;
    if (is_eof nexttok) then
      eof_found := true
  done;
  List.rev !lst

let assert_lex s expected ctx =
  try
    assert_equal expected (lex_string s) ~printer:tok_list_printer
  with
  | Invalid_argument(_) -> () (* Allow abstract arguments (i.e. number tokens) to fail silently *)
let test_lex name s expected =
  name>::assert_lex s expected

let string_of_position p =
  sprintf "%s:line %d, col %d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol);;

let do_parse name lexbuf : Ast.program  =
  try
    let oldval = !Ast.use_dummy in
    Lexer.reset();
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
    Ast.use_dummy := true;
    let res = Parser.program Lexer.token lexbuf in
    Ast.use_dummy := oldval;
    match res with
    | (prog,_)::[] -> prog
    | [] -> failwith "empty parse"
    | hd::tail -> let pair_tostring (ast,str) =
                    sprintf "(%s,%s)" (ast_printer ast) str in
      let mapped = List.map pair_tostring tail in
      let folded = List.fold_left (fun acc x -> x ^ "; " ^ acc) (pair_tostring hd) mapped in
      failwith (sprintf "bad parse: [%s]" folded)
  with
  | Failure "lexing: empty token" ->
    failwith (sprintf "lexical error at %s"
                (string_of_position lexbuf.lex_curr_p))

let do_parse_str name s = do_parse name @@ Lexing.from_string s

let parse name lexbuf =
  try
    Right(do_parse name lexbuf)
  with
  | Failure msg -> Left(msg)

let parse_string name s =
  let lexbuf = Lexing.from_string s in
  parse name lexbuf

let test_parse_body ?(preproc=(fun x -> x)) name str exp test_ctxt =
  match do_parse_str name str with
  | Ast.SProgram(_, _, _, _, Ast.SBlock(_, res)) ->
    assert_equal exp (preproc res) ~printer:(list_printer Ast.expr_to_string)
  | Ast.SProgram(_, _, _, _, _) ->
    failwith "Test error: Non-SBlock body in parse"

let test_parse_body_block ?(preproc=(fun x -> x)) name str exp test_ctxt =
  match do_parse_str name str with
  | Ast.SProgram(_, _, _, _, block) ->
    assert_equal exp (preproc block) ~printer:Ast.expr_to_string

let test_parse ?(preproc=(fun x -> x)) name str exp test_ctxt =
  assert_equal exp (preproc (do_parse_str name str))
    ~printer:ast_printer

let test_parse_err name str errmsg test_ctxt =
  assert_equal
    (Left(errmsg))
    (parse_string name str)
    ~printer:either_ast_printer
    ~cmp: (fun check result ->
        match check, result with
        | Left(expect_msg), Left(actual_message) ->
          String.exists actual_message expect_msg
        | _ -> false)

let test_parse_fails name str =
  let do_test test_ctxt =
    try
      match (parse_string name str) with
      | Left(m) -> failwith m (* Not a Syntax Error *)
      | Right(v) -> failwith (Printf.sprintf "Got valid parse:\n%s" (Ast.prog_to_string v))
    with
    | Dyp.Syntax_error -> () in
  let testname = (Printf.sprintf "`%s' should fail to parse" str) in
  testname>::do_test

let test_parses name str =
  let do_test test_ctxt =
    match (parse_string name str) with
    | Left(msg) -> failwith msg
    | Right(_) -> () in
  let testname = (Printf.sprintf "`%s' should parse" str) in
  testname>::do_test
