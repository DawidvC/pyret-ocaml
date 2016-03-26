(** Tool which just prints the AST for a given input file *)
open PyretUtils
let string_ref = ref ""
let process_argument s =
  if s = "" then raise (Arg.Bad "Missing input file name")
  else string_ref := s

let _ = Arg.parse [] process_argument "Usage: ./show-parse file_name"
let _ = if !string_ref = "" then
    (print_string "Usage: ./show-parse file_name\n"; exit 0)

let print_ast = Sexplib.Sexp.to_string_hum ||> Ast.sexp_of_program

let input_file = !string_ref
let () =
  let lexbuf = Lexing.from_channel @@ Pervasives.open_in input_file in
  let prog = fst @@ List.hd @@ Parser.program Lexer.token lexbuf in
  Printf.printf "%s\n" @@ print_ast prog
