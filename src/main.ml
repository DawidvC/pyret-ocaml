let string_ref = ref ""
let process_argument s =
  if s = "" then raise (Arg.Bad "missing input file name")
  else string_ref := s
let _ = Arg.parse [] process_argument "usage: demo file_name"
let _ = if !string_ref = "" then
  (print_string "usage: demo file_name\n";
   exit 0)

let pprinter (v : Ast.program) = let pprinted = PPrint.pretty (Ast.tosource v) 40 in
  List.fold_right (fun f acc -> f ^ "\n" ^ acc) pprinted ""

let input_file = !string_ref
let lexbuf = Lexing.from_channel (Pervasives.open_in input_file)
let prog = fst (List.hd (Parser.program Lexer.token lexbuf))
let () =
  Printf.printf "%s" (pprinter prog)
