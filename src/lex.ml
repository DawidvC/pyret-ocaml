let string_ref = ref ""
let process_argument s =
  if s = "" then raise (Arg.Bad "missing input file name")
  else string_ref := s
let _ = Arg.parse [] process_argument "usage: demo file_name"
let _ = if !string_ref = "" then
  (print_string "usage: demo file_name\n";
   exit 0)

let lex_all lb =
  let lst = ref []
  and eof_found = ref false in
  while (not !eof_found) do
    let nexttok = (Lexer.token lb) in
    lst := nexttok::!lst;
    if (nexttok = Parser.EOF) then
      eof_found := true
  done;
  List.rev !lst

let input_file = !string_ref
let lexbuf = Lexing.from_channel (Pervasives.open_in input_file)
let prog = lex_all lexbuf
let () =
  Printf.printf "%s\n" (Lexer.tok_list_to_string prog)
