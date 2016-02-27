open OUnit2
open ANSITerminal

let use_color = ref (not Sys.win32) (* Disable color on Windows *)
let run_parser_tests = ref true

let dummy _ = ()

let arglist = [
  ("--no-color", Arg.Clear use_color, "Disable colorized test output");
  ("--no-parser", Arg.Clear run_parser_tests, "Disable parser tests")
]

let _ = Arg.parse arglist dummy "usage: run_tests [--no-color] [--no-parser]"

let print_msg color (msg : string) =
  if !use_color then
    print_string [color] msg
  else
    Printf.printf "%s" msg

let print_std = print_msg cyan

let run_p_tests () =
  if !run_parser_tests then begin
    print_std "Running Parser Tests...\n";
    run_test_tt_main ParserTest.suite;
  end
;;

let () =
  print_std "Running Pretty-Printer Tests...\n";
    run_test_tt_main PPrintTest.suite;
  print_std "Running Lexer Tests...\n";
  run_test_tt_main LexerTest.suite;
  run_p_tests();
  print_std "Running AST Visitor Tests...\n";
  run_test_tt_main VisitorTest.suite;
  print_std "Running Desugaring Tests...\n";
  run_test_tt_main DesugarTest.suite;
