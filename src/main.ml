open ANSITerminal
open CompileStructs
let string_ref = ref ""
let process_argument s =
  if s = "" then raise (Arg.Bad "missing input file name")
  else string_ref := s
let _ = Arg.parse [] process_argument "usage: demo file_name"
let _ = if !string_ref = "" then
  (Pervasives.print_string "usage: demo file_name\n";
   exit 0)

let pprinter (v : Ast.program) = let pprinted = PPrint.pretty (Ast.tosource v) 40 in
  List.fold_right (fun f acc -> f ^ "\n" ^ acc) pprinted ""

let use_color = (not Sys.win32)
let print_msg color msg =
  if use_color then
    print_string [color] msg
  else
    Printf.printf "%s" msg

let print_error = print_msg red

let input_file = !string_ref
let () =
  let compiled = Compile.compile_js_file
      Compile.CompilationPhase.Start input_file "testing"
      standard_builtins
      standard_imports
      default_compile_options in
  match compiled with
  | CompileResult.Ok(ccp) ->
    Codegen.Js.OfPyret.pyret_js_runnable (Printf.printf "%s") ccp
  | CompileResult.Err(errs) ->
    let ed_to_string ed = (ErrorDisplay.display_to_string ed []) ^ "\n" in
    List.map CompileError.render_reason errs
    |> List.map ed_to_string
    |> List.iter print_error
