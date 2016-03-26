open CompileStructs
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
let () =
  let compiled = TstCompile.compile_js_file
      Compile.CompilationPhase.Start input_file "testing"
      standard_builtins
      standard_imports
      default_compile_options in
  match compiled with
  | CompileResult.Ok(ccp) ->
    Codegen.Js.OfPyret.pyret_to_js_pretty 80 ccp
    |> Printf.printf "%s"
  | CompileResult.Err(errs) ->
    ValueSkeleton.of_list CompileError.to_vs errs ()
    |> ValueSkeleton.render
    |> Printf.printf "Errors:\n%s"
