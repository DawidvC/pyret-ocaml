module CompilationPhase = struct
  type t =
      Start
    | Phase of string * (unit -> string) * t

  let to_thunk_list phase =
    let rec help acc = function
      | Start -> acc
      | Phase(name, result, prev) ->
        help ((name, result) :: acc) prev in
    help [] phase

  let to_list phase =
    (* The list shouldn't bee *too* terribly long, so
       traversing twice isn't the end of the world. *)
    List.map (fun (name, thunk) -> (name, thunk())) @@ to_thunk_list phase
end

let compile_js_ast phases ast name env libs options =
  let open CompileStructs in
  let open CompileResult in
  let open CompilationPhase in
  let sstr : 'a. ('a -> Sexplib.Sexp.t) -> 'a -> string = fun fmt value ->
    Sexplib.Sexp.to_string_hum (fmt value) in
  let ret = ref phases in
  let set_phase str fmt v =
    if options.collect_all then
      ret := Phase(str, (fun () -> fmt v), !ret) in
  let ast_ended = AstUtils.append_nothing_if_necessary ast in
  (match ast_ended with
   | None -> ()
   | Some(value) -> set_phase "Added nothing" (sstr Ast.sexp_of_program) value);
  let wf = WellFormed.check_well_formed
    @@ (match ast_ended with
        | Some(v) -> v
        | None -> ast) in
  (*set_phase "Checked well-formedness" (sstr @@ CompileResult.sexp_of_t Ast.sexp_of_program) wf;*)
  let checker =
    if options.check_mode then
      DesugarCheck.desugar_check else DesugarCheck.desugar_no_checks in
  match wf with
  | Ok(wf_code) ->
    begin
      let wf_ast = wf_code in
      let checked = checker wf_ast in
      let imported = AstUtils.wrap_extra_imports checked libs in
      let scoped = ResolveScope.desugar_scope imported env in
      let named_result = ResolveScope.resolve_names scoped env in
      match named_result with
      | NameResolution.Resolved(named_ast, named_errors, binds, type_binds, datatypes) ->
        let desugared = Desugar.desugar named_ast in
        let type_checked =
          if options.type_check then
            failwith "Type-checking is not yet implemented"
          else Ok(desugared) in
        match type_checked with
        | Ok(tc_ast) ->
          let any_errors = named_errors
                           @ (AstUtils.check_unbound env tc_ast)
                           @ (AstUtils.bad_assignments env tc_ast) in
          let dp_ast = DesugarPostTC.desugar_post_tc tc_ast env in
          let cleaned =
            dp_ast
            |> (new AstUtils.merge_nested_blocks)#visit_program
            |> (new AstUtils.flatten_single_blocks)#visit_program
            |> (new AstUtils.link_list_visitor env)#visit_program
            |> (new AstUtils.letrec_visitor)#visit_program in
          let inlined = (new AstUtils.inline_lams)#visit_program cleaned in
          (match any_errors with
           | [] -> Ok(Codegen.Js.OfPyret.make_compiled_pyret inlined env options)
           | _ -> Err(any_errors))
        | Err(_) -> failwith "Impossible" (* Type-checking can't fail if it doesn't happen *)
    end
  | Err(_) -> failwith "Error"

let compile_js trace code name env libs options =
  let open CompilationPhase in
  let ret = ref trace in
  (* FIXME: The lexing/parsing bit should be abstracted out elsewhere *)
  let lexbuf = Lexing.from_string code in
  let ast = fst @@ List.hd @@ Parser.program Lexer.token lexbuf in
  (if options.CompileStructs.collect_all then
    ret := Phase("Parsed", (fun () -> Sexplib.Sexp.to_string_hum @@ Ast.sexp_of_program ast), !ret));
  compile_js_ast !ret ast name env libs options
