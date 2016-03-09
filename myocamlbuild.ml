open Ocamlbuild_plugin

(** Allows for resolution of package locations *)
let ocamlfind_query pkg =
  let cmd = Printf.sprintf "ocamlfind query %s" (Filename.quote pkg) in
  Ocamlbuild_pack.My_unix.run_and_open cmd (fun ic -> input_line ic)

let () =
  dispatch begin function
    | After_rules ->
      rule "dypgen"
        ~prods:["%.ml"]
        ~deps:["%.dyp"]
        begin fun env _ ->
          let dyp = env "%" in
          let dypfile = dyp ^ ".dyp"  in
          let output_file = dyp^"_temp.ml" in
          let extract_type = dyp^".extract_type" in
          let useocf = "--command \"ocamlfind ocamlc -package dyp "^output_file^
                       " > "^extract_type^"\"" in
          Cmd(S[A"dypgen"; A "--no-mli";A"--pv-obj"; Sh useocf; Px dypfile])
        end;
    | _ -> ()
  end;;
