include OUnit2

type printer = string -> unit

module type TESTS = sig
  val suite_name : string option
  val suite : OUnit2.test
end

module type SUITE = sig
  val run_tests : printer -> unit
end

module MakeSuite (Defs : TESTS) : SUITE = struct
  let run_tests printer =
    (match Defs.suite_name with
     | None -> ()
     | Some(name) ->
       name
       |> Printf.sprintf "Running %s Tests...\n"
       |> printer);
    run_test_tt_main Defs.suite
end
