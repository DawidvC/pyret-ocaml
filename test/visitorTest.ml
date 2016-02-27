open OUnit2

module A = Ast

class collector_visitor = object(self)
  inherit A.default_map_visitor

  val mutable nums_seen = []
  method clear_nums () =
    nums_seen <- [];
    ()

  method get_nums_seen () : string list =
    nums_seen

  method s_num(l,n) =
    nums_seen <- (BatNum.to_string n)::nums_seen;
    A.SNum(l,n)
end

let lst_printer = function
  | [] -> "[]"
  | f::r ->
    "["^(List.fold_right (fun x acc -> x ^"; "^acc) r f)^"]"

let after_visit s l =
  let do_test ctx =
    let parsed = Utils.do_parse "visitor" (Lexing.from_string s) in
    let visitor = new collector_visitor in
    visitor#clear_nums();
    let _ = visitor#visit_program parsed in
    assert_equal l (visitor#get_nums_seen())
      ~printer:lst_printer
      ~cmp:(fun x y -> (List.sort (String.compare) x) = (List.sort (String.compare) y)) in
  let name = Printf.sprintf "`%s' makes %s when visited" s (lst_printer l) in
  name>::do_test

let suite = "Visitor Suite">:::[
    after_visit "2 + 2" ["2";"2"];
  ]
