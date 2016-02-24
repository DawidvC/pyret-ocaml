open OUnit2
open Printf
open PPrint

let test_words = List.map str ["This";"is";"a";"sentence";"with";"eight";"words"]
let test_words_flow = flow test_words

let opt_break x y =
  match (x,y) with
  | (MtDoc(_,_),_) -> y
  | (_,MtDoc(_,_)) -> x
  | (_,_) -> x +^ ((sbreak 1) +^ y)

let binop left op right =
  group (nest 2 (opt_break (group (opt_break (str left) (str op))) (str right)))

let ifthen c t f =
  group
    (opt_break
       (group (nest 2 (opt_break (str "if") c)))
       (opt_break 
          (group (nest 2 (opt_break (str "then") t)))
          (group (nest 2 (opt_break (str "else") f)))))

let ifthenelse = ifthen (binop "a" "==" "b") (binop "a" "<<" "2") (binop "a" "+" "b")

let list_printer l = match l with
  | [] -> "[]"
  | fst::rest ->
    let rec print_rest r = match r with
      | [] -> ""
      | f::rst -> sprintf ";\"%s\"%s" f (print_rest rst) in
    sprintf "[\"%s\"%s]" fst (print_rest rest)

let test_pretty d n e ctx =
  assert_equal e (pretty d n) ~printer:list_printer

let test_flow n expect = let name = "flow " ^ (string_of_int n) in
  name>::test_pretty test_words_flow n expect

let test_ite n expect = let name = "ite " ^ (string_of_int n) in
  name>::test_pretty ifthenelse n expect

let test_eq name a b =
  name>::(fun ctx -> assert_equal a b ~printer:dbg_printer)

let suite =
  "pprint_suite">:::
  [
    test_flow 40 ["This is a sentence with eight words"];

    test_ite 32 ["if a == b then a << 2 else a + b"];
    test_ite 15 ["if a == b";"then a << 2";"else a + b"];
    test_ite 10 ["if a == b";"then";"  a << 2";"else a + b"];
    test_ite 8 ["if";"  a == b";"then";"  a << 2";"else";"  a + b"];
    test_ite 7 ["if";"  a ==";"    b";"then";"  a <<";"    2";"else";"  a + b"];
    test_ite 6 ["if";"  a ==";"    b";"then";"  a <<";"    2";"else";"  a +";"    b"];

    "binop">::test_pretty (binop "a" "==" "b") 20 ["a == b"];

    "direct concat">::test_pretty (Concat ((str "a == "), (str "b"),5,false)) 20 ["a == b"];
    "no_nest">::test_pretty (concat (str "a == ") (str "b")) 20
      ["a == b"];

    test_eq "with sbreak"
      ((sbreak 1) +^ (str "foo"))
      (Concat(IfFlat(Blank(1,1,false),Hardline(0,true),1,false),(str "foo"),4,false))
  ]
;;

