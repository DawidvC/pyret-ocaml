open OUnit2
open TestUtils

class unglobal = object(self)
  inherit Ast.default_map_visitor

  method s_global s = Ast.SName(Ast.dummy_loc, s)

  method s_atom (base,serial) = Ast.SName(Ast.dummy_loc, base)
end

let visitor = new unglobal

let d = Ast.dummy_loc

let ds prog = (visitor#visit_expr (Desugar.desugar_expr prog))

let p str = match (do_parse "test" (Lexing.from_string str)) with
  | Ast.SProgram(_,_,_,_,b) -> b

let id s = Ast.SId(d,Ast.SName(d,s))

let one = Ast.SNum(d, BatNum.Int 1)

let two = Ast.SNum(d, BatNum.Int 2)

let pretty prog = (PPrint.pretty_string (Ast.expr_tosource prog) 80)

let if_else = "if true: 5 else: 6 end"

let ask_otherwise = "ask: | true then: 5 | otherwise: 6 end"

let parse_and_ds str = ds (p str)

let test_ds str ast text_ctx =
  assert_equal ast (parse_and_ds str) ~printer:Ast.expr_to_string

let desugars_to str ast =
  let name = Printf.sprintf "`%s' desugars to `%s'" str (Ast.expr_to_string ast) in
  name>::test_ds str ast

let desugars_to_str str expect =
  let tst ctx =
    assert_equal expect (pretty (parse_and_ds str))
      ~printer:(fun x -> x) in
  let name = Printf.sprintf "`%s' desugars to `%s'" str expect in
  name>::tst

module A = Ast

let prog2 = "[list: 1,2,1 + 2]"
let prog3 = "[list: 1,2,1 + 2,1,2,2 + 1]"
let prog4 = "for map(elt from l): elt + 1 end"

let suite = "Desugaring Test Suite">:::[
    desugars_to prog2 (A.SBlock(d,[
        A.SApp(d,A.SDot(d,A.SId(d,A.SName(d,"list")), "make3"),
               [one;two;A.SApp(d,id "_plus", [one; two])])]));
    (* Change the `+' to a `_plus' call to check without desugaring the expected
       case, but doing so will cause the test to fail, since the parser doesn't know
       to make `_plus' a SGlobal...which matters because the visitor is broken. *)
    desugars_to prog4 (p "map(lam(elt): _plus(elt,1) end, l)")
  ]
