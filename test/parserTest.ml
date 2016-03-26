open TestUtils
open Ast
open OUnit2

let mkprogram body =
  SProgram (dummy_loc, (SProvideNone(dummy_loc)),
            (SProvideTypesNone(dummy_loc)), [], SBlock(dummy_loc,body))

let test_empty = test_parses "empty" ""

let test_num =
  "3 parses correctly">::(test_parse "num" "3" (mkprogram([SNum(dummy_loc, BatNum.of_string "3")])))

let test_ident =
  "foo parses correctly">::(test_parse "foo" "foo"
                              (mkprogram([SId(dummy_loc, SName(dummy_loc, "foo"))])))

let test_lets_letrecs =
  let should_fail = test_parse_fails "let"
  and should_pass = test_parses "let" in
  "should parse lets and letrecs">:::[
    should_fail "let: 10 end";
    should_fail "letrec: 10 end";
    should_pass "let x = 10, y = 12: x + y end";
    should_pass "let x = 10, y = 12, z = 13: BAMBOOZLE end";
    should_pass "letrec x = 10, y = 12: x + y end";
    should_pass "letrec z = 62, x = 10, y = 12: x + y end"
  ]

let test_type_lets =
  let should_fail = test_parse_fails "type-let"
  and should_pass = test_parses "type-let" in
  "should parse type-lets">:::[
    should_pass "type-let t1 = Number, t2 = String: 5 end";
    should_pass "type-let t1 = Number: 10 end";
    should_fail "type-let: 10 end";
    should_pass "type-let newtype List as ListT: {} end";
    should_pass "type-let newtype List as ListT, thing = foo: {} end"
  ]

let test_standalone_types =
  let should_fail = test_parse_fails "standalone"
  and should_pass = test_parses "standalone" in
  "should parse standalone types">:::[
    should_pass "type foo = { x :: Number }";
    should_fail "type foo = Number -> String"
  ]

let test_standalone_newtypes =
  let should_fail = test_parse_fails "standalone-new"
  and should_pass = test_parses "standalone-new" in
  "should parse standalone newtypes">:::[
    should_pass "newtype Foo as FooT";
    should_fail "newtype (Number -> String)"
  ]

let test_provide_types =
  let should_pass = test_parses "provide-types" in
  "should parse provide-types">:::[
    should_pass "provide-types { List :: List }";
    should_pass "provide-types { List :: List, x :: (Number -> String) }"
  ]

let test_provide_types_exprs =
  let should_fail = test_parse_fails "provide-types-exprs" in
  "shouldn't parse expressions in provide-types">:::[
    should_fail "provide-types { List :: 5 + 5 }";
    should_fail "provide-types { List :: List, x :: lam(x): x end }"
  ]

let test_english_op_ident =
  let should_fail = test_parse_fails "english-op-ident" in
  let wss = [" ";" \n"; "\n "; " \n"; " \n "]
  and en_ops = ["or";"and";"is";"satisfies";"raises"] in
  let do_test ctx =
    let test_with_ws op ws =
      let _ = [
        should_fail ("("^op^ws^op^ws^op^")");
        should_fail (op^ws^"="^"false");
        should_fail (op^"="^ws^"false");
        should_fail (op^ws^"="^ws^"false");
      ] in
      () in
    let test_with_op op =
      let _ = [should_fail (op^"="^"false")] in
      List.iter (test_with_ws op) wss in
    List.iter test_with_op en_ops in
  "shouldn't allow English ops as identifiers, no matter the whitespace">::do_test

let test_hyphen_ident =
  let should_fail = test_parse_fails "hyphen-ident"
  and should_pass = test_parses "hyphen-ident" in
  "shouldn't allow hyphens at the beginning or end of identifiers">:::[
    should_fail "-";
    should_fail "(- -)";
    should_fail "--";
    should_fail "(-- --)";
    should_fail "a- b";
    should_fail "a -b";
    should_fail "a- = b";
    should_fail "-a = b";
    should_pass "a-a";
    should_pass "a-a-a";
    should_pass "a--aa";
    should_pass "aa--a"
  ]

let test_english_ops =
  let should_fail = test_parse_fails "english-ops"
  and should_pass = test_parses "english-ops" in
  let wss = [" ";" \n";"\n ";" \n";" \n "]
  and en_ops = ["or";"and";"is";"satisfies";"raises"] in
  let do_test ctx =
    let test_with_ws op ws =
      let _ = [
        should_fail ("(false)"^ws^op);
        should_fail (op^ws^"(false)");
        should_pass ("(false)"^ws^op^"(false)");
        should_pass ("(false)"^op^ws^"(false)");
        should_pass ("(false)"^ws^op^ws^"(false)");
      ] in
      (); in
    let test_with_op op =
      let _ = [
        should_fail ("(false)"^op);
        should_fail (op^"(false)");
        should_pass ("(false)"^op^"(false)");
      ] in
      List.iter (test_with_ws op) wss in
    List.iter test_with_op en_ops in
  "should allow English ops with all manner of surrounding whitespace and parens">::do_test

let test_parse_errors =
  let should_fail = test_parse_fails "parse-errors" in
  "should notice parse errors">:::[
    should_fail "bad end";
    should_fail "provide-types { List :: List } end"
  ]

let test_angle_brackets_type_inst =
  let should_fail = test_parse_fails "angle-brackets-type-inst"
  and should_pass = test_parses "angle-brackets-type-inst" in
  "should parse angle brackets without whitespace only as type instantiations">:::[
    should_pass "map<A>";
    should_pass "(map<A>)";
    should_pass "(map<A, B>)";
    should_pass "map<A, B>(id)";
    should_fail "(map < A, B > (id))";
    should_fail "(map\n<\nA, B\n>\n(id))";
    should_pass "map<A,\nB>(id)"
  ]

let test_angle_brackets_func =
  let should_fail = test_parse_fails "angle-brackets-func"
  and should_pass = test_parses "angle-brackets-func" in
  "should parse angle brackets without whitespace in annotations only as type function application">:::[
    should_fail "a :: List < A > = a";
    should_fail "a :: List < A, B > = a";
    should_pass "a :: List<A> = a";
    should_pass "a :: List<A, B> = a"
  ]

let test_angle_gt_lt =
  let should_fail = test_parse_fails "angle-gt-lt"
  and should_pass = test_parses "angle-gt-lt" in
  "should parse angle brackets with whitespace as gt/lt">:::[
    should_pass "1\n<\n2 or false\n B > (id)";
    should_fail "1 <\n2 or false, B > (id)"
  ]

let test_angle_whitespace =
  let should_pass = test_parses "angle-whitespace" in
  "should not care about whitespace and angle brackets in declarations">:::[
    should_pass "fun print<A>(): end";
    should_pass "fun print< A>(): end";
    should_pass "fun print <A>(): end";
    should_pass "fun print<A >(): end";
    should_pass "fun print< A >(): end";
    should_pass "fun print <A >(): end";
  ]

let test_paren_op =
  let should_pass = test_parses "paren-op" in
  "should not treat (...) after operators as application">:::[
    should_pass "(true) or (false)";
    should_pass "(true) < (false)";
    should_pass "(true) > (false)"
  ]

let test_semi_eof =
  let should_pass = test_parses "semi-eof" in
  "should not mind ; at EOF">:::[
    should_pass "lam<T>(x :: T) -> T: x;"
  ]

let test_semi_eol =
  let should_pass = test_parses "semi-eol" in
  let a = "  fun x<T>(x :: T) -> T: x;" in
  "should not mind ; at EOL, and then another statement">:::[
    should_pass ("block:\n"^a^"\n"^a^"end");
    should_pass ("block:\n"^a^" \n"^a^"end")
  ]

let test_ws_coloncolon_thickarrow =
  let should_fail = test_parse_fails "ws-coloncolon-thickarrow"
  and should_pass = test_parses "ws-coloncolon-thickarrow" in
  "should require whitespace after :: and =>">:::[
    should_fail "cases (T) x: | Foo() =>(true) end";
    should_pass "cases (T) x: | Foo() => (true) end";
    should_pass "cases (T) x: | Foo() =>\n(true) end";
    should_fail "block: dog ::Cat = really-huh end";
    should_pass "block: dog :: Cat = really-huh end";
    should_pass "block: dog :: Cat =\nreally-huh end"
  ]

let test_paren_group_comma =
  let should_pass = test_parses "paren-grp-comma" in
  "should treat (...) as grouping after ,">:::[
    should_pass "[list: x,(x)]";
    should_pass "[list: x , (x)]";
    should_pass "[list: x ,\n(x)]"
  ]

let test_paren_group_colon =
  let should_pass = test_parses "paren-grp-colon" in
  "should treat (...) as grouping after :">:::[
    should_pass "{ asdf:(asdf) }";
    should_pass "{ asdf : (asdf) }";
    should_pass "{ asdf :\n(asdf) }";
    should_pass "fun f(x):\nx\nwhere(f(5)) is 5\nend";
    should_pass "check:(5) is 5 end";
    should_pass "examples:(5) is 5 end";
    should_pass "ask:\n  | false then: 1\n  | otherwise:(true)\nend";
    should_pass "ask:\n  | true then:(1)\nend";
    should_pass "if true: 1 else:(1) end";
    should_pass "block:(5) end";
    should_pass "ask:\n  |(true) then: 1\nend";
  ]

let test_paren_group_eq =
  let should_pass = test_parses "paren-grp-eq" in
  "should treat (...) as grouping after =">:::[
    should_pass "block: x=(x) end";
    should_pass "block: x = (x) end";
    should_pass "block: x =\n(x) end";
  ]

let test_paren_group_coloneq =
  let should_pass = test_parses "paren-grp-coloneq" in
  "should treat (...) as grouping after :=">:::[
    should_pass "block: x:=(x) end";
    should_pass "block: x := (x) end";
    should_pass "block: x :=\n(x) end";
  ]

let test_paren_group_semi =
  let should_pass = test_parses "paren-grp-semi" in
  "should treat (...) as grouping after ;">:::[
    should_pass "block: lam(x): x;(x);";
    should_pass "block: lam(x): x ; (x);";
    should_pass "block: lam(x): x ;\n(x);";
  ]

let test_get_bang =
  let should_pass = test_parses "get-bang" in
  "should parse get-bang">:::[
    should_pass "o!x";
    should_pass "y.x!x";
  ]

let test_update =
  let should_pass = test_parses "update" in
  "should parse update">:::[
    should_pass "o!{x:5}";
    should_pass "y!{x:5, y:10}";
  ]

let test_ref_data_defs =
  let should_pass = test_parses "ref-data-defs" in
  "should parse ref fields in data definitions">:::[
    should_pass "data D: d(ref x) end";
    should_pass "data D: d(ref x :: Number % (is-odd)) end";
    should_pass "data D: d(ref x, ref y :: Number) end";
    should_pass "data D: | d(ref x :: Boolean, ref y) end";
  ]

let test_ref_obj_lits =
  let should_fail = test_parse_fails "ref-obj-lits"
  and should_pass = test_parses "ref-obj-lits" in
  "should parse ref fields in object literals">:::[
    should_pass "{ref x :: Number: 22}";
    should_pass "{ref x: 22}";
    should_pass "{ref x: 22, y: \"a\"}";
    should_pass "{ref x: 22, ref y: \"a\"}";
    should_pass "{ref x: 22, ref y :: String: \"a\"}";
    should_pass "{ref x :: { z :: Number}: 22, ref y :: String: \"a\"}";
    should_fail "{x :: Number: 5}";
    should_fail "{ ref ref y :: String: 5 }";
    should_fail "{ ref ref: 5 }";
  ]

let test_imports =
  let should_fail = test_parse_fails "imports"
  and should_pass = test_parses "imports" in
  "should parse imports">:::[
    should_pass "import modname as G";
    should_pass "import \"modname.arr\" as G";
    should_fail "import gdrive(a) as G";
    should_pass "import gdrive(\"a\") as G";
    should_pass "import gdrive(\"a\", \"b\") as G";
    should_fail "import gdrive() as G";
  ]

let test_includes =
  let should_fail = test_parse_fails "includes"
  and should_pass = test_parses "includes" in
  "should parse includes">:::[
    should_pass "include modname";
    should_pass "include \"modname.arr\"";
    should_fail "include gdrive(a)";
    should_pass "include gdrive(\"a\")";
    should_pass "include gdrive(\"a\", \"b\")";
    should_fail "include gdrive()";
  ]

let test_new_eqops =
  let should_fail = test_parse_fails "new-eqops"
  and should_pass = test_parses "new-eqops" in
  "should parse new equality operators">:::[
    should_pass "o <=> o2";
    should_fail "o <= > o2";
    should_fail "o < = > o2";
    should_fail "o < => o2";
    should_pass "o =~ o2";
    should_pass "o == o2";

    should_pass "check: o is== o2;";
    should_fail "check: o is == o2;";
    should_pass "check: o is=~ o2;";
    should_fail "check: o is =~ o2;";
    should_pass "check: o is<=> o2;";
    should_fail "check: o is <=> o2;";

    should_pass "check: o is-not== o2;";
    should_fail "check: o is-not == o2;";
    should_pass "check: o is-not=~ o2;";
    should_fail "check: o is-not =~ o2;";
    should_pass "check: o is-not<=> o2;";
    should_fail "check: o is-not <=> o2;";
  ]

let test_examples =
  let should_pass = test_parses "examples" in
  "should parse examples">:::[
    should_pass "examples: 5 is 5 end";
  ]

let test_ref_cases =
  let should_fail = test_parse_fails "ref-cases"
  and should_pass = test_parses "ref-cases" in
  "should parse ref cases bindings">:::[
    should_pass "cases(List) l: | link(ref first, rest) => 5 end";
    should_pass "cases(List) l: | link(ref first, ref rest) => 5 end";
    should_pass "cases(List) l: | link(first, ref rest) => 5 end";
    should_pass "cases(List) l: | link(ref first :: Number, rest) => 5 end";
    should_pass "cases(List) l: | link(ref first :: Number, rest :: Number) => 5 end";
    should_pass "cases(List) l: | link(first :: Number, ref rest :: Number) => 5 end";
    should_pass "cases(List) l: | link(ref first :: Number, ref rest :: Number) => 5 end";

    should_fail "cases(List) l: link(ref ref) => 5 end";
  ]

let test_type_params_methods =
  let should_pass = test_parses "type-params-methods" in
  "should parse type parameters on methods">:::[
    should_pass "method<a>(self): self end";
    should_pass "method<a,b>(self): self end";
    should_pass "method<a,b,c>(self): self end";
    should_pass "{ m<a>(self): self end }";
    should_pass "{ m<a,b>(self): self end }";
    should_pass "{ m<a,b,c>(self): self end }";
    should_pass "data D: | var1 with: m<a>(self): 5 end sharing: m2<a>(self): 5 end end";
    should_pass "data D: | var1 with: m<a,b>(self): 5 end sharing: m2<a,b>(self): 5 end end";
    should_pass "data D: | var1 with: m<a,b,c>(self): 5 end sharing: m2<a,b,c>(self): 5 end end";
  ]

let test_rec_stmts =
  let should_fail = test_parse_fails "rec-stmts"
  and should_pass = test_parses "rec-stmts" in
  "should parse rec statements">:::[
    should_pass "rec a = 10";
    should_pass "rec ohn = lz(1, lam(): ohn end)";
    should_fail "rec = 5";
  ]

let test_brack_exprs =
  let should_fail = test_parse_fails "brack-exprs" in
  "shouldn't parse bracket exprs">:::[
    should_fail "o.[x]";
  ]

let test_str_keys =
  let should_fail = test_parse_fails "str-keys" in
  "shouldn't parse string keys">:::[
    should_fail "{\"x x\": true}";
    should_fail "{'x x': true}";
  ]

let test_block_comments =
  let should_fail = test_parse_fails "block-comments"
  and should_pass = test_parses "block-comments" in
  "should parse block comments">:::[
    should_pass "#| anything |#";
    should_pass "#| even with  | pipes |#";
    should_pass "#|||#";
    should_pass "#||||||#";
    should_pass "#| | # | # | # | # |#";
    should_pass "#| back to |##| back |#";
    should_pass "#||##||#";
    should_pass "#|\n|#";
    should_pass "#||#";
    should_pass " #||#";
    should_pass "\n#||#";
    should_pass "\r\n#||#";
    (* Unterminated comments *)
    should_fail "#| #| |#";
    should_fail "#|#||#";
    should_fail "#|#|#";
    (* Nested comments *)
    should_pass "x = #| not #| parsed |# here either |# 5";

    should_pass "#| |# # extra hash for line comment";
    should_fail "#| |# closing hash doesn't count as line comment";

    should_pass "#| |#\nfun f():\n  5\nend\n#| |#";

    (* mid-expression *)
    should_pass "#| |#\nfun f():\n  5 + #| |#\n    5\nend\n#| |#";
    should_pass "lam(x #| stuff |#, y): x + y end";
    should_pass "lam(x #| two |##| comments|#, y): x + y end";

    (* PyretDoc style? *)
    should_pass ("#|\n"^
                 "# Things\n"^
                 "# about \n"^
                 "# the \n"^
                 "# program \n"^
                 "|#");

    (* notices the _first_ close comment *)
    should_fail "#| |# |#";
  ]

let suite = "Parser Suite">:::[
    test_empty;
    test_num;
    test_ident;
    test_lets_letrecs;
    test_type_lets;
    test_standalone_types;
    test_standalone_newtypes;
    test_provide_types;
    test_provide_types_exprs;
    test_english_op_ident;
    test_hyphen_ident;
    test_english_ops;
    test_parse_errors;
    test_angle_brackets_type_inst;
    test_angle_brackets_func;
    test_angle_gt_lt;
    test_angle_whitespace;
    test_paren_op;
    test_semi_eof;
    test_semi_eol;
    test_ws_coloncolon_thickarrow;
    test_paren_group_comma;
    test_paren_group_colon;
    test_paren_group_eq;
    test_paren_group_coloneq;
    test_paren_group_semi;
    test_get_bang;
    test_update;
    test_ref_data_defs;
    test_ref_obj_lits;
    test_imports;
    test_includes;
    test_new_eqops;
    test_examples;
    test_ref_cases;
    test_type_params_methods;
    test_rec_stmts;
    test_brack_exprs;
    test_str_keys;
    test_block_comments
  ]
