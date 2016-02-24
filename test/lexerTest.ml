open Lexer
open Parser
open OUnit2
open Utils

(** Test cases for lexing *)

let tok_words = [
  "an-ident";"import";"provide-types";"provide";"as";"newtype";"type-let";"type";"lazy";
  "var";"letrec";"let";"fun";"lam";"true";"false";"method";"doc:";"check:";"check";
  "except";"cases";"when";"ask:";"otherwise:";"if";"then:";"else:";"else if";"else";
  "data";"with:";"sharing:";"shadow";"mutable";"cyclic";"block:";"for";"from";"end";
  "4/5";"-123/456";"01823.1225426";"-1.2";"1";"-2";"and";"or";"is";"satisfies";"raises";
  "```a one line string```";"```a two\nline string```";"\"a string\"";"'a string'"
]

let tok_words_pairs = [
  ("an-ident", NAME("an-ident"));
  ("import", IMPORT);
  ("provide-types", PROVIDE_TYPES);
  ("provide", PROVIDE);
  ("as", AS);
  ("newtype", NEWTYPE);
  ("type-let", TYPE_LET);
  ("type", TYPE);
  ("lazy", LAZY);
  ("var", VAR);
  ("letrec", LETREC);
  ("let", LET);
  ("fun", FUN);
  ("lam", LAM);
  ("true", TRUE);
  ("false", FALSE);
  ("method", METHOD);
  ("doc:", DOC);
  ("check:", CHECKCOLON);
  ("check", CHECK);
  ("cases", CASES);
  ("when", WHEN);
  ("ask:", ASKCOLON);
  ("otherwise:", OTHERWISECOLON);
  ("if", IF);
  ("then:", THENCOLON);
  ("else:", ELSECOLON);
  ("else if", ELSEIF);
  ("else", ELSE);
  ("data", DATA);
  ("with:", WITH);
  ("sharing:", SHARING);
  ("shadow", SHADOW);
  ("block:", BLOCK);
  ("for", FOR);
  ("from", FROM);
  ("end", END);
  ("4/5", NUMBER(BatNum.of_string "4/5"));
  ("-123/456", NUMBER(BatNum.of_string "-123/456"));
  ("01823.1225426", NUMBER(BatNum.of_string "018231225426/10000000"));
  ("-1.2", NUMBER(BatNum.of_string "-12/10"));
  ("~01823.1225426", ROUGHNUM(01823.1225426));
  ("~-1.2", ROUGHNUM(-1.2));
  ("1", NUMBER(BatNum.of_string "1"));
  ("-2", NUMBER(BatNum.of_string "-2"));
  ("and", AND);
  ("or", OR);
  ("is", IS);
  ("satisfies", SATISFIES);
  ("raises", RAISES);
  ("```a one line string```", STRING("a one line string"));
  ("```a two\nline string```", STRING("a two\nline string"));
  ("\"a string\"", STRING("a string"));
  ("'a string'", STRING("a string"))
]

let tok_opers = [
  ".";"!";"%";",";"->";"=>";":=";"::";":";"|";
  " ^ ";" + ";" - ";" * ";" / ";" <= ";" >= ";" == ";" <> ";" < ";" > ";"<";">";
  "[";"]";"{";"}";"(";")";"=";";";"\\"
]

let compound_toks = [
  "else if";"else:";"else:=";
  "else::";"check:";"check:=";
  "check::";"::";"1.4/5";
  "-2.4/5";"1.01823.1125426";"1.1";
  "-2.01823.1125426";"-2.1"
]

let toks_needing_ws = [
  "::";"=>";"^";
  "+";"-";"*";
  "/";"<=";">=";
  "<>";"<";">"
]

let trimmed_tok_words = List.map String.trim tok_words

let trimmed_tok_opers = List.map String.trim tok_opers

let all_toks = trimmed_tok_words @ trimmed_tok_opers

let ws = [
  ""; "   "; "\n"; "# comment\n\n"; "   \n"; "  \n\n   "; "  \n  # comment  \n   \n   "
]

let test_tight_lexical_extents = function ctx ->
  let runtest (ws1 : string) (tok : string) (ws2 : string) =
    let to_lex = (ws1 ^ tok ^ ws2) in
    let run_test = function() ->
      let lexed = lex_string to_lex in
      (*assert_equal 2 (List.length lexed) ~printer:(Printf.sprintf "%d");
      assert_equal EOF (List.hd (List.tl lexed)) ~printer:tok_to_string;
        assert_bool ("First of "^to_lex^" is not EOF") (not (is_eof (List.hd lexed))) in*)
      (match lexed with
      | [_;EOF] -> ()
      | _ -> assert_failure
               ("Failure: Should have tight lexical extent when tokenizing \""^
                to_lex^"\":\nParse: " ^ (tok_list_printer lexed))) in
    run_test() in
  let loop_ws2 ws1 tok = List.iter (runtest ws1 tok) ws in 
  let loop_all ws1 = List.iter (loop_ws2 ws1) all_toks in
  List.iter loop_all ws

let test_lex_one = function() ->
  let runtest (str,tok) =
    test_lex ("\""^str^"\" lexes to \""^(tok_to_string tok)^"\"") str [tok;EOF] in
  List.map runtest tok_words_pairs

let test_lex_comment comment =
  test_lex (comment^" lexes to EOF") comment [EOF]

let suite =
  "Lexer Tests">:::
  [
    test_lex "Should have tight lexical extents for all tokens" "" [EOF];
    "Should have tight lexical extents for all tokens">::test_tight_lexical_extents;
    test_list (test_lex_one());
    test_lex_comment ("#|\n"^
                      "# Things\n"^
                      "# about \n"^
                      "# the \n"^
                      "# program \n"^
                      "|#");
  ]
