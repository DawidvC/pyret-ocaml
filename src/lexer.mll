{
open Parser
open Lexing

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

let update_linenum lexbuf line =
  update_loc lexbuf None line false 0

let add_char_code buf lexbuf = begin
  let str = lexeme lexbuf in
  let (esc, numstr) = ((String.sub str 1 1), (String.sub str 2 ((String.length str) - 2))) in
  let to_add = (match esc with
    | "u" -> Scanf.sscanf numstr "%x" (fun x -> x)
    | "x" -> Scanf.sscanf numstr "%x" (fun x -> x)
    | _ -> Scanf.sscanf (esc^numstr) "%o" (fun x -> x)) in
  if (to_add > 255) then
    failwith "Unicode Characters are currently unsupported."
  else
    Buffer.add_char buf (Char.chr to_add);
end

let string_to_num str =
  try
    let dec_idx = String.index str '.' in
    let (pre, post) = (String.sub str 0 dec_idx,
                       String.sub str (dec_idx + 1) ((String.length str) - (dec_idx + 1))) in
    let den = "1" ^ (String.make (String.length post) '0') in
    BatNum.of_string (pre ^ post ^ "/" ^ den)
  with (* fails if '.' not in string *)
    | _ -> (BatNum.of_string str)

let unget n lexbuf =
  (* WARNING: Do not unget the start of a line! (This shouldn't be a problem for our lexer) *)
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
    pos_cnum = lexbuf.lex_curr_p.pos_cnum - 1;
  }

let paren_is_for_exp = ref true

let reset = function () -> paren_is_for_exp := true; ()
}
let dec_digit = ['0'-'9']
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let oct_digit = ['0'-'7']
let number_digits = dec_digit+
let sign = ['+' '-']
let exp = (['e' 'E'] sign? number_digits)
let number = sign? number_digits ('.' number_digits)? exp?

let roughnum = '~' number
let rational = sign? number_digits '/' number_digits
let bad_number = sign? '.' number_digits exp?

let identchars = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let ident = identchars ('-'+ ['a'-'z' 'A'-'Z' '0'-'9' '_']+)*

let newline_char = ("\r\n"|"\n\r"|'\n'|'\r')
let comment = '#' ((([^'|'])[^ '\r' '\n']*(newline_char | eof)) | (newline_char | eof))
let blockcommentstart = "#|"
let blockcommentend = "|#"

let blank = [' ' '\t']+

let op_no_space = ('^'|'+'|'-'|'*'|'/'|"<="|">="|"<=>"|">="|"=="|"=~"|"<>"|'<'|'>')

let std_escapes = (("\\" dec_digit dec_digit? dec_digit?)
                 | ("\\x" hex_digit hex_digit?)
                 | ("\\u" hex_digit hex_digit? hex_digit? hex_digit?)
                 | ("\\" ['\\' 'n' 'r' 't' '"' '\''] ))

let tquot_str = "```" (std_escapes
                    | "\\`"
                    | "`"[^ '`']
                    | "``"[^ '`']
                    | [^ '`' '\\'])* "```"

let dquot_str = '"' (std_escapes
                   | [^ '\\' '"' '\n' '\r'])* '"'

let squot_str = '\'' (std_escapes
                   | [^ '\\' '\'' '\n' '\r'])* '\''

let unterminated_str = (['\'' '"'] | "```") _* ([^ '\'' '"' '`'] | [^ '`'] '`' | [^ '`'] "``" | [^ '`'] [^ '`'] '`') eof

let unicode_esc = "\\u" hex_digit (hex_digit (hex_digit hex_digit?)?)?
let hex_esc = "\\x" hex_digit hex_digit?
let oct_esc = "\\" oct_digit (oct_digit oct_digit?)?
let num_esc = (unicode_esc | hex_esc | oct_esc)

let ws_before = (blank | newline_char)
let ws_after = (blank | comment | eof | newline_char)
let op_plus      = ws_before+ '+'   ws_after
let op_caret     = ws_before+ '^'   ws_after
let op_minus     = ws_before+ '-'   ws_after
let op_times     = ws_before+ '*'   ws_after
let op_div       = ws_before+ '*'   ws_after
let op_leq       = ws_before+ "<="  ws_after
let op_geq       = ws_before+ ">="  ws_after
let op_identical = ws_before+ "<=>" ws_after
let op_eq        = ws_before+ "=="  ws_after
let op_eqnow     = ws_before+ "=~"  ws_after
let op_neq       = ws_before+ "<>"  ws_after
let op_lt        = ws_before+ "<"   ws_after
let op_gt        = ws_before+ ">"   ws_after

let coloncolon = "::" ws_after
let thickarrow = "=>" ws_after

rule token = parse
  | blockcommentstart { paren_is_for_exp := false; match (read_comment 0 lexbuf) with
    | None -> token lexbuf
    | Some(t) -> t (* UNTERMINATED_BLOCK_COMMENT *) }
  | comment { paren_is_for_exp := false; token lexbuf }
  | "let" { paren_is_for_exp := false; LET }
  | "fun" { paren_is_for_exp := false; FUN }
  | "where:" { paren_is_for_exp := false; WHERE }
  | "if" { paren_is_for_exp := false; IF }
  | "then:" { paren_is_for_exp := true; THENCOLON }
  | "else:" { paren_is_for_exp := true; ELSECOLON }
  | "else if" { paren_is_for_exp := false; ELSEIF }
  | "end" { paren_is_for_exp := false; END }
  | "and" { paren_is_for_exp := true; AND }
  | "or" { paren_is_for_exp := true; OR }
  | "is==" { paren_is_for_exp := true; ISEQUALEQUAL }
  | "is=~" { paren_is_for_exp := true; ISEQUALTILDE }
  | "is<=>" { paren_is_for_exp := true; ISSPACESHIP }
  | "is-not==" { paren_is_for_exp := true; ISNOTEQUALEQUAL }
  | "is-not=~" { paren_is_for_exp := true; ISNOTEQUALTILDE }
  | "is-not<=>" { paren_is_for_exp := true; ISNOTSPACESHIP }
  | "is-not" { paren_is_for_exp := true; ISNOT }
  | "is" { paren_is_for_exp := true; IS }
  | "satisfies" { paren_is_for_exp := true; SATISFIES }
  | "violates" { paren_is_for_exp := true; SATISFIESNOT }
  | "raises-other-than" { paren_is_for_exp := true; RAISESOTHER }
  | "does-not-raise" { paren_is_for_exp := true; RAISESNOT }
  | "raises-satisfies" { paren_is_for_exp := true; RAISESSATISFIES }
  | "raises-violates" { paren_is_for_exp := true; RAISESVIOLATES }
  | "raises" { paren_is_for_exp := true; RAISES }
  | "->" { paren_is_for_exp := false; THINARROW }
  | thickarrow { paren_is_for_exp := true; THICKARROW }
  | coloncolon { paren_is_for_exp := true; COLONCOLON }
  | ":=" { paren_is_for_exp := true; COLONEQUALS }
  | ':' { paren_is_for_exp := true; COLON }
  | op_caret { paren_is_for_exp := true; CARET }
  | '|' { paren_is_for_exp := true; BAR }
  | op_identical { paren_is_for_exp := true; SPACESHIP }
  | op_leq { paren_is_for_exp := true; LEQ }
  | op_geq { paren_is_for_exp := true; GEQ }
  | op_eq { paren_is_for_exp := true; EQUALEQUAL }
  | op_eqnow { paren_is_for_exp := true; EQUALTILDE }
  | op_neq { paren_is_for_exp := true; NEQ }
  | op_lt { paren_is_for_exp := true; LT }
  | op_gt { paren_is_for_exp := true; GT }
  | op_plus { paren_is_for_exp := true; PLUS }
  | op_minus { paren_is_for_exp := true; DASH }
  | op_times { paren_is_for_exp := true; STAR }
  | op_div { paren_is_for_exp := true; SLASH }
  | "<"   { paren_is_for_exp := false; LANGLE }
  | ">"   { paren_is_for_exp := false; RANGLE }(*
  | [' ']+ '^' '(' { unget 1 lexbuf; paren_is_for_exp := true; CARET }
  | [' ']+ "<=>" '(' { unget 1 lexbuf; paren_is_for_exp := true; SPACESHIP }
  | [' ']+ "<=" '(' { unget 1 lexbuf; paren_is_for_exp := true; LEQ }
  | [' ']+ ">=" '(' { unget 1 lexbuf; paren_is_for_exp := true; GEQ }
  | [' ']+ "==" '(' { unget 1 lexbuf; paren_is_for_exp := true; EQUALEQUAL }
  | [' ']+ "=~" '(' { unget 1 lexbuf; paren_is_for_exp := true; EQUALTILDE }
  | [' ']+ "<>" '(' { unget 1 lexbuf; paren_is_for_exp := true; NEQ }
  | [' ']+ '<' '(' { unget 1 lexbuf; paren_is_for_exp := true; LT }
  | [' ']+ '>' '(' { unget 1 lexbuf; paren_is_for_exp := true; GT }
  | [' ']+ '+' '(' { unget 1 lexbuf; paren_is_for_exp := true; PLUS }
  | [' ']+ '-' '(' { unget 1 lexbuf; paren_is_for_exp := true; DASH }
  | [' ']+ '*' '(' { unget 1 lexbuf; paren_is_for_exp := true; STAR }
  | [' ']+ '/' '(' { unget 1 lexbuf; paren_is_for_exp := true; SLASH } *)
  | op_no_space { paren_is_for_exp := false; BAD_OPER (lexeme lexbuf) }
  | "(("  { let t = if !paren_is_for_exp then PARENSPACE else PARENNOSPACE in
    unget 1 lexbuf;
    paren_is_for_exp := true;
    t }
  | " (" { paren_is_for_exp := true; PARENSPACE (* FIXME: Should modify start position *)}
  | '(' { let t = if !paren_is_for_exp then PARENSPACE else PARENNOSPACE in
    paren_is_for_exp := true; t }
  | ')' { paren_is_for_exp := false; RPAREN }
  | '{' { paren_is_for_exp := false; LBRACE }
  | '}' { paren_is_for_exp := false; RBRACE }
  | '[' { paren_is_for_exp := false; LBRACK }
  | ']' { paren_is_for_exp := false; RBRACK }
  | '.' { paren_is_for_exp := false; DOT }
  | ',' { paren_is_for_exp := true; COMMA }
  | '!' { paren_is_for_exp := false; BANG }
  | '%' { paren_is_for_exp := false; PERCENT }
  | ';' { paren_is_for_exp := false; SEMI }
  | '\\' { paren_is_for_exp := false; BACKSLASH }
  | "=" { paren_is_for_exp := true; EQUALS }
  | "import" { paren_is_for_exp := false; IMPORT }
  | "include" { paren_is_for_exp := false; INCLUDE }
  | "provide-types" { paren_is_for_exp := false; PROVIDE_TYPES }
  | "provide" { paren_is_for_exp := false; PROVIDE }
  | "as" { paren_is_for_exp := false; AS }
  | "newtype" { paren_is_for_exp := false; NEWTYPE }
  | "type-let" { paren_is_for_exp := false; TYPE_LET }
  | "type" { paren_is_for_exp := false; TYPE }
  | "var" { paren_is_for_exp := false; VAR }
  | "rec" { paren_is_for_exp := false; REC }
  | "letrec" { paren_is_for_exp := false; LETREC }
  | "let" { paren_is_for_exp := false; LET }
  | "fun" { paren_is_for_exp := false; FUN }
  | "lam" { paren_is_for_exp := false; LAM }
  | "true" { paren_is_for_exp := false; TRUE }
  | "false" { paren_is_for_exp := false; FALSE }
  | "method" { paren_is_for_exp := false; METHOD }
  | "doc:" { paren_is_for_exp := true; DOC }
  | "where:" { paren_is_for_exp := true; WHERE }
  | "check:" { paren_is_for_exp := true; CHECKCOLON }
  | "examples:" { paren_is_for_exp := true; EXAMPLESCOLON }
  | "check" { paren_is_for_exp := false; CHECK }
  | "cases" { paren_is_for_exp := false; CASES }
  | "when" { paren_is_for_exp := false; WHEN }
  | "ask:" { paren_is_for_exp := true; ASKCOLON }
  | "otherwise:" { paren_is_for_exp := true; OTHERWISECOLON }
  | "else if" { paren_is_for_exp := false; ELSEIF }
  | "else" { paren_is_for_exp := false; ELSE }
  | "datatype" { paren_is_for_exp := false; DATATYPE }
  | "deriving" { paren_is_for_exp := false; DERIVING }
  | "data" { paren_is_for_exp := false; DATA }
  | "with constructor" { paren_is_for_exp := false; WITHCONSTRUCTOR }
  | "with:" { paren_is_for_exp := true; WITH }
  | "sharing:" { paren_is_for_exp := true; SHARING }
  | "shadow" { paren_is_for_exp := false; SHADOW }
  | "ref" { paren_is_for_exp := false; REF }
  | "block:" { paren_is_for_exp := true; BLOCK }
  | "for" { paren_is_for_exp := false; FOR }
  | "from" { paren_is_for_exp := false; FROM }
  | "end" { paren_is_for_exp := false; END }
  | "lazy" { paren_is_for_exp := false; LAZY }
  | ident { paren_is_for_exp := false; let x = lexeme lexbuf in NAME x }
  | "```" { paren_is_for_exp := false; read_tquote_str (Buffer.create 16) lexbuf }
  | '"'   { paren_is_for_exp := false; read_dquote_str (Buffer.create 16) lexbuf }
  | '\''  { paren_is_for_exp := false; read_squote_str (Buffer.create 16) lexbuf }
  | blank { paren_is_for_exp := true; token lexbuf }
  | newline_char { paren_is_for_exp := true; update_loc lexbuf None 1 false 0; token lexbuf }
  | rational { paren_is_for_exp := false; let x = lexeme lexbuf in
    NUMBER (BatNum.of_string x) }
  | number { paren_is_for_exp := false; let x = lexeme lexbuf in
    (*(* Kludge since I cannot into lexing, it would seem *)
    if (String.contains x '.') then
      ROUGHNUM (float_of_string x)
    else*)
      NUMBER (string_to_num x)  }
  | roughnum { paren_is_for_exp := false; let x = lexeme lexbuf in ROUGHNUM (float_of_string (String.sub x 1 ((String.length x) - 1))) }
  | eof { paren_is_for_exp := false; EOF }
  | _ { paren_is_for_exp := false; UNKNOWN (lexeme lexbuf) }

and read_dquote_str buf =
  parse
  | '\\' newline_char { read_dquote_str buf lexbuf }
  | newline_char { UNTERMINATED_STRING }
  | "\\n" { Buffer.add_char buf '\n'; read_dquote_str buf lexbuf }
  | "\\r" { Buffer.add_char buf '\r'; read_dquote_str buf lexbuf }
  | "\\\"" { Buffer.add_char buf '"'; read_dquote_str buf lexbuf }
  | "\\\\" { Buffer.add_char buf '\\'; read_dquote_str buf lexbuf }
  | num_esc { add_char_code buf lexbuf; read_dquote_str buf lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buf (lexeme lexbuf);
    read_dquote_str buf lexbuf }
  | '"' { STRING (Buffer.contents buf) }
  | eof { UNTERMINATED_STRING }
  | _ { failwith ("Illegal string character: " ^ (lexeme lexbuf)) }

and read_squote_str buf =
  parse
  | "\\" newline_char { read_squote_str buf lexbuf }
  | newline_char { UNTERMINATED_STRING }
  | "\\n" { Buffer.add_char buf '\n'; read_squote_str buf lexbuf }
  | "\\r" { Buffer.add_char buf '\r'; read_squote_str buf lexbuf }
  | "\\'" { Buffer.add_char buf '\''; read_squote_str buf lexbuf }
  | "\\\\" { Buffer.add_char buf '\\'; read_squote_str buf lexbuf }
  | num_esc { add_char_code buf lexbuf; read_squote_str buf lexbuf }
  | [^ ''' '\\']+ { Buffer.add_string buf (lexeme lexbuf);
    read_squote_str buf lexbuf }
  | '\'' { STRING (Buffer.contents buf) }
  | eof { UNTERMINATED_STRING }
  | _ { failwith ("Illegal string character: " ^ (lexeme lexbuf)) }

and read_tquote_str buf =
  parse
  | "\\" newline_char { read_tquote_str buf lexbuf }
  | "\\n" { Buffer.add_char buf '\n'; read_tquote_str buf lexbuf }
  | "\\r" { Buffer.add_char buf '\r'; read_tquote_str buf lexbuf }
  | "\\`" { Buffer.add_char buf '`'; read_tquote_str buf lexbuf }
  | "\\\\" { Buffer.add_char buf '\\'; read_tquote_str buf lexbuf }
  | num_esc { add_char_code buf lexbuf; read_tquote_str buf lexbuf }
  | '`' [^'`'] { Buffer.add_string buf (lexeme lexbuf); read_tquote_str buf lexbuf }
  | "``" [^'`'] { Buffer.add_string buf (lexeme lexbuf); read_tquote_str buf lexbuf }
  | [^ '`' '\\']+ { Buffer.add_string buf (lexeme lexbuf);
    read_tquote_str buf lexbuf }
  | "```" { STRING (Buffer.contents buf) }
  | eof { UNTERMINATED_STRING }
  | _ { failwith ("Illegal string character: " ^ (lexeme lexbuf)) }

and read_comment nesting =
  parse
  | newline_char {update_linenum lexbuf 1; read_comment nesting lexbuf}
  | blockcommentend { if (nesting = 0) then None else
    (read_comment (nesting - 1) lexbuf) }
  | blockcommentstart { read_comment (nesting + 1) lexbuf }
  | _ { read_comment nesting lexbuf }
  | eof { Some(UNTERMINATED_BLOCK_COMMENT) }

{
let tok_to_string t = match t with
  | IF -> "IF"
  | THENCOLON -> "THENCOLON"
  | ELSECOLON -> "ELSECOLON"
  | ELSEIF -> "ELSEIF"
  | ELSE -> "ELSE"
  | END -> "END"
  | AND -> "AND"
  | OR -> "OR"
  | ISEQUALEQUAL -> "ISEQUALEQUAL"
  | ISEQUALTILDE -> "ISEQUALTILDE"
  | ISSPACESHIP -> "ISSPACESHIP"
  | ISNOTEQUALEQUAL -> "ISNOTEQUALEQUAL"
  | ISNOTEQUALTILDE -> "ISNOTEQUALTILDE"
  | ISNOTSPACESHIP -> "ISNOTSPACESHIP"
  | ISNOT -> "ISNOT"
  | IS -> "IS"
  | SATISFIES -> "SATISFIES"
  | SATISFIESNOT -> "SATISFIESNOT"
  | RAISESOTHER -> "RAISESOTHER"
  | RAISESNOT -> "RAISESNOT"
  | RAISESSATISFIES -> "RAISESSATISFIES"
  | RAISES -> "RAISES"
  | THINARROW -> "THINARROW"
  | THICKARROW -> "THICKARROW"
  | COLONCOLON -> "COLONCOLON"
  | COLONEQUALS -> "COLONEQUALS"
  | COLON -> "COLON"
  | CARET -> "CARET"
  | BAR -> "BAR"
  | SPACESHIP -> "SPACESHIP"
  | LEQ -> "LEQ"
  | GEQ -> "GEQ"
  | EQUALEQUAL -> "EQUALEQUAL"
  | EQUALTILDE -> "EQUALTILDE"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | GT -> "GT"
  | PLUS -> "PLUS"
  | DASH -> "DASH"
  | STAR -> "STAR"
  | SLASH -> "SLASH"
  | PARENSPACE -> "PARENSPACE"
  | PARENNOSPACE -> "PARENNOSPACE"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | DOT -> "DOT"
  | COMMA -> "COMMA"
  | BANG -> "BANG"
  | PERCENT -> "PERCENT"
  | SEMI -> "SEMI"
  | BACKSLASH -> "BACKSLASH"
  | WS -> "WS"
  | IMPORT -> "IMPORT"
  | INCLUDE -> "INCLUDE"
  | PROVIDE_TYPES -> "PROVIDE_TYPES"
  | PROVIDE -> "PROVIDE"
  | AS -> "AS"
  | NEWTYPE -> "NEWTYPE"
  | TYPE_LET -> "TYPE_LET"
  | TYPE -> "TYPE"
  | VAR -> "VAR"
  | REC -> "REC"
  | LETREC -> "LETREC"
  | LET -> "LET"
  | FUN -> "FUN"
  | LAM -> "LAM"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | METHOD -> "METHOD"
  | DOC -> "DOC"
  | WHERE -> "WHERE"
  | CHECKCOLON -> "CHECKCOLON"
  | EXAMPLESCOLON -> "EXAMPLESCOLON"
  | CHECK -> "CHECK"
  | CASES -> "CASES"
  | WHEN -> "WHEN"
  | ASKCOLON -> "ASKCOLON"
  | OTHERWISECOLON -> "OTHERWISECOLON"
  | DATA -> "DATA"
  | WITH -> "WITH"
  | SHARING -> "SHARING"
  | SHADOW -> "SHADOW"
  | REF -> "REF"
  | BLOCK -> "BLOCK"
  | FOR -> "FOR"
  | FROM -> "FROM"
  | LAZY -> "LAZY"
  | EOF -> "EOF"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | UNTERMINATED_STRING -> "UNTERMINATED_STRING"
  | UNTERMINATED_BLOCK_COMMENT -> "UNTERMINATED_BLOCK_COMMENT"
  | EQUALS -> "EQUALS"
  | WITHCONSTRUCTOR -> "WITHCONSTRUCTOR"
  | DATATYPE -> "DATATYPE"
  | DERIVING -> "DERIVING"
  | RAISESVIOLATES -> "RAISESVIOLATES"
  | COMMENT -> "COMMENT"
  | LANGLE -> "LANGLE"
  | RANGLE -> "RANGLE"
  | NAME(s) -> Printf.sprintf "NAME<%s>" s
  | STRING(s) -> Printf.sprintf "STRING<%s>" s
  | NUMBER(n) -> Printf.sprintf "NUMBER<%s>" (BatNum.to_string n)
  | ROUGHNUM(n) -> Printf.sprintf "ROUGHNUM<%f>" n
  | BAD_OPER(o) -> Printf.sprintf "BAD_OPER<%s>" o
  | UNKNOWN(u) -> Printf.sprintf "UNKNOWN<%s>" u

let tok_list_to_string lst =
  "["^(List.fold_right (fun x acc-> (tok_to_string x) ^ "; " ^ acc) lst "")^"]"

}