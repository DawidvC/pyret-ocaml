(** Javascript Abstract Syntax Tree Definitions *)

let indent = 2
let break_one = PPrint.sbreak 1
let blank_one = PPrint.blank 1

type string_printer = { append : string -> unit; get : unit -> string }
let string_printer () =
  let strs = ref [] in
  {
    append = (fun s -> strs := s :: !strs);
    get = fun () -> PyretUtils.join_str !strs "";
  }

type label = { get : unit -> int; }

module rec JBlock : sig
  type t = JBlock of JStmt.t ConcatList.t
  val label : t -> string
  val stmts : t -> JStmt.t ConcatList.t
  val print_ugly_source : (string -> unit) -> t -> unit
  val to_ugly_source : t -> string
  val tosource : t -> PPrint.pprintdoc

  class virtual ['a] visitor : object
    method visit_jblock : t -> 'a
    method virtual j_block : JStmt.t ConcatList.t -> 'a
  end
end = struct
  type t = JBlock of JStmt.t ConcatList.t

  let label _ = "j-block"

  let stmts = function
    | JBlock(stmts) -> stmts

  let print_ugly_source printer = function
    | JBlock(stmts) ->
      ConcatList.iter (JStmt.print_ugly_source printer) stmts

  let tosource = function
    | JBlock(stmts) when (ConcatList.is_empty stmts) -> PPrint.mt_doc
    | JBlock(stmts) -> PPrint.vert @@ ConcatList.map_to_list stmts JStmt.tosource

  let to_ugly_source x =
    let strprint = string_printer() in
    print_ugly_source strprint.append x;
    strprint.get()

  class virtual ['a] visitor = object(self)
    method visit_jblock = function
      | JBlock(stmts) -> self#j_block stmts

    method virtual j_block : JStmt.t ConcatList.t -> 'a
  end

end

and JStmt : sig
  type t = JVar of Ast.name * JExpr.t
         | JIf1 of JExpr.t * JBlock.t
         | JIf of JExpr.t * JBlock.t * JBlock.t
         | JReturn of JExpr.t
         | JTryCatch of JBlock.t * Ast.name * JBlock.t
         | JThrow of JExpr.t
         | JSExpr of JExpr.t
         | JBreak
         | JContinue
         | JSwitch of JExpr.t * JCase.t ConcatList.t
         | JWhile of JExpr.t * JBlock.t
         | JFor of bool * JExpr.t * JExpr.t * JExpr.t * JBlock.t
  val label : t -> string
  val print_ugly_source : (string -> unit) -> t -> unit
  val to_ugly_source : t -> string
  val tosource : t -> PPrint.pprintdoc

  class virtual ['a] visitor : object
    method visit_jstmt : t -> 'a
    method virtual j_var : (Ast.name * JExpr.t) -> 'a
    method virtual j_if1 : (JExpr.t * JBlock.t) -> 'a
    method virtual j_if : (JExpr.t * JBlock.t * JBlock.t) -> 'a
    method virtual j_return : JExpr.t -> 'a
    method virtual j_try_catch : (JBlock.t * Ast.name * JBlock.t) -> 'a
    method virtual j_throw : JExpr.t -> 'a
    method virtual j_s_expr : JExpr.t -> 'a
    method virtual j_break : unit -> 'a
    method virtual j_continue : unit -> 'a
    method virtual j_switch : (JExpr.t * JCase.t ConcatList.t) -> 'a
    method virtual j_while : (JExpr.t * JBlock.t) -> 'a
    method virtual j_for : (bool * JExpr.t * JExpr.t * JExpr.t * JBlock.t) -> 'a
  end
end = struct
  type t =
      JVar of Ast.name * JExpr.t
    | JIf1 of JExpr.t * JBlock.t
    | JIf of JExpr.t * JBlock.t * JBlock.t
    | JReturn of JExpr.t
    | JTryCatch of JBlock.t * Ast.name * JBlock.t
    | JThrow of JExpr.t
    | JSExpr of JExpr.t
    | JBreak
    | JContinue
    | JSwitch of JExpr.t * JCase.t ConcatList.t
    | JWhile of JExpr.t * JBlock.t
    | JFor of bool * JExpr.t * JExpr.t * JExpr.t * JBlock.t

  let label = function
    | JVar(_, _) -> "j-var"
    | JIf1(_, _) -> "j-if1"
    | JIf(_, _, _) -> "j-if"
    | JReturn(_) -> "j-return"
    | JTryCatch(_, _, _) -> "j-try-catch"
    | JThrow(_) -> "j-throw"
    | JSExpr(_) -> "j-expr"
    | JBreak -> "j-break"
    | JContinue -> "j-continue"
    | JSwitch(_, _) -> "j-switch"
    | JWhile(_, _) -> "j-while"
    | JFor(_, _, _, _, _) -> "j-for"

  let print_ugly_source printer = function
    | JVar(name, rhs) ->
      printer ("var " ^ (Ast.name_tosourcestring name) ^ " = ");
      JExpr.print_ugly_source printer rhs;
      printer ";\n"
    | JIf1(cond, consq) ->
      printer "if(";
      JExpr.print_ugly_source printer cond;
      printer ") {\n";
      JBlock.print_ugly_source printer consq;
      printer "}\n"
    | JIf(cond, consq, alt) ->
      printer "if(";
      JExpr.print_ugly_source printer cond;
      printer ") {\n";
      JBlock.print_ugly_source printer consq;
      printer "} else {\n";
      JBlock.print_ugly_source printer alt;
      printer "}\n"
    | JReturn(expr) ->
      printer "return ";
      JExpr.print_ugly_source printer expr;
      printer ";\n";
    | JTryCatch(body, exn, catch) ->
      printer "try {\n";
      JBlock.print_ugly_source printer body;
      printer ("} catch (" ^ (Ast.name_tosourcestring exn) ^ ") {\n");
      JBlock.print_ugly_source printer catch;
      printer "}\n"
    | JThrow(exp) ->
      printer "throw ";
      JExpr.print_ugly_source printer exp;
      printer ";\n";
    | JSExpr(expr) ->
      JExpr.print_ugly_source printer expr;
      printer ";\n";
    | JBreak -> printer "break;\n"
    | JContinue -> printer "continue;\n"
    | JSwitch(exp, branches) ->
      printer "switch(";
      JExpr.print_ugly_source printer exp;
      printer ") {\n";
      ConcatList.iter (JCase.print_ugly_source printer) branches;
      printer "}\n"
    | JWhile(cond, body) ->
      printer "while(";
      JExpr.print_ugly_source printer cond;
      printer ") {\n";
      JBlock.print_ugly_source printer body;
      printer "}\n"
    | JFor(create_var, init, cond, update, body) ->
      printer "for(";
      (if create_var then printer "var ");
      JExpr.print_ugly_source printer init;
      printer ";";
      JExpr.print_ugly_source printer cond;
      printer ";";
      JExpr.print_ugly_source printer update;
      printer ") {\n";
      JBlock.print_ugly_source printer body;
      printer "}\n"

  let tosource =
    let (+^) = PPrint.(+^) in
    let groupl lst = PPrint.group
      @@ List.fold_right
        (+^) lst PPrint.mt_doc in
    function
    | JVar(name, rhs) ->
      groupl [
        PPrint.str "var ";
        PPrint.group
        @@ PPrint.nest indent
        @@ List.fold_right (+^) [
          Ast.name_tosource name;
          PPrint.str " =";
          PPrint.sbreak 1;
          JExpr.tosource rhs;
        ] PPrint.mt_doc;
        PPrint.str ";";
      ]
    | JIf1(cond, consq) ->
      groupl [
        PPrint.str "if";
        PPrint.parens (JExpr.tosource cond);
        PPrint.str " ";
        PPrint.surround indent 1
          PPrint.lbrace
          (JBlock.tosource consq)
          PPrint.rbrace;
      ]
    | JIf(cond, consq, alt) ->
      let alt_doc = JBlock.tosource alt in
      let else_doc =
        if alt_doc = PPrint.mt_doc then
          PPrint.mt_doc
        else
          (PPrint.str " else ") +^
          (PPrint.surround indent 1 PPrint.lbrace alt_doc PPrint.rbrace) in
      groupl [
        PPrint.str "if";
        PPrint.parens (JExpr.tosource cond);
        PPrint.str " ";
        PPrint.surround indent 1 PPrint.lbrace (JBlock.tosource consq) PPrint.rbrace;
        else_doc
      ]
    | JReturn(expr) ->
      (PPrint.str "return ") +^ (JExpr.tosource expr) +^ (PPrint.str ";")
    | JTryCatch(body, exn, catch) ->
      (PPrint.surround indent 1 (PPrint.str "try {") (JBlock.tosource body) PPrint.rbrace)
      +^ (PPrint.surround indent 1 (PPrint.str(" catch(" ^ (Ast.name_tosourcestring exn) ^ ") {"))
            (JBlock.tosource catch)
            PPrint.rbrace)
    | JThrow(exp) ->
      (PPrint.group
         (PPrint.nest indent @@ (PPrint.str("throw ")) +^ (JExpr.tosource exp)))
      +^ (PPrint.str ";")
    | JSExpr(expr) ->
      (JExpr.tosource expr) +^ (PPrint.str ";")
    | JBreak -> PPrint.str "break;"
    | JContinue -> PPrint.str "continue;"
    | JSwitch(exp, branches) ->
      PPrint.surround 0 1
        (PPrint.group @@ (PPrint.str "switch") +^ (PPrint.parens (JExpr.tosource exp))
                         +^ (PPrint.sbreak 1) +^ PPrint.lbrace)
        (PPrint.flow_map PPrint.hardline JCase.tosource (ConcatList.to_list branches))
        PPrint.rbrace
    | JWhile(cond, body) ->
      PPrint.surround indent 1
        (groupl [
            PPrint.str "while";
            PPrint.parens (JExpr.tosource cond);
            PPrint.sbreak 1;
            PPrint.lbrace
          ]) (JBlock.tosource body) PPrint.rbrace
    | JFor(create_var, init, cond, update, body) ->
      let semi = (PPrint.str(";")) +^ (PPrint.sbreak 1) in
      let init_src =
        if create_var then (PPrint.str "var ") +^ (JExpr.tosource init)
        else (JExpr.tosource init) in
      PPrint.surround indent 1
        (groupl [
            PPrint.str "for";
            PPrint.parens (init_src +^ semi +^ (JExpr.tosource cond)
                           +^ semi +^ (JExpr.tosource update));
            PPrint.sbreak 1;
            PPrint.lbrace
          ])
        (JBlock.tosource body)
        PPrint.rbrace

  let to_ugly_source x =
    let strprint = string_printer() in
    print_ugly_source strprint.append x;
    strprint.get()

  class virtual ['a] visitor = object(self)
    method visit_jstmt = function
      | JVar(name, rhs) -> self#j_var (name, rhs)
      | JIf1(cond, consq) -> self#j_if1 (cond, consq)
      | JIf(cond, consq, alt) -> self#j_if (cond, consq, alt)
      | JReturn(expr) -> self#j_return (expr)
      | JTryCatch(body, exn, catch) -> self#j_try_catch (body, exn, catch)
      | JThrow(exp) -> self#j_throw (exp)
      | JSExpr(expr) -> self#j_s_expr (expr)
      | JBreak -> self#j_break()
      | JContinue -> self#j_continue()
      | JSwitch(exp, branches) -> self#j_switch (exp, branches)
      | JWhile(cond, body) -> self#j_while (cond, body)
      | JFor(create_var, init, cond, update, body) -> self#j_for (create_var, init, cond, update, body)

    method virtual j_var : (Ast.name * JExpr.t) -> 'a
    method virtual j_if1 : (JExpr.t * JBlock.t) -> 'a
    method virtual j_if : (JExpr.t * JBlock.t * JBlock.t) -> 'a
    method virtual j_return : JExpr.t -> 'a
    method virtual j_try_catch : (JBlock.t * Ast.name * JBlock.t) -> 'a
    method virtual j_throw : JExpr.t -> 'a
    method virtual j_s_expr : JExpr.t -> 'a
    method virtual j_break : unit -> 'a
    method virtual j_continue : unit -> 'a
    method virtual j_switch : (JExpr.t * JCase.t ConcatList.t) -> 'a
    method virtual j_while : (JExpr.t * JBlock.t) -> 'a
    method virtual j_for : (bool * JExpr.t * JExpr.t * JExpr.t * JBlock.t) -> 'a
  end

end

and JCase : sig
  type t =
      JCase of JExpr.t * JBlock.t
    | JDefault of JBlock.t

  val label : t -> string
  val print_ugly_source : (string -> unit) -> t -> unit
  val to_ugly_source : t -> string
  val tosource : t -> PPrint.pprintdoc

  class virtual ['a] visitor : object
    method visit_jcase : t -> 'a
    method virtual j_case : (JExpr.t * JBlock.t) -> 'a
    method virtual j_default : (JBlock.t) -> 'a
  end
end = struct
  type t =
      JCase of JExpr.t * JBlock.t
    | JDefault of JBlock.t

  let label = function
    | JCase(_, _) -> "j-case"
    | JDefault(_) -> "j-default"

  let print_ugly_source printer = function
    | JCase(exp, body) ->
      printer "case ";
      JExpr.print_ugly_source printer exp;
      printer ": ";
      JBlock.print_ugly_source printer body;
    | JDefault(body) ->
      printer "default: ";
      JBlock.print_ugly_source printer body

  let tosource = let (+^) = PPrint.(+^) in
    function
    | JCase(exp, body) ->
      PPrint.group
      @@ PPrint.nest indent
      @@ (PPrint.group
          @@ PPrint.nest indent
          @@ (PPrint.str "case") +^ (JExpr.tosource exp) +^ PPrint.str ":")
         +^ (PPrint.sbreak 1) +^ (JBlock.tosource body)
    | JDefault(body) ->
      PPrint.group @@ PPrint.nest indent @@
      (PPrint.str("default: ")) +^ (PPrint.sbreak 1) +^ (JBlock.tosource body)

  let to_ugly_source x =
    let strprint = string_printer() in
    print_ugly_source strprint.append x;
    strprint.get()

  class virtual ['a] visitor = object(self)
    method visit_jcase = function
      | JCase(exp, body) -> self#j_case (exp, body)
      | JDefault(body) -> self#j_default (body)

    method virtual j_case : (JExpr.t * JBlock.t) -> 'a
    method virtual j_default : (JBlock.t) -> 'a
  end

end

and JBinop : sig
  type t =
      JPlus
    | JMinus
    | JTimes
    | JDivide
    | JAnd
    | JOr
    | JLt
    | JLeq
    | JGt
    | JGeq
    | JEq
    | JEquals
    | JNeq
    | JNequals
    | JInstanceof

  val to_ugly_source : t -> string
  val print_ugly_source : (string -> unit) -> t -> unit
  val tosource : t -> PPrint.pprintdoc
end = struct
  type t =
      JPlus
    | JMinus
    | JTimes
    | JDivide
    | JAnd
    | JOr
    | JLt
    | JLeq
    | JGt
    | JGeq
    | JEq
    | JEquals
    | JNeq
    | JNequals
    | JInstanceof

  let to_ugly_source = function
    | JPlus -> "+"
    | JMinus -> "-"
    | JTimes -> "*"
    | JDivide -> "/"
    | JAnd -> "&&"
    | JOr -> "||"
    | JLt -> "<"
    | JLeq -> "<="
    | JGt -> ">"
    | JGeq -> ">="
    | JEq -> "==="
    | JEquals -> "=="
    | JNeq -> "!=="
    | JNequals -> "!="
    | JInstanceof -> "instanceof"

  let print_ugly_source printer x =
    x
    |> to_ugly_source
    |> printer

  let tosource x =
    PPrint.str (to_ugly_source x)
end

and JUnop : sig
  type t =
      JIncr
    | JDecr
    | JPostIncr
    | JPostDecr
    | JNot

  val to_ugly_source : t -> string
  val print_ugly_source : (string -> unit) -> t -> unit
  val tosource : t -> PPrint.pprintdoc
end = struct
  type t =
      JIncr
    | JDecr
    | JPostIncr
    | JPostDecr
    | JNot

  let to_ugly_source = function
    | JIncr -> "++"
    | JDecr -> "--"
    | JPostIncr -> "++"
    | JPostDecr -> "--"
    | JNot -> "!"

  let print_ugly_source printer x =
    x
    |> to_ugly_source
    |> printer

  let tosource x =
    PPrint.str (to_ugly_source x)

end

and JExpr : sig
  type t =
      JParens of t
    | JUnop of t * JUnop.t
    | JBinop of t * JBinop.t * t
    | JFun of Ast.name ConcatList.t * JBlock.t
    | JNew of t * t ConcatList.t
    | JApp of t * t ConcatList.t
    | JMethod of t * string * t ConcatList.t
    | JTernary of t * t * t
    | JAssign of Ast.name * t
    | JBracketAssign of t * t * t
    | JDotAssign of t * string * t
    | JDot of t * string
    | JBracket of t * t
    | JList of bool * t ConcatList.t
    | JObj of JField.t ConcatList.t
    | JId of Ast.name
    | JStr of string
    | JNum of BatNum.num
    | JTrue
    | JFalse
    | JNull
    | JUndefined
    | JLabel of label

  val print_ugly_source : (string -> unit) -> t -> unit
  val to_ugly_source : t -> string
  val tosource : t -> PPrint.pprintdoc

  class virtual ['a] visitor : object
    method visit_jexpr : t -> 'a
    method virtual j_parens : (t) -> 'a
    method virtual j_unop : (t * JUnop.t) -> 'a
    method virtual j_binop : (t * JBinop.t * t) -> 'a
    method virtual j_fun : (Ast.name ConcatList.t * JBlock.t) -> 'a
    method virtual j_new : (t * t ConcatList.t) -> 'a
    method virtual j_app : (t * t ConcatList.t) -> 'a
    method virtual j_method : (t * string * t ConcatList.t) -> 'a
    method virtual j_ternary : (t * t * t) -> 'a
    method virtual j_assign : (Ast.name * t) -> 'a
    method virtual j_bracket_assign : (t * t * t) -> 'a
    method virtual j_dot_assign : (t * string * t) -> 'a
    method virtual j_dot : (t * string) -> 'a
    method virtual j_bracket : (t * t) -> 'a
    method virtual j_list : (bool * t ConcatList.t) -> 'a
    method virtual j_obj : (JField.t ConcatList.t) -> 'a
    method virtual j_id : (Ast.name) -> 'a
    method virtual j_str : (string) -> 'a
    method virtual j_num : (BatNum.num) -> 'a
    method virtual j_true : unit -> 'a
    method virtual j_false : unit -> 'a
    method virtual j_null : unit -> 'a
    method virtual j_undefined : unit -> 'a
    method virtual j_label : (label) -> 'a
  end
end = struct
  type t =
      JParens of t
    | JUnop of t * JUnop.t
    | JBinop of t * JBinop.t * t
    | JFun of Ast.name ConcatList.t * JBlock.t
    | JNew of t * t ConcatList.t
    | JApp of t * t ConcatList.t
    | JMethod of t * string * t ConcatList.t
    | JTernary of t * t * t
    | JAssign of Ast.name * t
    | JBracketAssign of t * t * t
    | JDotAssign of t * string * t
    | JDot of t * string
    | JBracket of t * t
    | JList of bool * t ConcatList.t
    | JObj of JField.t ConcatList.t
    | JId of Ast.name
    | JStr of string
    | JNum of BatNum.num
    | JTrue
    | JFalse
    | JNull
    | JUndefined
    | JLabel of label

  let rec print_ugly_source printer = function
    | JParens(exp) ->
      printer "(";
      print_ugly_source printer exp;
      printer ")"
    | JUnop(exp, op) ->
      let exp_print () = print_ugly_source printer exp in
      let open JUnop in
      (match op with
       | JPostIncr
       | JPostDecr ->
         exp_print();
         print_ugly_source printer op
       | _ ->
         print_ugly_source printer op;
         exp_print())
    | JBinop(left, op, right) ->
      print_ugly_source printer left;
      printer " ";
      JBinop.print_ugly_source printer op;
      printer " ";
      print_ugly_source printer right
    | JFun(args, body) ->
      printer "function(";
      printer @@ ConcatList.join_str args Ast.name_tosourcestring ",";
      printer ") {\n";
      JBlock.print_ugly_source printer body;
      printer "}"
    | JNew(func, args) ->
      printer "new ";
      print_ugly_source printer func;
      printer "(";
      let args = ConcatList.to_list args in
      List.iter (print_ugly_source printer) args;
      printer ")"
    | JApp(func, args) ->
      print_ugly_source printer func;
      printer "(";
      (match (ConcatList.to_list args) with
       | [] -> ()
       | hd :: tl ->
         print_ugly_source printer hd;
         List.iter (fun a -> printer ","; print_ugly_source printer a) tl);
      printer ")"
    | JMethod(obj, meth, args) ->
      print_ugly_source printer obj;
      printer ".";
      printer meth;
      printer "(";
      (match (ConcatList.to_list args) with
       | [] -> ()
       | hd :: tl ->
         print_ugly_source printer hd;
         List.iter (fun a -> printer ","; print_ugly_source printer a) tl);
      printer ")"
    | JTernary(test, consq, altern) ->
      print_ugly_source printer test;
      printer "?";
      print_ugly_source printer consq;
      printer ":";
      print_ugly_source printer altern
    | JAssign(name, rhs) ->
      printer @@ Ast.name_tosourcestring name;
      printer " = ";
      print_ugly_source printer rhs
    | JBracketAssign(obj, field, rhs) ->
      print_ugly_source printer obj;
      printer "[";
      print_ugly_source printer field;
      printer "]";
      printer " = ";
      print_ugly_source printer rhs
    | JDotAssign(obj, name, rhs) ->
      print_ugly_source printer obj;
      printer ".";
      printer name;
      printer " = ";
      print_ugly_source printer rhs;
    | JDot(obj, field) ->
      print_ugly_source printer obj;
      printer ".";
      printer field
    | JBracket(obj, field) ->
      print_ugly_source printer obj;
      printer "[";
      print_ugly_source printer field;
      printer "]"
    | JList(multi_line, elts) ->
      printer "[";
      let elts = ConcatList.to_list elts in
      (match elts with
       | [] -> ()
       | hd :: tl ->
         print_ugly_source printer hd;
         List.iter (fun f ->
             printer ",";
             (if multi_line then printer "\n");
             print_ugly_source printer f) tl);
      printer "]"
    | JObj(fields) ->
      printer "{";
      let fields = ConcatList.to_list fields in
      (match fields with
       | [] -> ()
       | hd :: tl ->
         JField.print_ugly_source printer hd;
         List.iter (fun f ->
             printer ",\n";
             JField.print_ugly_source printer f) tl);
      printer "}"
    | JId(id) ->
      printer @@ Ast.name_tosourcestring id
    | JStr(s) ->
      printer @@ Printf.sprintf "\"%s\"" (String.escaped s)
    | JNum(n) ->
      printer @@ BatNum.to_string n
    | JTrue ->
      printer "true"
    | JFalse ->
      printer "false"
    | JNull ->
      printer "null"
    | JUndefined ->
      printer "undefined"
    | JLabel(label) ->
      printer @@ string_of_int @@ label.get()

  let rec tosource =
    let open PPrint in
    function
    | JParens(exp) ->
      surround indent 1 (str "(") (tosource exp) (str ")")
    | JUnop(exp, op) ->
      let expsrc = tosource exp
      and opsrc = JUnop.tosource op in
      (match op with
       | JUnop.JPostIncr
       | JUnop.JPostDecr -> expsrc +^ opsrc
       | _ -> opsrc +^ expsrc)
    | JBinop(left, op, right) ->
      flow [tosource left; JBinop.tosource op; tosource right]
    | JFun(args, body) ->
      let arglist =
        nest indent
        @@ surround_separate indent 0 (lparen +^ rparen) lparen commabreak rparen
        @@ ConcatList.map_to_list args Ast.name_tosource in
      let header = group @@ (str "function") +^ arglist in
      surround indent 1
        (header +^ (str " {"))
        (JBlock.tosource body)
        (str "}")
    | JNew(func, args) ->
      group
      @@ (str "new ")
         +^ (tosource func)
         +^ (parens
             @@ nest indent
             @@ separate commabreak
             @@ ConcatList.map_to_list args tosource)
    | JApp(func, args) ->
      group
      @@ (tosource func)
         +^ (parens
             @@ nest indent
             @@ separate commabreak
             @@ ConcatList.map_to_list args tosource)
    | JMethod(obj, meth, args) ->
      group
      @@ (infix indent 0 (str ".") (tosource obj) (str meth))
         +^ (parens
             @@ nest indent
             @@ separate commabreak
             @@ ConcatList.map_to_list args tosource)
    | JTernary(test, consq, altern) ->
      parens
      @@ (tosource test)
         +^ (nest indent
             @@ break_one +^ (str "?") +^ blank_one +^
                (group @@ nest indent @@ tosource consq))
         +^ (nest indent
             @@ break_one +^ (str ":") +^ blank_one +^
                (group @@ nest indent @@ tosource altern))
    | JAssign(name, rhs) ->
      group
      @@ nest indent
      @@ (Ast.name_tosource name)
         +^ (str " =")
         +^ break_one
         +^ (tosource rhs)
    | JBracketAssign(obj, field, rhs) ->
      group
      @@ nest indent
      @@ (tosource obj)
         +^ lbrack
         +^ (tosource field)
         +^ rbrack
         +^ (str " =")
         +^ break_one
         +^ (tosource rhs)
    | JDotAssign(obj, name, rhs) ->
      group
      @@ nest indent
      @@ (infix indent 0 (str ".") (tosource obj) (str name))
         +^ (str " =")
         +^ break_one
         +^ (tosource rhs)
    | JDot(obj, field) ->
      infix indent 0 (str ".") (tosource obj) (str field)
    | JBracket(obj, field) ->
      surround indent 0 lbrack (tosource field) rbrack
    | JList(multi_line, elts) ->
      surround_separate indent 1 (lbrack +^ rbrack)
        lbrack commabreak rbrack
      @@ ConcatList.map_to_list elts tosource
    | JObj(fields) ->
      surround_separate indent 1 (lbrace +^ rbrace)
        lbrace commabreak rbrace
      @@ ConcatList.map_to_list fields JField.tosource
    | JId(id) ->
      Ast.name_tosource id
    | JStr(s) ->
      str @@ Printf.sprintf "\"%s\"" (String.escaped s)
    | JNum(n) ->
      (* NB: Original Pyret implementation used PPrint.number *)
      str @@ BatNum.to_string n
    | JTrue ->
      str "true"
    | JFalse ->
      str "false"
    | JNull ->
      str "null"
    | JUndefined ->
      str "undefined"
    | JLabel(label) ->
      (* NB: Original Pyret implementation used PPrint.number *)
      str @@ string_of_int @@ label.get()

  let to_ugly_source x =
    let strprint = string_printer() in
    print_ugly_source strprint.append x;
    strprint.get()

  class virtual ['a] visitor = object(self)
    method visit_jexpr = function
      | JParens(exp) -> self#j_parens(exp)
      | JUnop(exp, op) -> self#j_unop (exp, op)
      | JBinop(left, op, right) -> self#j_binop (left, op, right)
      | JFun(args, body) -> self#j_fun (args, body)
      | JNew(func, args) -> self#j_new (func, args)
      | JApp(func, args) -> self#j_app (func, args)
      | JMethod(obj, meth, args) -> self#j_method (obj, meth, args)
      | JTernary(test, consq, altern) -> self#j_ternary (test, consq, altern)
      | JAssign(name, rhs) -> self#j_assign (name, rhs)
      | JBracketAssign(obj, field, rhs) -> self#j_bracket_assign (obj, field, rhs)
      | JDotAssign(obj, name, rhs) -> self#j_dot_assign (obj, name, rhs)
      | JDot(name, field) -> self#j_dot (name, field)
      | JBracket(obj, field) -> self#j_bracket (obj, field)
      | JList(multi_line, elts) -> self#j_list (multi_line, elts)
      | JObj(fields) -> self#j_obj (fields)
      | JId(id) -> self#j_id (id)
      | JStr(s) -> self#j_str (s)
      | JNum(n) -> self#j_num (n)
      | JTrue -> self#j_true()
      | JFalse -> self#j_false()
      | JNull -> self#j_null()
      | JUndefined -> self#j_undefined()
      | JLabel(label) -> self#j_label (label)

    method virtual j_parens : (t) -> 'a
    method virtual j_unop : (t * JUnop.t) -> 'a
    method virtual j_binop : (t * JBinop.t * t) -> 'a
    method virtual j_fun : (Ast.name ConcatList.t * JBlock.t) -> 'a
    method virtual j_new : (t * t ConcatList.t) -> 'a
    method virtual j_app : (t * t ConcatList.t) -> 'a
    method virtual j_method : (t * string * t ConcatList.t) -> 'a
    method virtual j_ternary : (t * t * t) -> 'a
    method virtual j_assign : (Ast.name * t) -> 'a
    method virtual j_bracket_assign : (t * t * t) -> 'a
    method virtual j_dot_assign : (t * string * t) -> 'a
    method virtual j_dot : (t * string) -> 'a
    method virtual j_bracket : (t * t) -> 'a
    method virtual j_list : (bool * t ConcatList.t) -> 'a
    method virtual j_obj : (JField.t ConcatList.t) -> 'a
    method virtual j_id : (Ast.name) -> 'a
    method virtual j_str : (string) -> 'a
    method virtual j_num : (BatNum.num) -> 'a
    method virtual j_true : unit -> 'a
    method virtual j_false : unit -> 'a
    method virtual j_null : unit -> 'a
    method virtual j_undefined : unit -> 'a
    method virtual j_label : (label) -> 'a
  end
end

and JField : sig
  type t = JField of string * JExpr.t
  val label : t -> string
  val print_ugly_source : (string -> unit) -> t -> unit
  val to_ugly_source : t -> string
  val tosource : t -> PPrint.pprintdoc

  class virtual ['a] visitor : object
    method visit_jfield : t -> 'a
    method virtual j_field : (string * JExpr.t) -> 'a
  end
end = struct
  type t = JField of string * JExpr.t
  let label _ = "j-field"

  let print_ugly_source printer (JField(name, value)) =
    printer "\"";
    printer name;
    printer "\":";
    JExpr.print_ugly_source printer value

  let tosource (JField(name, value)) =
    let (+^) = PPrint.(+^) in
    PPrint.nest indent
    @@ (PPrint.dquote (PPrint.str name))
       +^ (PPrint.str ": ")
       +^ (JExpr.tosource value)

  let to_ugly_source x =
    let strprint = string_printer() in
    print_ugly_source strprint.append x;
    strprint.get()

  class virtual ['a] visitor = object(self)
    method visit_jfield = function
      | JField(name, value) -> self#j_field (name, value)

    method virtual j_field : (string * JExpr.t) -> 'a
  end

end

class default_map_visitor = object(self)
  inherit [JBlock.t] JBlock.visitor
  inherit [ JStmt.t]  JStmt.visitor
  inherit [ JCase.t]  JCase.visitor
  inherit [ JExpr.t]  JExpr.visitor
  inherit [JField.t] JField.visitor

  method j_block(stmts) = JBlock.JBlock(ConcatList.map self#visit_jstmt stmts)

  method j_var(name, rhs) = JStmt.JVar(name, self#visit_jexpr rhs)
  method j_if1(cond, consq) = JStmt.JIf1(self#visit_jexpr cond, self#visit_jblock consq)
  method j_if(cond, consq, alt) =
    JStmt.JIf(self#visit_jexpr cond, self#visit_jblock consq, self#visit_jblock alt)
  method j_return(exp) = JStmt.JReturn(self#visit_jexpr exp)
  method j_try_catch(body, exn, catch) = JStmt.JTryCatch(self#visit_jblock body, exn,
                                                         self#visit_jblock catch)
  method j_throw(exp) = JStmt.JThrow(self#visit_jexpr exp)
  method j_s_expr(exp) = JStmt.JSExpr(self#visit_jexpr exp)
  method j_break() = JStmt.JBreak
  method j_continue() = JStmt.JContinue
  method j_switch(exp, branches) =
    JStmt.JSwitch(self#visit_jexpr exp, ConcatList.map self#visit_jcase branches)
  method j_while(cond, body) = JStmt.JWhile(self#visit_jexpr cond, self#visit_jblock body)
  method j_for(create_var, init, cond, update, body) =
    JStmt.JFor(create_var, self#visit_jexpr init, self#visit_jexpr cond, self#visit_jexpr update,
               self#visit_jblock body)

  method j_case(exp, body) = JCase.JCase(self#visit_jexpr exp, self#visit_jblock body)
  method j_default(body) = JCase.JDefault(self#visit_jblock body)

  method j_parens(exp) = JExpr.JParens(self#visit_jexpr exp)
  method j_unop(exp, op) = JExpr.JUnop(self#visit_jexpr exp, op)
  method j_binop(left, op, right) = JExpr.JBinop(self#visit_jexpr left, op, self#visit_jexpr right)
  method j_fun(args, body) = JExpr.JFun(args, self#visit_jblock body)
  method j_new(func, args) = JExpr.JNew(self#visit_jexpr func, ConcatList.map self#visit_jexpr args)
  method j_app(func, args) = JExpr.JApp(self#visit_jexpr func, ConcatList.map self#visit_jexpr args)
  method j_method(obj, meth, args) =
    JExpr.JMethod(self#visit_jexpr obj, meth, ConcatList.map self#visit_jexpr args)
  method j_ternary(test, consq, alt) =
    JExpr.JTernary(self#visit_jexpr test, self#visit_jexpr consq, self#visit_jexpr alt)
  method j_assign(name, rhs) = JExpr.JAssign(name, self#visit_jexpr rhs)
  method j_bracket_assign(obj, field, rhs) =
    JExpr.JBracketAssign(self#visit_jexpr obj, self#visit_jexpr field, self#visit_jexpr rhs)
  method j_dot_assign(obj, name, rhs) =
    JExpr.JDotAssign(self#visit_jexpr obj, name, self#visit_jexpr rhs)
  method j_dot(obj, name) = JExpr.JDot(self#visit_jexpr obj, name)
  method j_bracket(obj, field) = JExpr.JBracket(self#visit_jexpr obj, self#visit_jexpr field)
  method j_list(multi_line, fields) = JExpr.JList(multi_line, ConcatList.map self#visit_jexpr fields)
  method j_obj(fields) = JExpr.JObj(ConcatList.map self#visit_jfield fields)
  method j_id(id) = JExpr.JId(id)
  method j_str(s) = JExpr.JStr(s)
  method j_num(n) = JExpr.JNum(n)
  method j_true() = JExpr.JTrue
  method j_false() = JExpr.JFalse
  method j_null() = JExpr.JNull
  method j_undefined() = JExpr.JUndefined
  method j_label(label) = JExpr.JLabel(label)

  method j_field(name, value) = JField.JField(name, value)
end

let make_label_sequence (init : int) =
  let next = ref init in
  (* TODO: This seems obtuse... *)
  fun () ->
    let value = ref None in
    JExpr.JLabel {
      get = fun () ->
        match !value with
        | None ->
          let old_next = !next in
          value := Some(old_next);
          next := old_next + 1;
          old_next
        | Some(v) -> v
    }
