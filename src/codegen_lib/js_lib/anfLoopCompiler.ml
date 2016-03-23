open PyretUtils
open ConcatList
open JAst;;

let type_name = (^) "$type$"

let js_names = Ast.make_name 0
let js_ids = MutableStringDict.create 50
let effective_ids = MutableStringDict.create 50

let rec fresh_id (id : Ast.name) : Ast.name =
  let open MutableStringDict in
  let open Str in
  let open Ast in
  let base_name = match id with
    | STypeGlobal(_) -> name_tosourcestring id
    | _ -> name_toname id in
  let no_hyphens = global_replace (regexp "-") "$" base_name in
  let n = js_names no_hyphens in
  let n_srcstr = name_tosourcestring n in
  if mem effective_ids n_srcstr then
    fresh_id id (* Awkward name collision! *)
  else
    begin
      add effective_ids n_srcstr true;
      n
    end

let js_id_of (id : Ast.name) : Ast.name =
  let open MutableStringDict in
  let open Ast in
  let s = name_key id in
  if mem js_ids s then
    find js_ids s
  else
    begin
      let safe_id = fresh_id id in
      add js_ids s safe_id;
      safe_id
    end

let const_id (name : string) : Ast.name =
  Ast.SName(Ast.dummy_loc, name)

let compiler_name (id : string) : Ast.name =
  const_id @@ "$" ^ id

let formal_shadow_name (id : Ast.name) : Ast.name =
  let js_id = js_id_of id in
  Ast.SName(Ast.dummy_loc, "$" ^ (Ast.name_tosourcestring js_id))

let j_const_id : string -> JExpr.t = (fun x -> JExpr.JId(x)) ||> const_id

let get_field_loc = j_const_id "G"
let throw_uninitialized = j_const_id "U"
let source_name = j_const_id "M"
let undefined = j_const_id "D"
let _RUNTIME = j_const_id "R"
let _NAMESPACE = j_const_id "NAMESPACE"
let _THIS = j_const_id "this"
let _ARGUMENTS = j_const_id "arguments"

let rt_field name = JExpr.JDot(_RUNTIME, name)
let rt_method name args = JExpr.JMethod(_RUNTIME, name, args)
let app _ f args = JExpr.JMethod(f, "app", args)

let j_bool b = if b then JExpr.JTrue else JExpr.JFalse

let obj_of_loc l =
  let open BatNum in
  let open Ast.Srcloc in
  let open JExpr in
  match l with
  | Builtin(name) -> JList(false, of_list [JStr(name)])
  | Srcloc(_, start_line, start_col, start_char, end_line, end_col, end_char) ->
    JList(false,
          of_list [
            source_name;
            JNum(num_of_int start_line);
            JNum(num_of_int start_col);
            JNum(num_of_int start_char);
            JNum(num_of_int end_line);
            JNum(num_of_int end_col);
            JNum(num_of_int end_char);
          ])

let get_dict_field obj field =
  let open JExpr in
  JBracket(JDot(obj, "dict"), field)

let get_field (obj : JExpr.t) (field : JExpr.t) (loc : JExpr.t) : JExpr.t =
  JExpr.JApp(get_field_loc, of_list [obj; field; loc])

let get_field_ref (obj : JExpr.t) (field : JExpr.t) (loc : JExpr.t) : JExpr.t =
  rt_method "getFieldRef" @@ of_list [obj; field; loc]

let raise_id_exn loc name =
  let open JExpr in
  JApp(throw_uninitialized, of_list [loc; JStr(name)])

let add_stack_frame exn_id loc =
  let open JExpr in
  JMethod(JDot(JId(exn_id), "pyretStack"), "push", of_list [loc])

let check_fun l f =
  let open JStmt in
  let open JExpr in
  JIf1(JUnop(JParens(rt_method "is_function" @@ of_list [f]), JUnop.JNot),
       JBlock.JBlock(of_list [
           JSExpr(JMethod(rt_field "ffi", "throwNonFunApp", of_list [l; f]))
         ]))

let thunk_app block =
  let open JExpr in
  JApp(JParens(JFun(ConcatEmpty, block)), ConcatEmpty)

let thunk_app_stmt stmt =
  thunk_app @@ JBlock.JBlock(of_list [stmt])

let rec compile_ann (ann : Ast.ann) visitor =
  let open Ast in
  let open JExpr in
  let open DAGUtils in
  let destruct_cexp = function
    | CExp(exp, others) -> (exp, others)
    | _ -> failwith "Should not happen" in
  match ann with
  | AName(_, n) -> CExp(JId(js_id_of n), ConcatEmpty)
  | AArrow(_, _, _, _) -> CExp(rt_field "Function", ConcatEmpty)
  | AMethod(_, _, _) -> CExp(rt_field "Method", ConcatEmpty)
  | AApp(l, base, _) -> compile_ann base visitor
  | ARecord(l, fields) ->
    let (names, locs, fields, others) =
      let foldfun (names, locs, fields, others) = function
        | AField(l, name, ann) ->
          let (exp, other_stmts) = destruct_cexp @@ compile_ann ann visitor in
          (ConcatSnoc(names, JStr(name)),
           ConcatSnoc(locs, visitor # get_loc l),
           ConcatSnoc(fields, JField.JField(name, exp)),
           ConcatAppend(others, other_stmts)) in
      List.fold_left foldfun (ConcatEmpty, ConcatEmpty, ConcatEmpty, ConcatEmpty) fields in
    CExp(rt_method "makeRecordAnn" @@ of_list [
        JList(false, names);
        JList(false, locs);
        JObj(fields)
      ], others)
  | APred(l, base, exp) ->
    let name = match exp with
      | SId(_, id)
      | SIdLetrec(_, id, _) -> name_toname id
      | _ -> failwith @@ "Invalid name: " ^ (Sexplib.Sexp.to_string_hum @@ sexp_of_expr exp) in
    let expr_to_compile = match exp with
      | SId(l2, id) -> AstAnf.AId(l2, id)
      | SIdLetrec(l2, id, ok) -> AstAnf.AIdLetrec(l2, id, ok)
      | _ -> failwith "Impossible" in
    let (base_exp, base_others) = destruct_cexp @@ compile_ann base visitor in
    let (exp_exp,  exp_others ) = destruct_cexp @@ visitor # visit_expr expr_to_compile in
    CExp(rt_method "makePredAnn" @@ of_list [base_exp; exp_exp; JStr(name)],
         ConcatAppend(base_others, exp_others))
  | ADot(l, m, field) ->
    CExp(rt_method "getDotAnn" @@ of_list [
        visitor # get_loc l;
        JStr(name_toname m);
        JId(js_id_of m);
        JStr(field)
      ], ConcatEmpty)
  | ATypeVar(_, _)
  | ABlank
  | AAny -> CExp(rt_field "Any", ConcatEmpty)
  | AChecked(_,_) -> failwith "Should not happen"

(** Generates code which checks if the current function was called with `arity' arguments *)
let arity_check loc_expr arity =
  let open JBlock in
  let open JStmt in
  let open JExpr in
  let arity = BatNum.num_of_int arity in
  let len_id = compiler_name "l"
  and iter_id = compiler_name "i"
  and t_id = compiler_name "t" in
  let len = JId(len_id)
  and iter = JId(iter_id)
  and t = JId(t_id) in
  of_list [
    JVar(len_id, JDot(_ARGUMENTS, "length"));
    JIf1(JBinop(len, JBinop.JNeq, JNum(arity)),
         JBlock(of_list [
             JVar(t_id, JNew(JId(const_id "Array"), of_list [len]));
             JFor(true,
                  JAssign(iter_id, JNum(BatNum.num_of_int 0)),
                  JBinop(iter, JBinop.JLt, len),
                  JUnop(iter, JUnop.JIncr),
                  JBlock(of_list [
                      JSExpr(JBracketAssign(t, iter, JBracket(_ARGUMENTS, iter)))]));
             JSExpr(rt_method "checkArityC" @@ of_list [loc_expr; JNum(arity); t])]))]

let no_vars () = MutableStringDict.create 1

class local_bound_vars_visitor = object(self)
  inherit [Ast.name MutableStringDict.t] JExpr.visitor
  inherit [Ast.name MutableStringDict.t] JField.visitor

  (* Just visit contents *)
  method j_parens = self # visit_jexpr
  method j_unop(exp, _) = self # visit_jexpr exp
  method j_binop(left, _, right) =
    let ans = self # visit_jexpr left in
    MutableStringDict.merge ans @@ self # visit_jexpr right;
    ans
  method j_fun _ = no_vars()
  method j_new(func, args) =
    let base = self # visit_jexpr func in
    iter (fun arg -> MutableStringDict.merge base @@ self # visit_jexpr arg) args;
    base
  method j_app(func, args) =
    let base = self # visit_jexpr func in
    iter (fun arg -> MutableStringDict.merge base @@ self # visit_jexpr arg) args;
    base
  method j_method _ = no_vars()
  method j_ternary(test, consq, alt) =
    let ans = self # visit_jexpr test in
    MutableStringDict.merge ans @@ self # visit_jexpr consq;
    MutableStringDict.merge ans @@ self # visit_jexpr alt;
    ans

  method j_field(_, value) = self # visit_jexpr value
end
