open PyretUtils
open ConcatList
open DAGUtils
open JAst
open JUnop
open JBinop
open JField
open JCase
open JBlock
open JStmt
open JExpr;;

let (|@>) cl1 cl2 = ConcatAppend(cl1, cl2)

let get_bind = function
  | AstAnf.ACasesBind(_, _, bnd) -> bnd
let get_id = function
  | AstAnf.ABind(_, id, _) -> id

let num_of_int = BatNum.num_of_int

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

let j_const_id : string -> JExpr.t = (fun x -> JId(x)) ||> const_id

let get_field_loc = j_const_id "G"
let throw_uninitialized = j_const_id "U"
let source_name = j_const_id "M"
let undefined = j_const_id "D"
let _RUNTIME = j_const_id "R"
let _NAMESPACE = j_const_id "NAMESPACE"
let _THIS = j_const_id "this"
let _ARGUMENTS = j_const_id "arguments"

let rt_field name = JDot(_RUNTIME, name)
let rt_method name args = JMethod(_RUNTIME, name, args)
let app _ f args = JMethod(f, "app", args)

let j_bool b = if b then JTrue else JFalse

let obj_of_loc l =
  let open Ast.Srcloc in
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
  JBracket(JDot(obj, "dict"), field)

let get_field (obj : JExpr.t) (field : JExpr.t) (loc : JExpr.t) : JExpr.t =
  JApp(get_field_loc, of_list [obj; field; loc])

let get_field_ref (obj : JExpr.t) (field : JExpr.t) (loc : JExpr.t) : JExpr.t =
  rt_method "getFieldRef" @@ of_list [obj; field; loc]

let raise_id_exn loc name =
  JApp(throw_uninitialized, of_list [loc; JStr(name)])

let add_stack_frame exn_id loc =
  JMethod(JDot(JId(exn_id), "pyretStack"), "push", of_list [loc])

let check_fun l f =
  JIf1(JUnop(JParens(rt_method "is_function" @@ of_list [f]), JNot),
       JBlock(of_list [
           JSExpr(JMethod(rt_field "ffi", "throwNonFunApp", of_list [l; f]))
         ]))

let thunk_app block =
  JApp(JParens(JFun(ConcatEmpty, block)), ConcatEmpty)

let thunk_app_stmt stmt =
  thunk_app @@ JBlock(of_list [stmt])

let rec compile_ann (ann : Ast.ann) visitor =
  let open Ast in
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
           ConcatSnoc(fields, JField(name, exp)),
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
    let (exp_exp,  exp_others ) = destruct_cexp @@ visitor # visit_value expr_to_compile in
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
  let arity = num_of_int arity in
  let len_id = compiler_name "l"
  and iter_id = compiler_name "i"
  and t_id = compiler_name "t" in
  let len = JId(len_id)
  and iter = JId(iter_id)
  and t = JId(t_id) in
  of_list [
    JVar(len_id, JDot(_ARGUMENTS, "length"));
    JIf1(JBinop(len, JNeq, JNum(arity)),
         JBlock(of_list [
             JVar(t_id, JNew(JId(const_id "Array"), of_list [len]));
             JFor(true,
                  JAssign(iter_id, JNum(num_of_int 0)),
                  JBinop(iter, JLt, len),
                  JUnop(iter, JIncr),
                  JBlock(of_list [
                      JSExpr(JBracketAssign(t, iter, JBracket(_ARGUMENTS, iter)))]));
             JSExpr(rt_method "checkArityC" @@ of_list [loc_expr; JNum(arity); t])]))]

let no_vars () = MutableStringDict.create 1

(** Like MutableStringDict.merge, but returns the merged dictionary
    (i.e. this is a functional version) *)
let func_merge dct1 dct2 =
  MutableStringDict.merge dct1 dct2;
  dct1

let flip (f : 'a -> 'b -> 'c) : ('b -> 'a -> 'c) = fun a b -> f b a

(** `flip_merge a b' == `func_merge b a' (useful for pipelining) *)
let flip_merge = flip func_merge

class local_bound_vars_visitor = object(self)
  inherit [Ast.name MutableStringDict.t] JBlock.visitor
  inherit [Ast.name MutableStringDict.t] JStmt.visitor
  inherit [Ast.name MutableStringDict.t] JCase.visitor
  inherit [Ast.name MutableStringDict.t] JExpr.visitor
  inherit [Ast.name MutableStringDict.t] JField.visitor

  method j_block(stmts) =
    let base = MutableStringDict.create 50 in
    (* Reminder: `||>' is function composition *)
    iter (MutableStringDict.merge base ||> self # visit_jstmt) stmts;
    base

  method j_var(name, rhs) =
    (* Ignore all variables named $underscore##### *)
    match name with
    | Ast.SAtom("$underscore", _) -> self # visit_jexpr rhs
    | _ ->
      let ans = self # visit_jexpr rhs in
      MutableStringDict.add ans (Ast.name_key name) name;
      ans
  method j_if1(cond, consq) =
    self # visit_jexpr cond
    |> flip_merge @@ self # visit_jblock consq
  method j_if(cond, consq, alt) =
    self # visit_jexpr cond
    |> flip_merge @@ self # visit_jblock consq
    |> flip_merge @@ self # visit_jblock alt
  (* Just visit return's contents *)
  method j_return = self # visit_jexpr
  method j_try_catch(body, _, catch) =
    self # visit_jblock body
    |> flip_merge @@ self # visit_jblock catch
  (* Just visit throw's contents *)
  method j_throw = self # visit_jexpr
  (* Just visit expression in statement*)
  method j_s_expr = self # visit_jexpr
  method j_break() = no_vars()
  method j_continue() = no_vars()
  method j_switch(exp, branches) =
    let base = self # visit_jexpr exp in
    iter (MutableStringDict.merge base ||> self # visit_jcase) branches;
    base
  method j_while(cond, body) =
    self # visit_jexpr cond
    |> flip_merge @@ self # visit_jblock body
  method j_for(_, init, cond, update, body) =
    self # visit_jexpr init
    |> flip_merge @@ self # visit_jexpr cond
    |> flip_merge @@ self # visit_jexpr update
    |> flip_merge @@ self # visit_jblock body

  method j_case(exp, body) =
    self # visit_jexpr exp
    |> flip_merge @@ self # visit_jblock body
  (* Just visit default's body*)
  method j_default = self # visit_jblock

  (* Just visit contents *)
  method j_parens = self # visit_jexpr
  method j_unop(exp, _) = self # visit_jexpr exp
  method j_binop(left, _, right) =
    self # visit_jexpr left
    |> flip_merge  @@ self # visit_jexpr right
  method j_fun _ = no_vars()
  method j_new(func, args) =
    let base = self # visit_jexpr func in
    iter (MutableStringDict.merge base ||> self # visit_jexpr) args;
    base
  method j_app(func, args) =
    let base = self # visit_jexpr func in
    iter (MutableStringDict.merge base ||> self # visit_jexpr) args;
    base
  method j_method _ = no_vars()
  method j_ternary(test, consq, alt) =
    self # visit_jexpr test
    |> flip_merge @@ self # visit_jexpr consq
    |> flip_merge @@ self # visit_jexpr alt
  method j_assign(_, rhs) = self # visit_jexpr rhs
  method j_bracket_assign(obj, field, rhs) =
    self # visit_jexpr obj
    |> flip_merge @@ self # visit_jexpr field
    |> flip_merge @@ self # visit_jexpr rhs
  method j_dot_assign(obj, _, rhs) =
    self # visit_jexpr obj
    |> flip_merge @@ self # visit_jexpr rhs
  method j_dot(obj, _) = self # visit_jexpr obj
  method j_bracket(obj, field) =
    self # visit_jexpr obj
    |> flip_merge @@ self # visit_jexpr field
  method j_list(_, elts) =
    let base = MutableStringDict.create 10 in
    iter (MutableStringDict.merge base ||> self # visit_jexpr) elts;
    base
  method j_obj(fields) =
    let base = MutableStringDict.create 10 in
    iter (MutableStringDict.merge base ||> self # visit_jfield) fields;
    base
  method j_id(_)   = no_vars()
  method j_str(_)  = no_vars()
  method j_num(_)  = no_vars()
  method j_true()  = no_vars()
  method j_false() = no_vars()
  method j_null()  = no_vars()
  method j_undefined() = no_vars()
  method j_label(_) = no_vars()

  method j_field(_, value) = self # visit_jexpr value
end

(* Functional versions of ConcatList constructors *)
let cl_empty = ConcatEmpty
let cl_append a b = ConcatAppend(a, b)
let cl_snoc cl elt = ConcatSnoc(cl, elt)
let cl_cons elt cl = ConcatCons(elt, cl)

let destruct_exp =
  function
  | CExp(e, os) -> e, os
  | CBlock(_, _) -> failwith "Expected CExp, got CBlock"
  | CField(_, _) -> failwith "Expected CExp, got CField"

let destruct_field =
  function
  | CField(f, os) -> f, os
  | CExp(_, _) -> failwith "Expected CField, got CExp"
  | CBlock(_, _) -> failwith "Expected CField, got CBlock"

let destruct_block =
  function
  | CBlock(b, nc) -> b, nc
  | CExp(_, _) -> failwith "Expected CBlock, got CExp"
  | CField(_, _) -> failwith "Expected CBlock, got CField"

let get_exp = fst ||> destruct_exp

(* TODO: Refactor. This entire class is a horrifying behemoth. *)
class virtual compiler_visitor gl ca r opts = object(self)
  inherit [DAGUtils.case_results] AstAnf.default_folding_visitor

  val options : CompileStructs.compile_options = opts
  method get_loc = gl
  val cur_apploc = ca
  val resumer = r

  (* Bootstrapping *)
  val label_maker = fun () -> failwith "make_label() called before label_maker initialized"
  method make_label() = label_maker()

  val cur_target : JExpr.t option = None
  method get_cur_target() =
    match cur_target with
    | None -> failwith "get_cur_target() called before cur_target initialized"
    | Some(t) -> t

  val cur_step : Ast.name option = None
  method get_cur_step() =
    match cur_step with
    | None -> failwith "get_cur_step() called before cur_step initialized"
    | Some(s) -> s

  val cur_ans : Ast.name option = None
  method get_cur_ans() =
    match cur_ans with
    | None -> failwith "get_cur_ans() called before cur_ans initialized"
    | Some(a) -> a

  (* Helper methods *)
  method compile_fun_body (l : Ast.loc) (step : Ast.name) (fun_name : Ast.name)
      (args : AstAnf.bind list) (opt_arity : int option) (body : AstAnf.expr)
      (should_report_error_frame : bool) : JBlock.t =
    let make_label = make_label_sequence 0 in
    let ret_label = make_label() in
    let ans = fresh_id @@ compiler_name "ans" in
    let apploc = fresh_id @@ compiler_name "al" in
    let local_compiler = {< label_maker = make_label; cur_target = Some(ret_label);
                            cur_step = Some(step); cur_ans = Some(ans); cur_apploc = apploc >} in
    let visited_body = local_compiler#visit_expr body in
    let visited_body_block, visited_body_new_cases = destruct_block visited_body in
    (* To avoid penalty for assigning to formal parameters and also using the arguments object,
       we create a shadow set of formal arguments, and immediately assign them to the "real" ones
       in the normal entry case.  This expands the function preamble, but might enable JS optimizations,
       so it should be worth it *)
    let formal_args = args |> List.map (function
        | AstAnf.ABind(l, id, ann) -> AstAnf.ABind(l, formal_shadow_name id, ann)) in
    let no_real_args =
      match args with
      | (AstAnf.ABind(_, id, _)) :: _ -> id = (resumer)
      | _ -> failwith "compile_fun_body called without resumer as argument" in
    let copy_formals_to_args =
      if no_real_args then
        ConcatEmpty
      else
        map_list2 (fun (AstAnf.ABind(_,formal_arg,_)) (AstAnf.ABind(_,arg,_)) ->
            JVar(js_id_of arg, JId(formal_arg))) formal_args args in
    let ann_cases = local_compiler#compile_anns step args (local_compiler#make_label()) in
    let main_body_cases =
      match ann_cases with
      | (new_cases, new_label) ->
        cl_empty
        |@> new_cases
        |> flip cl_snoc @@ JCase(new_label, visited_body_block)
        |@> visited_body_new_cases in
    main_body_cases |> iter (function
        | JCase(JLabel(label), _) -> let _ = label.get() in ()
        | JCase(_,_) -> failwith "Non-JLabel in exp position of JCase"
        | _ -> ());
    let Results(main_body_cases, dead_vars) = simplify main_body_cases step in
    let all_vars = MutableStringDict.create 20 in
    main_body_cases |> iter (MutableStringDict.merge all_vars ||> (new local_bound_vars_visitor)#visit_jcase);
    let all_needed_vars = MutableStringDict.copy all_vars in
    (StringDict.bindings dead_vars) |> List.iter (fun (k, _) -> MutableStringDict.remove all_needed_vars k);
    let vars = MutableStringDict.bindings all_needed_vars |> List.map snd in
    let switch_cases =
      main_body_cases
      |> flip cl_snoc @@ JCase(local_compiler#get_cur_target(), JBlock(of_list [
          JSExpr(JUnop(rt_field "GAS", JIncr));
          JReturn(JId(local_compiler#get_cur_ans()))]))
      |> flip cl_snoc @@ JDefault(JBlock(of_list [
          JThrow(JBinop(JBinop(JStr("No case numbered "), JPlus, JId(step)), JPlus,
                        JStr(" in " ^ (Ast.name_tosourcestring fun_name))))])) in

    let act_record = rt_method "makeActivationRecord" @@ of_list [
        JId(apploc);
        JId(fun_name);
        JId(step);
        JList(false, if no_real_args then ConcatEmpty
              else map_list (fun (AstAnf.ABind(_,id,_)) -> JId(js_id_of id)) args);
        JList(false, map_list (fun v -> JId(v)) vars)
      ] in
    let e = fresh_id @@ compiler_name "e" in
    let first_arg = match formal_args with
      | [] -> failwith "Impossible"
      | (AstAnf.ABind(_, id, _)) :: _ -> id in
    (* (Used during stack traces)
       let entry_exit = of_list [
        JStr(Ast.str_of_loc l);
        JNum(num_of_int @@ List.length vars);
      ] in *)
    let preamble =
      let restorer =
        JBlock(
          of_list [
            JSExpr(JAssign(step, JDot(JId(first_arg), "step")));
            JSExpr(JAssign(apploc, JDot(JId(first_arg), "from")));
            JSExpr(JAssign(local_compiler#get_cur_ans(), JDot(JId(first_arg), "ans")));]
          |@> (args |> map_list_n (fun i (AstAnf.ABind(_,id,_)) ->
              JSExpr(JAssign(js_id_of id, JBracket(JDot(JId(first_arg), "args"), JNum(num_of_int i))))) 0)
          |@> (vars |> map_list_n (fun i v ->
              JSExpr(JAssign(v, JBracket(JDot(JId(first_arg), "vars"), JNum(num_of_int i))))) 0)) in
      match opt_arity with
      | Some(arity) ->
        JIf(rt_method "isActivationRecord" @@  of_list [JId(first_arg)],
            restorer,
            JBlock(
              arity_check (local_compiler#get_loc(l)) arity
              |> flip cl_append copy_formals_to_args))
      | None ->
        if no_real_args then
          JIf1(rt_method "isActivationRecord" @@ of_list [JId(first_arg)], restorer)
        else
          JIf(rt_method "isActivationRecord" @@ of_list [JId(first_arg)], restorer,
              JBlock(copy_formals_to_args)) in
    let stack_attach_guard =
      if options.CompileStructs.proper_tail_calls then
        JBinop(rt_method "isCont" @@ of_list [JId(e)], JAnd,
               JParens(JBinop(JId(step), JNeq, ret_label)))
      else
        rt_method "isCont" @@ of_list [JId(e)] in

    JBlock(of_list [
        JVar(step, JNum(num_of_int 0));
        JVar(local_compiler#get_cur_ans(), undefined);
        JVar(apploc, local_compiler#get_loc l);
        JTryCatch(
          JBlock(of_list [
              preamble;
              JIf1(JBinop(JUnop(rt_field "GAS", JDecr), JLeq, JNum(num_of_int 0)),
                   JBlock(of_list [
                       JSExpr(JDotAssign(_RUNTIME, "EXN_STACKHEIGHT", JNum(num_of_int 0)));
                       JThrow(rt_method "makeCont" ConcatEmpty)]));
              JWhile(JTrue,
                     JBlock(of_list [
                         JSwitch(JId(step), switch_cases)]))]),
          e,
          JBlock(((of_list [
              JIf1(stack_attach_guard,
                   JBlock(of_list [JSExpr(JBracketAssign(JDot(JId(e), "stack"),
                                                         JUnop(rt_field "EXN_STACKHEIGHT", JPostIncr),
                                                         act_record))]))])
                 |@> (if should_report_error_frame then
                        of_list [
                          JIf1(rt_method "isPyretException" @@ of_list [JId(e)],
                               JBlock(of_list [JSExpr(add_stack_frame e (JId(apploc)))]))
                        ]
                      else ConcatEmpty))
                 |@> (of_list [JThrow(JId(e))])))])


  method compile_anns step (binds : AstAnf.bind list) entry_label =
    let cur_target = ref entry_label in
    let new_cases = List.fold_left (fun acc ->
        function
        | AstAnf.ABind(_, bid, ann) ->
          match ann with
          | Ast.ABlank
          | Ast.AAny -> acc
          | _ -> 
            let compiled_ann = compile_ann ann self in
            let new_label = self#make_label() in
            match compiled_ann with
            | CExp(exp, other_stmts) ->
              let new_case = JCase(!cur_target,
                                   JBlock(ConcatAppend(other_stmts, of_list [
                                       JSExpr(JAssign(step, new_label));
                                       JSExpr(JAssign(cur_apploc, self#get_loc(Ast.ann_loc ann)));
                                       JSExpr(rt_method "_checkAnn" @@
                                              of_list [self#get_loc(Ast.ann_loc ann);
                                                       exp;
                                                       JId(js_id_of bid)]);
                                       JBreak]))) in
              cur_target := new_label;
              ConcatSnoc(acc, new_case)
            | CBlock(_, _) -> failwith "Expected CExp, got CBlock"
            | CField(_, _) -> failwith "Expected CExp, got CField"
      ) ConcatEmpty binds in
    (new_cases, !cur_target)

  method compile_annotated_let (b : AstAnf.bind) (compiled_e : DAGUtils.case_results)
      (compiled_body : DAGUtils.case_results) : DAGUtils.case_results =
    let compiled_e_exp, compiled_e_other_stmts =
      match compiled_e with
      | CExp(exp, os) -> exp, os
      | CField(_, _) -> failwith "Expected CExp, got CField"
      | CBlock(_, _) -> failwith "Expected CExp, got CBlock" in
    let compiled_body_block_stmts, compiled_body_new_cases =
      match compiled_body with
      | CBlock(JBlock(stmts), new_cases) -> stmts, new_cases
      | CExp(_, _) -> failwith "Expected CBlock, got CExp"
      | CField(_, _) -> failwith "Expected CBlock, got CField" in
    match b with
    | AstAnf.ABind(loc, id, ann) ->
      match ann with
      | Ast.ABlank
      | Ast.AAny ->
        CBlock(JBlock(
            compiled_e_other_stmts
            |> flip cl_append @@ (ConcatSingleton(JVar(js_id_of id, compiled_e_exp)))
            |> flip cl_append compiled_body_block_stmts), compiled_body_new_cases)
      | _ ->
        let step = self#get_cur_step() in
        let after_ann = self#make_label() in
        let after_ann_case = JCase(after_ann, JBlock(compiled_body_block_stmts)) in
        let compiled_ann = compile_ann ann self in
        match compiled_ann with
        | CExp(compiled_ann_exp, compiled_ann_other_stmts) ->
          CBlock(JBlock(compiled_e_other_stmts
                        |@> (ConcatSingleton(JVar(js_id_of id, compiled_e_exp)))
                        |@> compiled_ann_other_stmts
                        |@> of_list [
                          JSExpr(JAssign(step, after_ann));
                          JSExpr(JAssign(cur_apploc, self#get_loc(Ast.ann_loc ann)));
                          JSExpr(rt_method "_checkAnn" @@ of_list [
                              self#get_loc(Ast.ann_loc ann);
                              compiled_ann_exp;
                              JId(js_id_of id)])]),
                 ConcatCons(after_ann_case, compiled_body_new_cases))
        | CField(_, _) -> failwith "Expected CExp, got CField"
        | CBlock(_, _) -> failwith "Expected CExp, got CBlock"

  method get_new_cases opt_dest opt_body after_label ans =
    let opt_compiled_body = Option.map self#visit_expr opt_body in
    match opt_dest with
    | Some(dest) ->
      (match opt_compiled_body with
       | Some(compiled_body) ->
         let compiled_binding = self#compile_annotated_let dest (CExp(JId(ans), ConcatEmpty)) compiled_body in
         let block, new_cases =
           match compiled_binding with
           | CBlock(b, nc) -> b, nc
           | CExp(_, _) -> failwith "Expected CBlock, got CExp"
           | CField(_, _) -> failwith "Expected CBlock, got CField" in
         ConcatCons(JCase(after_label, block), new_cases)
       | None -> failwith "Impossible: compile-split-app can't have a dest without a body")
    | None ->
      match opt_compiled_body with
      | Some(compiled_body) ->
        let block, new_cases =
          match compiled_body with
          | CBlock(b, nc) -> b, nc
          | CExp(_, _) -> failwith "Expected CBlock, got CExp"
          | CField(_, _) -> failwith "Expected CBlock, got CField" in
        ConcatCons(JCase(after_label, block), new_cases)
      | None -> ConcatEmpty

  method compile_split_method_app l opt_dest obj methname args opt_body =
    let ans = self#get_cur_ans() in
    let step = self#get_cur_step() in
    let compiled_obj = get_exp @@ self#visit_value obj in
    let compiled_args = map_list (get_exp ||> self#visit_value) args in
    let opt_id = match compiled_obj with
    | JId(id) -> Some(compiled_obj)
    | _ -> None in

    (* Used if opt_id is None *)
    let obj_id_id = fresh_id @@ compiler_name "obj" in
    let obj_id =  JId(obj_id_id) in
    let colon_field = rt_method "getColonFieldLoc" @@
      of_list [Option.default obj_id opt_id; JStr(methname); self#get_loc(l)] in
    let colon_field_id_id = fresh_id @@ compiler_name "field" in
    let colon_field_id = JId(colon_field_id_id) in
    let check_method = rt_method "isMethod" @@ of_list [colon_field_id] in
    let after_app_label =
      match opt_body with
      | None -> self#get_cur_target()
      | Some(_) -> self#make_label() in
    let new_cases = self#get_new_cases opt_dest opt_body after_app_label ans in
    CBlock(JBlock(
        (of_list [
            JSExpr(JAssign(step, after_app_label));
            JSExpr(JAssign(cur_apploc, self#get_loc(l)))
          ])
        |@> (match opt_id with
            | Some(_) -> ConcatSingleton(JSExpr(JAssign(colon_field_id_id, colon_field)))
            | None -> of_list [JVar(obj_id_id, compiled_obj);
                               JVar(colon_field_id_id, colon_field)])
        |@> of_list [
          JIf(check_method,
              JBlock(of_list [
                  JSExpr(JAssign(ans, JApp(JDot(colon_field_id, "full_meth"),
                                           ConcatCons(obj_id, compiled_args))))]),
              JBlock(of_list [
                  check_fun (self#get_loc(l)) colon_field_id;
                  JSExpr(JAssign(ans, app (self#get_loc(l)) colon_field_id compiled_args))
                ]));
          JBreak]), new_cases)

  method compile_split_app l opt_dest f args opt_body =
    let ans = self#get_cur_ans() in
    let step = self#get_cur_step() in
    let compiled_f = get_exp @@ self#visit_value f in
    let compiled_args = map_list (get_exp ||> self#visit_value) args in
    let after_app_label =
      match opt_body with
      | None -> self#get_cur_target()
      | Some(_) -> self#make_label() in
    let new_cases = self#get_new_cases opt_dest opt_body after_app_label ans in
    CBlock(JBlock(of_list [
        JSExpr(JAssign(step, after_app_label));
        JSExpr(JAssign(cur_apploc, self#get_loc(l)));
        check_fun (JId(cur_apploc)) compiled_f;
        JSExpr(JAssign(ans, app (self#get_loc(l)) compiled_f compiled_args));
        JBreak]), new_cases)

  method compile_split_if opt_dest cond consq alt opt_body =
    let consq_label = self#make_label() in
    let alt_label = self#make_label() in
    let after_if_label =
      match opt_body with
      | None -> self#get_cur_target()
      | Some(_) -> self#make_label() in
    let ans = self#get_cur_ans() in
    let compiler_after_if = {< cur_target = Some(after_if_label) >} in
    let compiled_consq_block, compiled_consq_cases = destruct_block @@ compiler_after_if#visit_expr consq in
    let compiled_alt_block, compiled_alt_cases = destruct_block @@ compiler_after_if#visit_expr alt in
    let new_cases =
      (ConcatCons(JCase(consq_label, compiled_consq_block), compiled_consq_cases))
      |@> (ConcatCons(JCase(alt_label, compiled_alt_block), compiled_alt_cases))
      |@> self#get_new_cases opt_dest opt_body after_if_label ans in
    CBlock(JBlock(of_list [
        JIf(rt_method "isPyretTrue" @@ of_list [get_exp @@ self#visit_value cond],
            JBlock(of_list [JSExpr(JAssign(self#get_cur_step(), consq_label))]),
            JBlock(of_list [JSExpr(JAssign(self#get_cur_step(), alt_label))]));
        JBreak]), new_cases)

  method compile_cases_branch compiled_val (branch : AstAnf.cases_branch) =
    let open AstAnf in
    let compiled_body = self#visit_cases_branch branch in
    let compiled_body_block, compiled_body_new_cases = destruct_block compiled_body in
    if (ConcatList.length compiled_body_new_cases) < 5 then
      self#compile_inline_cases_branch compiled_val branch compiled_body
    else
      begin
        let temp_branch = fresh_id @@ compiler_name "temp_branch" in
        let branch_args =
          match branch with
          | ACasesBranch(loc, pat_loc, _, arghd :: argtl, _) ->
            List.map (function
                | ACasesBind(_, _, b) -> b) (arghd :: argtl)
          | ACasesBranch(_, _, _, _, body)
          | ASingletonCasesBranch(_, _, _, body) ->
            [ABind(expr_loc body, resumer, Ast.ABlank)] in
        let step = fresh_id @@ compiler_name "step" in
        let ref_binds_mask =
          match branch with
          | ACasesBranch(_, _, _, args, _) ->
            JList(false, map_list (function
                | ACasesBind(_,ft,_) ->
                  j_bool (match ft with
                      | Ast.SCasesBindRef -> true
                      | Ast.SCasesBindNormal -> false)) args)
          | ASingletonCasesBranch(_, _, _, _) -> JList(false, ConcatEmpty) in
        let branch_l, branch_body, branch_body_l =
          match branch with
          | ACasesBranch(l, _, _, _, body)
          | ASingletonCasesBranch(l, _, _, body) ->
            l, body, (expr_loc body) in
        let compiled_branch_fun =
          self#compile_fun_body branch_body_l step temp_branch branch_args None branch_body true in
        let preamble = self#cases_preamble compiled_val branch in
        let deref_fields =
          JSExpr(JAssign(self#get_cur_ans(),
                        JMethod(compiled_val, "$app_fields", of_list [JId(temp_branch); ref_binds_mask]))) in
        let actual_app =
          of_list [
            JSExpr(JAssign(self#get_cur_step(), self#get_cur_target()));
            JSExpr(JAssign(cur_apploc, self#get_loc(branch_l)));
            JVar(temp_branch,
                 JFun(map_list (function
                     | ABind(_, id, _) -> formal_shadow_name id) branch_args, compiled_branch_fun));
            deref_fields;
            JBreak] in
        CBlock(JBlock(preamble |@> actual_app), ConcatEmpty)
      end

  method cases_preamble compiled_val branch =
    let checker = match branch with
      | AstAnf.ACasesBranch(_, pat_loc, name, args, body) ->
        let branch_given_arity = JNum(num_of_int @@ List.length args) in
        let obj_expected_arity = JDot(compiled_val, "$arity") in
        JIf(JBinop(obj_expected_arity, JGeq, JNum(num_of_int 0)),
            JBlock(of_list [
                JIf1(JBinop(branch_given_arity, JNeq, obj_expected_arity),
                     JBlock(of_list [
                         JSExpr(JMethod(rt_field "ffi", "throwCasesArityErrorC",
                                        of_list [self#get_loc(pat_loc);
                                                 branch_given_arity;
                                                 obj_expected_arity]))]))]),
            JBlock(of_list [
                JSExpr(JMethod(rt_field "ffi", "throwCasesSingletonErrorC",
                               of_list [self#get_loc(pat_loc); JTrue]))]))
      | AstAnf.ASingletonCasesBranch(_, pat_loc, _, _) ->
        JIf1(JBinop(JDot(compiled_val, "$arity"), JNeq, JNum(num_of_int (-1))),
             JBlock(of_list [
                 JSExpr(JMethod(rt_field "ffi", "throwCasesSingletonErrorC",
                                of_list [self#get_loc(pat_loc); JFalse]))])) in
    of_list [checker]

  method compile_inline_cases_branch compiled_val branch compiled_body =
    let preamble = self#cases_preamble compiled_val branch in
    let JBlock(body_stmts), body_cases = destruct_block compiled_body in
    match branch with
    | AstAnf.ASingletonCasesBranch(_, _, _, _) ->
      CBlock(JBlock(preamble |@> body_stmts), body_cases)
    | AstAnf.ACasesBranch(l, pat_loc, typ, args, body) ->
      let entry_label = self#make_label() in
      let ann_cases_cases, ann_cases_label =
        self#compile_anns (self#get_cur_step()) (List.map get_bind args) entry_label in
      let field_names_id = js_id_of @@ compiler_name "fn" in
      let field_names = JId(field_names_id) in
      let get_field_names = JVar(field_names_id, JDot(JDot(compiled_val, "$constructor"), "$fieldNames")) in
      let deref_fields =
        map_list_n (fun i arg ->
            let i = num_of_int i in
            let mask = JBracket(JDot(compiled_val, "$mut_fields_mask"), JNum(i)) in
            let field = get_dict_field compiled_val @@ JBracket(field_names, JNum(i)) in
            let arg_bind_id =
              match get_bind arg with
              | AstAnf.ABind(_, id, _) -> id in
            let arg_is_ref =
              match arg with
              | AstAnf.ACasesBind(_, ft, _) ->
                match ft with
                | Ast.SCasesBindRef -> JTrue
                | Ast.SCasesBindNormal -> JFalse in
            JVar(js_id_of arg_bind_id, rt_method "derefField" @@ of_list [field; mask; arg_is_ref])) 0 args in
      match ann_cases_cases with
      | ConcatEmpty ->
        CBlock(JBlock(
            (preamble
             |> flip cl_snoc get_field_names)
            |@> deref_fields
            |@> body_stmts), body_cases)
      | _ ->
        CBlock(JBlock
                 (((preamble
                    |> flip cl_snoc get_field_names)
                   |@> deref_fields)
                  |> flip cl_snoc @@ JSExpr(JAssign(self#get_cur_step(), entry_label))
                  |> flip cl_snoc JBreak),
               ann_cases_cases
               |@> body_cases
               |> flip cl_snoc @@ JCase(ann_cases_label, JBlock(body_stmts)))

  method compile_split_cases cases_loc opt_dest typ value branches _else opt_body =
    let compiled_val = get_exp @@ self#visit_value value in
    let after_cases_label =
      match opt_body with
      | None -> self#get_cur_target()
      | Some(_) -> self#make_label() in
    let compiler_after_cases = {< cur_target = Some(after_cases_label) >} in
    let compiled_branches = List.map (compiler_after_cases#compile_cases_branch compiled_val) branches in
    let compiled_else = compiler_after_cases#visit_expr _else in
    let branch_labels = List.map (fun _ -> self#make_label()) branches in
    let else_label = self#make_label() in
    let branch_cases = List.fold_left2 (fun acc label branch ->
        let block, new_cases = destruct_block branch in
        (acc
         |> flip cl_snoc @@ JCase(label, block))
        |@> new_cases) ConcatEmpty branch_labels compiled_branches in
    let branch_else_cases =
      let block, new_cases = destruct_block compiled_else in
      (branch_cases
       |> flip cl_snoc @@ JCase(else_label, block))
      |@> new_cases in
    let dispatch_table = JObj(map_list2 (fun branch label ->
        match branch with
        | AstAnf.ACasesBranch(_, _, name, _, _)
        | AstAnf.ASingletonCasesBranch(_, _, name, _) -> JField(name, label)) branches branch_labels) in
    let dispatch_id = fresh_id @@ compiler_name "cases_dispatch" in
    let dispatch = JId(dispatch_id) in
    let new_cases =
      branch_else_cases
      |@> self#get_new_cases opt_dest opt_body after_cases_label (self#get_cur_ans()) in
    CBlock(JBlock(of_list [
        JVar(dispatch_id, dispatch_table);
        JSExpr(JAssign(cur_apploc, self#get_loc(cases_loc)));
        JSExpr(JAssign(self#get_cur_step(),
                       JBinop(JBracket(dispatch, JDot(compiled_val, "$name")), JOr, else_label)));
        JBreak]), new_cases)

  method compile_split_update opt_dest obj fields opt_body =
    let ans = self#get_cur_ans() in
    let step = self#get_cur_step() in
    let compiled_obj = get_exp @@ self#visit_value obj in
    (* Unzip fields *)
    let field_locs, field_names, field_vals =
      List.fold_right (fun (AstAnf.AField(l, n, v)) (locs, names, vals)->
        l :: locs, n :: names, v :: vals) fields ([],[],[]) in
    let compiled_field_vals = map_list (get_exp ||> self#visit_value) field_vals in
    let field_names = map_list (fun n -> JStr(n)) field_names in
    let field_locs = map_list (self#get_loc) field_locs in
    let after_update_label =
      match opt_body with
      | None -> self#get_cur_target()
      | Some(_) -> self#make_label() in
    let new_cases = self#get_new_cases opt_dest opt_body after_update_label ans in
    CBlock(JBlock(of_list [
        JSExpr(JAssign(step, after_update_label));
        JSExpr(JAssign(ans, rt_method "checkRefAnns" @@ of_list [
            compiled_obj;
            JList(false, field_names);
            JList(false, compiled_field_vals);
            JList(false, field_locs);
          ]));
        JBreak;
      ]), new_cases)

  (* BEGIN visitor methods *)
  method a_module(l, answer, dvs, dts, provides, types, checks) =
    let type_obj_fields, type_others =
      types |> List.fold_left (fun (fields, others) (Ast.AField(_, name, ann)) ->
        let compiled = compile_ann ann self in
        let exp, other_stmts =
          match compiled with
          | CExp(exp, os) -> exp, os
          | CBlock(_, _) -> failwith "Expected CExp, got CBlock"
          | CField(_, _) -> failwith "Expected CExp, got CField" in
        (ConcatSnoc(fields, JField(name, exp)), others |@> other_stmts)) (ConcatEmpty, ConcatEmpty) in

    let compiled_provides = self#visit_value provides in
    let compiled_answer = self#visit_value answer in
    let compiled_checks = self#visit_value checks in

    let prov_exp, prov_stmts = destruct_exp compiled_provides in
    let ans_exp, ans_stmts = destruct_exp compiled_answer in
    let check_exp, check_stmts = destruct_exp compiled_checks in

    CExp(rt_method "makeObject" @@ of_list [
        JObj(of_list [
            JField("answer", ans_exp);
            JField("namespace", _NAMESPACE);
            JField("defined-values",
                   JObj(map_list (fun (AstAnf.ADefinedValue(name, value)) ->
                       let compiled_val = get_exp @@ self#visit_value value in
                       JField(name, compiled_val)) dvs));
            JField("defined-types",
                   JObj(map_list (fun (AstAnf.ADefinedType(name, typ)) ->
                       let compiled_ann = get_exp @@ compile_ann typ self in
                       JField(name, compiled_ann)) dts));
            JField("provide-plus-types",
                   rt_method "makeObject" @@ of_list [
                     JObj(of_list [
                         JField("values", prov_exp);
                         JField("types", JObj(type_obj_fields))])]);
            JField("checks", check_exp)])],
         type_others |@> prov_stmts |@> ans_stmts |@> check_stmts)

  method a_type_let(l, bind, body) =
    let open AstAnf in
    match bind with
    | ATypeBind(l2, name, ann) ->
      let JBlock(visited_body_block_stmts), visited_body_new_cases =
        destruct_block @@ self#visit_expr body in
      let compiled_ann_exp, compiled_ann_other_stmts = destruct_exp @@ compile_ann ann self in
      CBlock(JBlock(compiled_ann_other_stmts
                    |@> (of_list [JVar(js_id_of name, compiled_ann_exp)])
                    |@> visited_body_block_stmts), visited_body_new_cases)
    | ANewtypeBind(l2, name, nameb) ->
      let brander_id = js_id_of nameb in
      let JBlock(visited_body_block_stmts), visited_body_new_cases =
        destruct_block @@ self#visit_expr body in
      CBlock(JBlock((of_list [
          JVar(brander_id, rt_method "namedBrander" @@ of_list [JStr(Ast.name_toname name); self#get_loc(l2)]);
          JVar(js_id_of name, rt_method "makeBranderAnn"
               @@ of_list [JId(brander_id); JStr(Ast.name_toname name)])
        ]) |@> visited_body_block_stmts), visited_body_new_cases)

  method a_let(l, b, e, body) =
    let open AstAnf in
    match e with
    | AApp(l2, f, args) ->
      self#compile_split_app l2 (Some(b)) f args (Some(body))
    | AMethodApp(l2, obj, m, args) ->
      self#compile_split_method_app l2 (Some(b)) obj m args (Some(body))
    | AIf(l2, cond, thn, els) ->
      self#compile_split_if (Some(b)) cond thn els (Some(body))
    | ACases(l2, typ, value, branches, _else) ->
      self#compile_split_cases l2 (Some(b)) typ value branches _else (Some(body))
    | AUpdate(l2, obj, fields) ->
      self#compile_split_update (Some(b)) obj fields (Some(body))
    | _ ->
      let compiled_e = self#visit_lettable e in
      let compiled_body = self#visit_expr body in
      self#compile_annotated_let b compiled_e compiled_body

  method a_var(l, b, e, body) =
    let compiled_body = self#visit_expr body in
    let compiled_e = self#visit_lettable e in
    let b_id =
      match b with
      | AstAnf.ABind(_, id, _) -> id in
    let e_exp = get_exp compiled_e in
    let JBlock(body_stmts), body_new_cases = destruct_block compiled_body in
    CBlock(JBlock(ConcatCons(JVar(js_id_of b_id,
                                  JObj(of_list [JField("$var", e_exp)])),
                             body_stmts)), body_new_cases)

  method a_seq(l, e1, e2) =
    let open AstAnf in
    match e1 with
    | AApp(l2, f, args) ->
      self#compile_split_app l2 None f args (Some(e2))
    | AMethodApp(l2, obj, m, args) ->
      self#compile_split_method_app l2 None obj m args (Some(e2))
    | AIf(l2, cond, thn, els) ->
      self#compile_split_if None cond thn els (Some(e2))
    | ACases(l2, typ, value, branches, _else) ->
      self#compile_split_cases l2 None typ value branches _else (Some(e2))
    | AUpdate(l2, obj, fields) ->
      self#compile_split_update None obj fields (Some(e2))
    | _ ->
      let compiled_e1 = self#visit_lettable e1 in
      let compiled_e2 = self#visit_expr e2 in
      let first_stmt = JSExpr(get_exp compiled_e1) in
      let _, e1_other_stmts = destruct_exp compiled_e1 in
      let JBlock(e2_stmts), e2_new_cases = destruct_block compiled_e2 in
      CBlock(JBlock(e1_other_stmts |@> ConcatCons(first_stmt, e2_stmts)), e2_new_cases)

  method a_if _ : DAGUtils.case_results =
    failwith "Impossible: a_if directly in compiler_visitor should never happen"
  method a_cases _ : DAGUtils.case_results =
    failwith "Impossible: a_cases directly in compiler_visitor should never happen"
  method a_update _ : DAGUtils.case_results =
    failwith "Impossible: a_update directly in compiler_visitor should never happen"

  method a_lettable(_, e) =
    let open AstAnf in
    match e with
    | AApp(l2, f, args) ->
      self#compile_split_app l2 None f args None
    | AMethodApp(l2, obj, m, args) ->
      self#compile_split_method_app l2 None obj m args None
    | AIf(l2, cond, thn, els) ->
      self#compile_split_if None cond thn els None
    | ACases(l2, typ, value, branches, _else) ->
      self#compile_split_cases l2 None typ value branches _else None
    | AUpdate(l2, obj, fields) ->
      self#compile_split_update None obj fields None
    | _ ->
      let compiled_e = self#visit_lettable e in
      let e_exp, e_other_stmts = destruct_exp compiled_e in
      CBlock(JBlock(
          (ConcatSingleton(JSExpr(JAssign(self#get_cur_step(), self#get_cur_target()))))
          |@> e_other_stmts
          |@> (of_list [
              JSExpr(JAssign(self#get_cur_ans(), e_exp));
              JBreak])), ConcatEmpty)

  method a_assign(l, id, value) =
    let visit_value = self#visit_value value in
    let val_exp, val_other_stmts = destruct_exp visit_value in
    CExp(JDotAssign(JId(js_id_of id), "$var", val_exp), val_other_stmts)

  method a_app _ = failwith "Impossible: a_app directly in compiler_visitor should never happen"

  method a_prim_app(l, f, args) =
    let visit_args = List.map self#visit_value args in
    let set_loc = of_list [JSExpr(JAssign(cur_apploc, self#get_loc(l)))] in
    let other_stmts = List.fold_right (fun va acc ->
        let _, os = destruct_exp va in os |@> acc) visit_args set_loc in
    CExp(rt_method f @@ map_list get_exp visit_args, other_stmts)

  method a_ref(l, maybe_ann) =
    match maybe_ann with
    | None -> CExp(rt_method "makeGraphableRef" ConcatEmpty, ConcatEmpty)
    | Some(ann) -> failwith "Cannot handle annotations in refs yet"

  method a_obj(l, fields) =
    let visit_fields = List.map (destruct_field ||> self#visit_field) fields in
    let fields, other_stmts =
      List.fold_right (fun (f, os) (fs, oss) -> ConcatCons(f,fs), (os |@> oss))
        visit_fields (ConcatEmpty, ConcatEmpty) in
    CExp(rt_method "makeObject" @@ of_list [JObj(fields)], other_stmts)

  method a_get_bang(l, obj, field) =
    let obj_exp, obj_other_stmts = destruct_exp @@ self#visit_value obj in
    CExp(rt_method "getFieldRef" @@ of_list [obj_exp; JStr(field); self#get_loc(l)], obj_other_stmts)

  method a_extend(l, obj, fields) =
    let obj_exp, obj_other_stmts = destruct_exp @@ self#visit_value obj in
    let visit_fields = List.map (destruct_field ||> self#visit_field) fields in
    let fields, other_stmts =
      List.fold_right (fun (f, os) (fs, oss) -> ConcatCons(f, fs), (os |@> oss))
        visit_fields (ConcatEmpty, obj_other_stmts) in
    CExp(rt_method "extendObj" @@ of_list [
        self#get_loc(l);
        obj_exp;
        JObj(fields);
      ], other_stmts)

  method a_dot(l, obj, field) =
    let obj_exp, obj_other_stmts = destruct_exp @@ self#visit_value obj in
    CExp(get_field obj_exp (JStr(field)) (self#get_loc(l)), obj_other_stmts)

  method a_colon(l, obj, field) =
    let obj_exp, obj_other_stmts = destruct_exp @@ self#visit_value obj in
    CExp(rt_method "getColonFieldLoc" @@ of_list [
        obj_exp;
        JStr(field);
        self#get_loc(l);
      ], obj_other_stmts)

  method a_lam(l, args, ret, body) =
    let new_step = fresh_id @@ compiler_name "step" in
    let temp = fresh_id @@ compiler_name "temp_lam" in
    let len = List.length args in
    let effective_args =
      if len = 0 then
        args
      else
        [AstAnf.ABind(l, resumer, Ast.ABlank)] in
    CExp(rt_method "makeFunction" @@ of_list [JId(temp)],
         of_list [
           JVar(temp,
                JFun(map_list (formal_shadow_name ||> get_id) effective_args,
                     self#compile_fun_body l new_step temp effective_args (Some(len)) body true))
         ])

  method a_method(l, args, ret, body) =
    let step = fresh_id @@ compiler_name "step" in
    let temp_full = fresh_id @@ compiler_name "temp_full" in
    let len = List.length args in
    let full_var =
      JVar(temp_full,
           JFun(map_list (formal_shadow_name ||> get_id) args,
                self#compile_fun_body l step temp_full args (Some(len)) body true)) in
    let method_expr =
      let meth =
        if len < 9 then
          ("makeMethod" ^ (string_of_int (len - 1)))
        else
          "makeMethodN" in
      rt_method meth @@ of_list [JId(temp_full)] in
    CExp(method_expr, of_list [full_var])

  method a_val(l, v) =
    self#visit_value(v)

  method a_field(l, name, value) =
    let v_exp, v_other_stmts = destruct_exp @@ self#visit_value value in
    CField(JField(name, v_exp), v_other_stmts)

  method a_srcloc(l, loc) =
    CExp(self#get_loc(loc), ConcatEmpty)

  method a_num(l, n) =
    (* FIXME: Needs to be equivalent to the following:
       if num-is-fixnum(n):
         c-exp(j-parens(j-num(n)), cl-empty)
       else:
         c-exp(rt-method("makeNumberFromString", [clist: j-str(tostring(n))]), cl-empty)
    *)
    CExp(rt_method "makeNumberFromString" @@ of_list [JStr(BatNum.string_of_num n)], ConcatEmpty)

  method a_str(l, s) =
    CExp(JParens(JStr(s)), ConcatEmpty)

  method a_bool(l, b) =
    CExp(JParens(if b then JTrue else JFalse), ConcatEmpty)

  method a_undefined(l) =
    CExp(undefined, ConcatEmpty)

  method a_id(l, id) =
    CExp(JId(js_id_of id), ConcatEmpty)

  method a_id_var(l, id) =
    CExp(JDot(JId(js_id_of id), "$var"), ConcatEmpty)

  method a_id_letrec(l, id, safe) =
    let s = JId(js_id_of id) in
    let exp =
      if safe then
        JDot(s, "$var")
      else
        JTernary(JBinop(JDot(s, "$var"), JEq, undefined),
                 raise_id_exn (self#get_loc(l)) (Ast.name_toname id),
                 JDot(s, "$var")) in
    CExp(exp, ConcatEmpty)

  method a_data_expr(l, name, namet, variants, shared) =
    let open AstAnf in
    (* Need to shadow since members are not case_binds *)
    let get_bind = function
      | AVariantMember(_, _, bind) -> bind in
    (* TODO: Probably remove
       let brand_name base =
      Ast.name_toname @@ js_id_of @@ compiler_name @@ "brand-" ^ base in*)

    let visit_shared_fields = map_list (destruct_field ||> self#visit_field) shared in
    let shared_fields, shared_stmts = fold_right (fun (f, os) (fs, oss) ->
        ConcatCons(f, fs), (os |@> oss))
        visit_shared_fields (ConcatEmpty, ConcatEmpty) in
    let external_brand = JId(js_id_of namet) in

    (* TODO: Double check it's not used in a later commit and remove
       let make_brand_predicate (loc : Ast.loc) (b : JExpr.t) (pred_name : string) =
      let value = fresh_id @@ compiler_name "val" in
      JField(pred_name, rt_method "makeFunction" @@ of_list [
          JFun(of_list [value],
               JBlock(arity_check (self#get_loc(loc)) 1
                      |@> of_list [JReturn(rt_method "makeBoolean" @@ of_list [
                          rt_method "hasBrand" @@ of_list [JId(value); b]])]))]) in*)

    let make_variant_constructor l2 base_id brands_id members refl_name refl_ref_fields
        refl_ref_fields_mask refl_fields constructor_id =

      let nonblank_anns = members |> List.filter @@ fun m ->
          match get_bind m with
          | ABind(_, _, Ast.ABlank)
          | ABind(_, _, Ast.AAny) -> false
          | _ -> true in

      let compiled_anns_anns, compiled_anns_others =
        nonblank_anns |> List.fold_left (fun (anns, others) m ->
          match get_bind m with
          | ABind(_, _, ann) ->
            let compiled_exp, compiled_stmts = destruct_exp @@ compile_ann ann self in
            (ConcatSnoc(anns, compiled_exp),
             others |@> compiled_stmts))
          (ConcatEmpty, ConcatEmpty) in
      let compiled_locs, compiled_vals =
        List.fold_right (fun m (locs, vals) ->
          match get_bind m with
          | ABind(l, id, _) ->
            let loc = self#get_loc(l) in
            let value = JStr(Ast.name_tosourcestring @@ js_id_of id) in
            (ConcatCons(loc, locs), ConcatCons(value, vals)))
          nonblank_anns
          (ConcatEmpty, ConcatEmpty) in

      let member_types, member_ids =
        List.fold_right (fun m (typs, ids) ->
            match m with
            | AVariantMember(_, typ, ABind(_, id, _)) ->
              let is_mutable = j_bool (typ = AMutable) in
              let id = JStr(Ast.name_tosourcestring @@ js_id_of id) in
              (ConcatCons(is_mutable, typs), ConcatCons(id, ids)))
          members
          (ConcatEmpty, ConcatEmpty) in
      (* NOTE(joe 6-14-2014): We cannot currently statically check for if an annotation
         is a refinement because of type aliases.  So, we use checkAnnArgs, which takes
         a continuation and manages all of the stack safety of annotation checking itself.
    
         NOTE(joe 5-26-2015): This has been moved to a hybrid static/dynamic solution by
         passing the check off to a runtime function that uses JavaScript's Function
         to only do the refinement check once.*)
      CExp(
        rt_method "makeVariantConstructor" @@ of_list [
          self#get_loc(l2);
          JFun(ConcatEmpty, JBlock(of_list [JReturn(JList(false, compiled_anns_anns))]));
          JList(false, compiled_vals);
          JList(false, compiled_locs);
          JList(false, member_types);
          JList(false, member_ids);
          refl_ref_fields_mask;
          JId(base_id);
          JId(brands_id);
          refl_name;
          refl_ref_fields;
          refl_fields;
          constructor_id
        ], ConcatEmpty) in

    let compile_variant = function
      | (AVariant(loc, _, vname, _, with_members) as v)
      | (ASingletonVariant(loc, vname, with_members) as v) ->
        let variant_base_id = js_id_of @@ compiler_name @@ vname ^ "-base" in
        let variant_brand = rt_method "namedBrander" @@ of_list [JStr(vname); self#get_loc(l)] in
        let variant_brand_id = js_id_of @@ compiler_name @@ vname ^ "-brander" in
        let variant_brand_obj_id = js_id_of @@ compiler_name @@ vname ^ "-brands" in
        let variant_brands = JObj(ConcatEmpty) in
        let visit_with_fields = List.map (destruct_field ||> self#visit_field) with_members in

        let refl_base_fields =
          match v with
          | ASingletonVariant(_, _, _) -> ConcatEmpty
          | AVariant(_, _, _, members, _) ->
            of_list [
              JField("$fieldNames",
                     JList(false, map_list (fun m ->
                         match get_bind m with
                         | ABind(_, id, _) -> JStr(Ast.name_toname id)) members))] in

        let f_id = const_id "f" in
        let refl_name = JStr(vname) in
        let refl_ref_fields_id = js_id_of @@ compiler_name @@ vname ^ "_getfieldsref" in
        let refl_ref_fields =
          match v with
          | ASingletonVariant(_, _, _) ->
            JFun(ConcatSingleton(f_id), JBlock(ConcatSingleton(JReturn(JApp(JId(f_id), ConcatEmpty)))))
          | AVariant(_, _, _, members, _) ->
            let refmask_id = const_id "refmask" in
            let f_args = members |> map_list_n (fun n ->
                function
                | AVariantMember(_, typ, ABind(_, id, _)) ->
                  let n = num_of_int n in
                  let field = get_dict_field _THIS (JStr(Ast.name_toname id)) in
                  let mask = JBracket(JId(refmask_id), JNum(n)) in
                  let is_ref = j_bool (typ = AMutable) in
                  rt_method "derefField" @@ of_list [field; is_ref; mask]
              ) 0 in
            JFun(of_list [f_id; refmask_id], JBlock(ConcatSingleton(JReturn(JApp(JId(f_id), f_args))))) in

        let refl_ref_field_mask_id = js_id_of @@ compiler_name @@ vname ^ "_mutablemask" in
        let refl_ref_fields_mask =
          match v with
          | ASingletonVariant(_, _, _) -> JList(false, ConcatEmpty)
          | AVariant(_, _, _, members, _) ->
            let get_typ = function
              | AVariantMember(_, t, _) -> t in
            let is_ref x = (x = AMutable) in
            JList(false, map_list (j_bool ||> is_ref ||> get_typ) members) in

        let refl_fields_id = js_id_of @@ compiler_name @@ vname ^ "_getfields" in
        let refl_fields =
          match v with
          | ASingletonVariant(_, _, _) ->
            JFun(ConcatSingleton(const_id "f"),
                 JBlock(ConcatSingleton(JReturn(JApp(JId(f_id), ConcatEmpty)))))
          | AVariant(_, _, _, members, _) ->
            let f_args = members |> map_list @@ fun m ->
              get_dict_field _THIS @@ JStr(Ast.name_toname @@ get_id @@ get_bind m) in
            JFun(ConcatSingleton(const_id "f"),
                 JBlock(ConcatSingleton(JReturn(JApp(JId(f_id), f_args))))) in

        let member_count = function
          | ASingletonVariant(_, _, _) -> num_of_int 0
          | AVariant(_, _, _, members, _) -> num_of_int @@ List.length members in

        let match_field = JField("_match",
                                 rt_method "makeMatch" @@ of_list [refl_name; JNum(member_count v)]) in

        let stmts = List.fold_right (fun (_, os) acc -> os |@> acc) visit_with_fields
          @@ of_list [
            JVar(refl_fields_id, refl_fields);
            JVar(refl_ref_fields_id, refl_ref_fields);
            JVar(refl_ref_field_mask_id, refl_ref_fields_mask);
            JVar(variant_base_id,
                 JObj(refl_base_fields
                      |@> shared_fields
                      |@> (map_list fst visit_with_fields)
                      |@> ConcatSingleton(match_field)));
            JVar(variant_brand_id, variant_brand);
            JVar(variant_brand_obj_id, variant_brands);
            JSExpr(JBracketAssign(
                JId(variant_brand_obj_id),
                JDot(external_brand, "_brand"),
                JTrue));
            JSExpr(JBracketAssign(
                JId(variant_brand_obj_id),
                JDot(JId(variant_brand_id), "_brand"),
                JTrue))
          ] in

        let predicate = JField(Ast.make_checker_name vname,
                               get_field
                                 (JId(variant_brand_id))
                                 (JStr("test"))
                                 (self#get_loc(l))) in

        match v with
        | ASingletonVariant(_, _, with_members) ->
          let constructor =
            JField(vname, rt_method "makeDataValue" @@ of_list [
                JId(variant_base_id);
                JId(variant_brand_id);
                refl_name;
                JId(refl_ref_fields_id);
                JId(refl_fields_id);
                JNum(num_of_int (-1));
                JId(refl_ref_field_mask_id);
                JId(variant_base_id);
              ]) in
          (stmts, constructor, predicate)
        | AVariant(l2, constr_loc, _, members, with_members) ->
          let constr_vname = js_id_of @@ const_id vname in
          let compiled_constructor =
            make_variant_constructor
              constr_loc
              variant_base_id
              variant_brand_obj_id
              members
              refl_name
              (JId(refl_ref_fields_id))
              (JId(refl_ref_field_mask_id))
              (JId(refl_fields_id))
              (JId(variant_base_id)) in
          let exp, other_stmts = destruct_exp compiled_constructor in
          let stmts = stmts |@> other_stmts |@> (of_list [JVar(constr_vname, exp)]) in
          let constructor = JField(vname, JId(constr_vname)) in
          (stmts, constructor, predicate)
    in

    let variant_pieces = variants |> List.map compile_variant in

    let header_stmts, obj_fields =
      variant_pieces |> List.fold_left (fun (hdr_stmts, fields) (stmts, constructor, predicate) ->
          (hdr_stmts |@> stmts), (fields |@> (of_list [predicate; constructor])))
        (ConcatEmpty, ConcatEmpty) in
    let data_predicate = JField(name, get_field external_brand (JStr("test")) (self#get_loc(l))) in

    let data_object = rt_method "makeObject" @@ ConcatSingleton(JObj(ConcatCons(data_predicate, obj_fields))) in

    CExp(data_object, shared_stmts |@> header_stmts)

end

(* TODO: Why is this done here and not during some sort of desugaring? *)
class remove_useless_if_visitor = object(self)
  inherit AstAnf.default_map_visitor

  method a_if(l, c, t, e) =
    let open AstAnf in
    match c with
    | ABool(_, test) ->
      if test then
        begin
          let visit_t = self#visit_expr t in
          match visit_t with
          | ALettable(_, expr) -> expr
          | _ -> AIf(l, self#visit_value c, visit_t, self#visit_expr e)
        end
      else
        begin
          let visit_e = self#visit_expr e in
          match visit_e with
          | ALettable(_, expr) -> expr
          | _ -> AIf(l, self#visit_value c, self#visit_expr t, visit_e)
        end
    | _ -> AIf(l, self#visit_value c, self#visit_expr t, self#visit_expr e)

end
