open JAst
open PyretUtils;;

type name_set = Ast.name MutableStringDict.t
type frozen_name_set = Ast.name StringDict.t

let ns_empty () = MutableStringDict.create 30

let rec difference s1 s2 =
  let s1_unfrozen_copy = StringDict.unfreeze s1 in
  remove_overlap_now s1_unfrozen_copy (StringDict.unfreeze s2);
  MutableStringDict.freeze s1_unfrozen_copy

and copy_nameset s =
  StringDict.unfreeze @@ MutableStringDict.freeze s

(* Does NOT mutate s1 or s2*)
and difference_now s1 s2 =
  let s1_copy = copy_nameset s1 in
  remove_overlap_now s1_copy s2;
  s1_copy

(* Mutates s1 *)
and remove_overlap_now s1 s2 =
  List.iter (fun (key,_) ->
      MutableStringDict.remove s1 key)
  @@ MutableStringDict.bindings s2

type graph_node = {
  _from : label;
  _to : label ConcatList.t;
  case_body : JCase.t;
  mutable free_vars : name_set;
  mutable used_vars : name_set;
  mutable decl_vars : name_set;
  mutable live_vars : name_set option;
  mutable live_after_vars : name_set option;
  mutable dead_vars : name_set option;
  mutable dead_after_vars : name_set option;
}

type case_results =
    CExp of JExpr.t * JStmt.t ConcatList.t
  | CField of JField.t * JStmt.t ConcatList.t
  | CBlock of JBlock.t * JCase.t ConcatList.t

type register_allocation =
    Results of JCase.t ConcatList.t * frozen_name_set

(* Begin function definitions *)

let rec used_vars_jblock = function
  | JBlock.JBlock(stmts) ->
    let acc = ns_empty() in
    ConcatList.iter (fun s ->
        MutableStringDict.merge acc @@ used_vars_jstmt s) stmts;
    acc

and used_vars_jstmt =
  let open JStmt in
  function
  | JVar(name, rhs) ->
    let ans = used_vars_jexpr rhs in
    MutableStringDict.remove ans (Ast.name_key name);
    ans
  | JIf1(cond, consq) ->
    let ans = used_vars_jexpr cond in
    MutableStringDict.merge ans @@ used_vars_jblock consq;
    ans
  | JIf(cond, consq, alt) ->
    let ans = used_vars_jexpr cond in
    MutableStringDict.merge ans @@ used_vars_jblock consq;
    MutableStringDict.merge ans @@ used_vars_jblock alt;
    ans
  | JTryCatch(body, exn, catch) ->
    let ns_catch = used_vars_jblock catch in
    MutableStringDict.remove ns_catch (Ast.name_key exn);
    let ans = used_vars_jblock body in
    MutableStringDict.merge ans ns_catch;
    ans
  | JThrow(expr)
  | JSExpr(expr)
  | JReturn(expr) -> used_vars_jexpr expr
  | JBreak
  | JContinue -> MutableStringDict.create 1
  | JSwitch(exp, branches) ->
    let acc = used_vars_jexpr exp in
    ConcatList.iter (fun b -> MutableStringDict.merge acc @@ used_vars_jcase b) branches;
    acc
  | JWhile(cond, body) ->
    let ans = used_vars_jexpr cond in
    MutableStringDict.merge ans @@ used_vars_jblock body;
    ans
  | JFor(create_var, init, cont, update, body) ->
    let ans = used_vars_jexpr init in
    MutableStringDict.merge ans @@ used_vars_jexpr cont;
    MutableStringDict.merge ans @@ used_vars_jexpr update;
    MutableStringDict.merge ans @@ used_vars_jblock body;
    (if create_var then
       match init with
       | JExpr.JAssign(name, _) -> MutableStringDict.remove ans (Ast.name_key name)
       | _ -> ());
    ans

and used_vars_jexpr =
  let open JExpr in
  function
  | JParens(exp)
  | JUnop(exp, _) -> used_vars_jexpr exp
  | JBinop(left, _, right) ->
    let ans = used_vars_jexpr left in
    MutableStringDict.merge ans @@ used_vars_jexpr right;
    ans
  | JFun(args, body) ->
    let acc = difference_now (used_vars_jblock body) (declared_vars_jblock body) in
    ConcatList.iter (fun a -> MutableStringDict.remove acc (Ast.name_key a)) args;
    acc
  | JNew(func, args)
  | JApp(func, args) ->
    let acc = used_vars_jexpr func in
    ConcatList.iter (fun a -> MutableStringDict.merge acc @@ used_vars_jexpr a) args;
    acc
  | JMethod(obj, _, args) ->
    let acc = used_vars_jexpr obj in
    ConcatList.iter (fun a -> MutableStringDict.merge acc @@ used_vars_jexpr a) args;
    acc
  | JTernary(test, consq, altern) ->
    let ans = used_vars_jexpr test in
    MutableStringDict.merge ans @@ used_vars_jexpr consq;
    MutableStringDict.merge ans @@ used_vars_jexpr altern;
    ans
  | JAssign(name, rhs) ->
    let ans = used_vars_jexpr rhs in
    MutableStringDict.add ans (Ast.name_key name) name;
    ans
  | JBracketAssign(obj, field, rhs) ->
    let ans = used_vars_jexpr obj in
    MutableStringDict.merge ans @@ used_vars_jexpr field;
    MutableStringDict.merge ans @@ used_vars_jexpr rhs;
    ans
  | JDotAssign(obj, _, rhs) ->
    let ans = used_vars_jexpr obj in
    MutableStringDict.merge ans @@ used_vars_jexpr rhs;
    ans
  | JDot(obj, _) -> used_vars_jexpr obj
  | JBracket(obj, field) ->
    let ans = used_vars_jexpr obj in
    MutableStringDict.merge ans @@ used_vars_jexpr field;
    ans
  | JList(_, elts) ->
    let acc = ns_empty() in
    ConcatList.iter (fun elt -> MutableStringDict.merge acc @@ used_vars_jexpr elt) elts;
    acc
  | JObj(fields) ->
    let acc = ns_empty() in
    ConcatList.iter (fun f -> MutableStringDict.merge acc @@ used_vars_jfield f) fields;
    acc
  | JId(id) -> MutableStringDict.of_list [(Ast.name_key id, id)]
  | JStr(_)
  | JNum(_)
  | JTrue
  | JFalse
  | JNull
  | JUndefined
  | JLabel(_) -> ns_empty()

and used_vars_jcase = function
  | JCase.JCase(exp, body) ->
    let ans = used_vars_jexpr exp in
    MutableStringDict.merge ans @@ used_vars_jblock body;
    ans
  | JCase.JDefault(body) -> used_vars_jblock body

and used_vars_jfield = function
  | JField.JField(_, value) -> used_vars_jexpr value

and declared_vars_jblock = function
  | JBlock.JBlock(stmts) ->
    let acc = ns_empty() in
    ConcatList.iter (fun s ->
        MutableStringDict.merge acc @@ declared_vars_jstmt s) stmts;
    acc

and declared_vars_jstmt =
  let open JStmt in
  function
  | JVar(name, _) -> MutableStringDict.of_list [(Ast.name_key name, name)]
  | JIf1(_, consq) -> declared_vars_jblock consq
  | JIf(_, consq, alt) ->
    let ans = declared_vars_jblock consq in
    MutableStringDict.merge ans @@ declared_vars_jblock alt;
    ans
  | JTryCatch(body, _, catch) ->
    let ans = declared_vars_jblock body in
    MutableStringDict.merge ans @@ declared_vars_jblock catch;
    ans
  | JSwitch(_, branches) ->
    let acc = MutableStringDict.create 15 in
    ConcatList.iter (fun b -> MutableStringDict.merge acc @@ declared_vars_jcase b) branches;
    acc
  | JWhile(_, body) ->
    declared_vars_jblock body
  | JFor(create_var, init, _, _, body) ->
    let ans = declared_vars_jblock body in
    (if create_var then
       match init with
       | JExpr.JAssign(name, _) -> MutableStringDict.add ans (Ast.name_key name) name
       | _ -> ());
    ans
  | JThrow(_)
  | JSExpr(_)
  | JBreak
  | JContinue
  | JReturn(_) -> MutableStringDict.create 1

and declared_vars_jcase = function
  | JCase.JCase(_, body)
  | JCase.JDefault(body) -> declared_vars_jblock body

let rec compute_live_vars (n : graph_node) (dag : graph_node StringDict.t) =
  match n.live_vars with
  | Some(live) ->
    live
  | None ->
    let live_after = copy_nameset n.free_vars in
    ConcatList.iter (fun follow ->
        let next_opt = StringDict.lookup (string_of_int @@ follow.get()) dag in
        match next_opt with
        | None -> ()
        | Some(next) ->
          let next_vars = compute_live_vars next dag in
          MutableStringDict.merge live_after next_vars) n._to;
    let decls = n.decl_vars in
    let live = difference_now live_after decls in
    let dead_after = difference_now decls live_after in
    let dead = difference_now dead_after n.used_vars in

    n.live_after_vars <- Some(live_after);
    n.live_vars <- Some(live);
    n.dead_after_vars <- Some(dead_after);
    n.dead_vars <- Some(dead);
    live

let rec find_steps_to stmts step =
  let looking_for = ref None in
  let (+) a b = ConcatList.ConcatAppend(a,b) in
  let foldfun stmt acc =
    let open JBlock in
    let open JStmt in
    match stmt with
    | JVar(name, JExpr.JObj(rhs_fields)) ->
      (match !looking_for with
       | Some(v) when v = name ->
         looking_for := None;
         ConcatList.fold_left (fun acc ->
             function
             | JField.JField(_, JExpr.JLabel(label)) ->
               ConcatList.ConcatSnoc(acc, label)
             | _ -> failwith "Invariant failure: Non-JLabel") acc rhs_fields
       | Some(_)
       | None -> acc)
    | JVar(_, _) -> acc
    | JIf1(cond, JBlock(stmts)) -> acc + (find_steps_to stmts step)
    | JIf(cond, JBlock(consq_stmts), JBlock(alt_stmts)) ->
      acc + (find_steps_to consq_stmts step) + (find_steps_to alt_stmts step)
    | JSExpr(JExpr.JAssign(name, JExpr.JLabel(label))) when name = step ->
      (* Simple assignment statement to $step *)
      ConcatList.ConcatSnoc(acc, label)
    | JSExpr(JExpr.JAssign(name, JExpr.JBinop(left, JBinop.JOr, right))) when name = step ->
      (* $step gets a cases dispatch *)
      (* ASSUMES that the dispatch table is assigned two statements before this one *)
      let get_obj = function
       | JExpr.JMethod(obj, _, _)
       | JExpr.JBracketAssign(obj, _, _)
       | JExpr.JDotAssign(obj, _, _)
       | JExpr.JDot(obj, _)
       | JExpr.JBracket(obj, _) -> obj
       | _ -> failwith "No obj field in expression" in
      let left_id = (match get_obj left with
          | JExpr.JId(id) -> id
          | _ -> failwith "Non-JId given on left-hand-side of binop") in
      looking_for := Some(left_id);
      (match right with
       | JExpr.JLabel(label) -> ConcatList.ConcatSnoc(acc, label)
       | _ -> failwith "Non-JLabel on right-hand-side of binop")
    | JSExpr(JExpr.JAssign(name, _)) when name = step -> failwith "Should not happen"
    | JSExpr(_)
    | JReturn(_)
    | JTryCatch(_, _, _)
    | JThrow(_)
    | JBreak
    | JContinue
    | JSwitch(_, _)
    | JWhile(_, _)
    | JFor(_, _, _, _, _) -> acc
  in
  ConcatList.fold_right foldfun stmts ConcatList.ConcatEmpty

let rec ignorable = function
  | JExpr.JApp(JExpr.JId(id), _) ->
    (Ast.name_toname id) = "G"
  | JExpr.JMethod(JExpr.JId(id), meth, _) ->
    ((Ast.name_toname id) = "R")
    && ((meth = "getFieldRef")
        || (meth = "getDotAnn"))
  | JExpr.JId(_) -> true
  | JExpr.JDot(obj, _) -> ignorable obj
  | JExpr.JBracket(obj, field) -> ignorable obj && ignorable field
  | _ -> false

let rec elim_dead_vars_jblock block dead_vars =
  let open JBlock in
  match block with
  | JBlock(stmts) ->
    JBlock(elim_dead_vars_jstmts stmts dead_vars)

and elim_dead_vars_jstmts stmts dead_vars =
  let open ConcatList in
  let open JStmt in
  let elim_block x = elim_dead_vars_jblock x dead_vars in
  let foldfun acc s =
    match s with
    | JVar(name, rhs) ->
      if StringDict.mem (Ast.name_key name) dead_vars then
        begin
          if ignorable rhs then
            acc
          else
            ConcatSnoc(acc, JSExpr(rhs))
        end
      else
        ConcatSnoc(acc, s)
    | JIf1(cond, consq) ->
      ConcatSnoc(acc, JIf1(cond, elim_block consq))
    | JIf(cond, consq, alt) ->
      ConcatSnoc(acc, JIf(cond, elim_block consq, elim_block alt))
    | JTryCatch(body, exn, catch) ->
      ConcatSnoc(acc, JTryCatch(elim_block body, exn, elim_block catch))
    | JSwitch(exp, branches) ->
      let new_switch_branches =
        map (fun b -> elim_dead_vars_jcase b dead_vars) branches in
      ConcatSnoc(acc, JSwitch(exp, new_switch_branches))
    | JWhile(cond, body) ->
      ConcatSnoc(acc, JWhile(cond, elim_block body))
    | JFor(create_var, init, cont, update, body) ->
      ConcatSnoc(acc, JFor(create_var, init, cont, update, elim_block body))
    | _ -> ConcatSnoc(acc, s)
  in
  fold_left foldfun ConcatEmpty stmts

and elim_dead_vars_jcase c dead_vars =
  let open JCase in
  let elim x = elim_dead_vars_jblock x dead_vars in
  match c with
  | JCase(exp, body) -> JCase(exp, elim body)
  | JDefault(body) -> JDefault(elim body)

let simplify body_cases step =
  let open ConcatList in
  let open JCase in
  let acc_dag = MutableStringDict.create 20 in
  iter (function
      | (JCase(JExpr.JLabel(label), JBlock.JBlock(stmts)) as body_case) ->
        MutableStringDict.add acc_dag
          (string_of_int @@ label.get())
          { _from = label;
            _to = find_steps_to stmts step;
            case_body = body_case;
            free_vars = ns_empty();
            used_vars = ns_empty();
            decl_vars = ns_empty();
            live_vars = None;
            live_after_vars = None;
            dead_vars = None;
            dead_after_vars = None}
      | _ -> ()) body_cases;
  let dag = MutableStringDict.freeze acc_dag in
  (*let labels = fold_right (fun body_case acc ->
      match body_case with
      | JCase(JExpr.JLabel(label), _) -> (label.get()) :: acc
      | _ -> acc) body_cases [] in*)
  let dag_binds = StringDict.bindings dag in
  List.iter (fun (lbl, n) ->
      n.decl_vars <- declared_vars_jcase n.case_body;
      n.used_vars <- used_vars_jcase n.case_body;
      n.free_vars <- difference_now n.used_vars n.decl_vars) dag_binds;
  List.iter (fun (lbl, n) -> let _ = compute_live_vars n dag in ()) dag_binds;
  let acc = ns_empty() in
  List.iter (fun (lbl, n) ->
      match n.dead_after_vars with
      | Some(v) -> MutableStringDict.merge acc v
      | _ -> ()) dag_binds;
  let discardable_vars = MutableStringDict.freeze acc in

  let dead_assignment_eliminated = map (function
      | (JCase(JExpr.JLabel(label), _) as body_case) ->
        let n = StringDict.find (string_of_int @@ label.get()) dag in
        (match n.dead_vars with
        | None -> body_case
        | Some(dead_vars) -> elim_dead_vars_jcase body_case (MutableStringDict.freeze dead_vars))
      | _ -> failwith "Impossible") body_cases in
  Results(dead_assignment_eliminated, discardable_vars)
