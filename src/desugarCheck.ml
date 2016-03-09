module A = Ast
module G = Gensym
module SL = Ast.Srcloc
module U = AstUtils

module CheckInfo = struct
  type t = CheckInfo of SL.t * string * A.expr
end

let ast_pretty ast =
  A.SStr(A.expr_loc ast, A.expr_to_string ast)

let ast_lam ast =
  A.SLam(A.expr_loc ast, [], [], A.ABlank, "", ast, None)

let ast_srcloc l =
  A.SPrimApp(l, "makeSrcloc", [A.SSrcloc(l,l)])

class check_stmts_visitor = object(self)
  inherit A.default_map_visitor

  method s_check_test(l,op,refinement,left,right) =
    let term = A.SCheckTest(l,op,refinement,left,right) in
    let check_op fieldname =
      match right with
      | None -> failwith "check_op called when right is None"
      | Some(right) ->
        A.SApp(l,A.SDot(l, U.checkers l, fieldname),
               [ast_pretty term; ast_lam left; ast_lam right; ast_srcloc l]) in
    let check_refinement refinement fieldname =
      match right with
      | None -> failwith "check_refinement called when right is None"
      | Some(right) ->
        A.SApp(l,A.SDot(l,U.checkers l, fieldname),
               [ast_pretty term; refinement; ast_lam left; ast_lam right; ast_srcloc l]) in
    match op with
    | A.SOpIs ->
      (match refinement with
       | None -> check_op "check-is"
       | Some(refinement) -> check_refinement refinement "check-is-refinement")
    | A.SOpIsNot ->
      (match refinement with
       | None -> check_op "check-is-not"
       | Some(refinement) -> check_refinement refinement "check-is-not-refinement")
    | A.SOpIsOp(opname) ->
      check_refinement (A.SId(l, A.SName(l, A.get_op_fun_name opname))) "check-is-refinement"
    | A.SOpIsNotOp(opname) ->
      check_refinement (A.SId(l, A.SName(l, A.get_op_fun_name opname))) "check-is-not-refinement"
    | A.SOpSatisfies -> check_op "check-satisfies-delayed"
    | A.SOpSatisfiesNot -> check_op "check-satisfies-not-delayed"
    | A.SOpRaises ->
      (match right with
       | None -> failwith "SOpRaises with None for right"
       | Some(right) ->
         A.SApp(l, A.SDot(l, U.checkers l, "check-raises-str"),
                  [ast_pretty term; ast_lam left; right; ast_srcloc l]))
    | A.SOpRaisesNot ->
      A.SApp(l,A.SDot(l,U.checkers l, "check-raises-not"),
             [ast_pretty term; ast_lam left; ast_srcloc l])
    | A.SOpRaisesOther ->
      (match right with
       | None -> failwith "SOpRaisesOther with None for right"
       | Some(right) ->
         A.SApp(l, A.SDot(l, U.checkers l, "check-raises-other-str"),
                [ast_pretty term; ast_lam left; right; ast_srcloc l]))
    | A.SOpRaisesSatisfies ->
      (match right with
       | None -> failwith "SOpRaisesSatisfies with None for right"
       | Some(right) ->
         A.SApp(l, A.SDot(l, U.checkers l, "check-raises-satisfies"),
                [ast_pretty term; ast_lam left; right; ast_srcloc l]))
    | A.SOpRaisesViolates ->
      (match right with
       | None -> failwith "SOpRaisesViolates with None for right"
       | Some(right) ->
         A.SApp(l, A.SDot(l, U.checkers l, "check-raises-violates"),
                [ast_pretty term; ast_lam left; right; ast_srcloc l]))

  method s_check(_,_,body,_) = self#visit_expr body
end

let csv = new check_stmts_visitor

let get_checks stmts =
  let standalone_counter = ref 0 in
  let add_check stmt lst =
    match stmt with
    | A.SFun(l,name,_,_,_,_,_,_check) ->
      (match _check with
       | Some(v) -> (CheckInfo.CheckInfo(l, name, (csv#visit_expr v))) :: lst
       | None -> lst)
    | A.SData(l,name,_,_,_,_,_check) ->
      (match _check with
       | Some(v) -> (CheckInfo.CheckInfo(l, name, (csv#visit_expr v))) :: lst
       | None -> lst)
    | A.SCheck(l,name,body,keyword_check) ->
      let check_name = match name with
        | None ->
          standalone_counter := !standalone_counter + 1;
          "check-block-" ^ (string_of_int !standalone_counter)
        | Some(v) -> v in
      (CheckInfo.CheckInfo(l,check_name,csv#visit_expr body)) :: lst
    | _ -> lst in
  List.fold_right add_check stmts []

let make_lam l args body =
  A.SLam(l,[],List.map (fun sym -> A.SBind(l,false,sym,A.ABlank)) args, A.ABlank, "", body, None)

let create_check_block l checks =
  let create_checker = function
    | CheckInfo.CheckInfo(l2,name,body) ->
      let check_fun = make_lam l2 [] body in
      A.SObj(l2,[
          A.SDataField(l2,"name",A.SStr(l2, name));
          A.SDataField(l2,"run",check_fun);
          A.SDataField(l2,"location",A.SPrimApp(l2,"makeSrcloc",[A.SSrcloc(l2,l2)]))
        ]) in
  let checkers = List.map create_checker checks in
  A.SBlock(l, [
      A.SApp(l, A.SDot(l, U.checkers l, "run-checkers"), [
          A.SStr(l, (match l with
              | SL.Srcloc(s,_,_,_,_,_,_)
              | SL.Builtin(s) -> s));
          A.SConstruct(l, A.SConstructNormal, A.SId(l , A.SName(l, "list")), checkers)
        ])
    ])

class no_checks_visitor = object(self)
  inherit A.default_map_visitor

  method s_block(l,stmts) =
    A.SBlock(l, List.map self#visit_expr stmts)
  method s_fun(l,name,params,args,ann,doc,body,_) =
    A.SFun(l,name,params,args,ann,doc,body,None)
  method s_data(l,name,params,mixins,variants,shared_members,_) =
    A.SData(l,name,params,mixins,variants,shared_members,None)
  method s_lam(l,params,args,ann,doc,body,_) =
    A.SLam(l,params,args,ann,doc,body,None)
  method s_check(l,name,body,keyword_check) =
    A.SId(l,A.SName(l,"nothing"))

end

class check_visitor = object(self)
  inherit A.default_map_visitor

  method s_block(l,stmts) =
    let checks_to_perform = get_checks stmts in
    let ds_stmts = List.map self#visit_expr stmts in
    let do_checks = create_check_block l checks_to_perform in
    match (checks_to_perform, ds_stmts) with
    | ([],_) -> A.SBlock(l,ds_stmts)
    | (_,[]) -> failwith "Empty block"
    | _ ->
      let rec take n = function
        | [] -> if (n = 0) then [] else failwith "Cannot take from empty list"
        | fst :: rst -> if (n = 0) then [] else fst :: (take (n - 1) rst) in
      let id_result = A.SName(l, G.make_name "result-after-checks") in
      let last_expr = List.fold_left (fun _ x -> x) (List.hd ds_stmts) (List.tl ds_stmts) in
      A.SBlock(
        l,
        (take ((List.length ds_stmts) - 1) ds_stmts)
        @ [A.SLet(l,A.SBind(l,false,id_result,A.ABlank),last_expr,false);
           do_checks;
           A.SId(l,id_result)]
      )

end
let ncv = new no_checks_visitor
let cv = new check_visitor

(**
Desugars all check blocks to be calls to the current checker
Preconditions on prog:
  - well-formed
Postconditions on prog:
  - contains no s-check or s-check-test statements
  - all where blocks on s-lam, s-fun, s-data, s-method are none
*)
let desugar_check = cv#visit_program

let desugar_no_checks = ncv#visit_program
