module A = Ast
module SSet = Set.Make(String)

type sset = SSet.t

type desugar_env = DEnv of sset * sset * sset

type ('a, 'b) pair = Pair of 'a * 'b

let names = A.global_names
let mt_d_env = DEnv(SSet.empty, SSet.empty, SSet.empty)

let g id = A.SGlobal(id)
let gid l id = A.SId(l, g id)

let mk_id_ann loc base ann =
  let a = names base in
  (a, A.SBind(loc,false,a,ann), A.SId(loc,a))

let mk_id loc base = mk_id_ann loc base A.ABlank

let get_arith_op = function
  | "op+" -> Some("_plus")
  | "op-" -> Some("_minus")
  | "op*" -> Some("_times")
  | "op/" -> Some("_divide")
  | "op<" -> Some("_lessthan")
  | "op>" -> Some("_greaterthan")
  | "op>=" -> Some("_greaterequal")
  | "op<=" -> Some("_lessequal")
  | _ -> None

(* TODO: We'll use the Pyret Names for now, but we might consider renaming these prim-apps *)
let check_bool l e (cont : (A.expr -> 'a)) : 'a =
  cont(A.SPrimApp(l, "checkWrapBoolean", [e]))

let no_branches_exn l typ =
  A.SPrimApp(l, "throwNoBranchesMatched", [A.SSrcloc(l,l); A.SStr(l,typ)])

let bool_exn l typ v =
  A.SPrimApp(l, "throwNonBooleanCondition", [A.SSrcloc(l,l); A.SStr(l,typ); v])

let bool_op_exn l position typ v =
  A.SPrimApp(l, "throwNonBooleanOp", [A.SSrcloc(l,l); A.SStr(l,position); A.SStr(l,typ); v])

let is_underscore (e : A.expr) =
  match e with
  | A.SId(_,n) ->
    (match n with
     | A.SUnderscore(_) -> true
     | _ -> false)
  | _ -> false

let rec desugar_a_field = function
  | A.AField(l,name,ann) ->
    A.AField(l, name, desugar_ann ann)

and desugar_ann (a : A.ann) =
  match a with
  | A.ABlank  -> a
  | A.AAny  -> a
  | A.AName (_,_) -> a
  | A.ATypeVar (_,_) -> a
  | A.AArrow (l,args,ret,use_parens) ->
    A.AArrow (l,List.map desugar_ann args, desugar_ann ret, use_parens)
  | A.AMethod (l,args,ret) ->
    A.AMethod (l,List.map desugar_ann args, desugar_ann ret)
  | A.ARecord (l,fields) ->
    A.ARecord(l,List.map desugar_a_field fields)
  | A.AApp (l,base,args) ->
    A.AApp(l,desugar_ann base, List.map desugar_ann args)
  | A.APred (l,ann,exp) ->
    A.APred(l,desugar_ann ann, desugar_expr exp)
  | A.ADot (_,_,_) -> a
  | A.AChecked (_,_) -> failwith "AChecked should not appear before type-checking!"

and desugar (program : A.program) =
  match program with
  | A.SProgram(l,_provide,provided_types,imports,body) ->
    A.SProgram(l,_provide,provided_types,imports,desugar_expr body)

and desugar_if l branches _else =
  List.fold_left (fun acc branch ->
      let (loc, test, body) = match branch with
        | A.SIfBranch(l,t,b) -> (l,t,b) in
      check_bool loc (desugar_expr test) (fun test_id ->
          A.SIfElse(l, [A.SIfBranch(loc, test_id, desugar_expr body)], acc)))
    (desugar_expr _else) (List.rev branches)

and desugar_if_pipe l branches _else =
  let if_pipe_to_if = function
    | A.SIfPipeBranch(l,t,b) -> A.SIfBranch(l,t,b) in
  desugar_if l (List.map if_pipe_to_if branches) _else

and desugar_cases_bind = function
  | A.SCasesBind(l,typ,bind) -> A.SCasesBind(l, typ, desugar_bind bind)

and desugar_cases_branch = function
  | A.SCasesBranch(l,pat_loc,name,args,body) ->
    A.SCasesBranch(l,pat_loc,name,List.map desugar_cases_bind args, desugar_expr body)
  | A.SSingletonCasesBranch(l,pat_loc,name,body) ->
    A.SSingletonCasesBranch(l,pat_loc,name,desugar_expr body)

and desugar_variant_member = function
  | A.SVariantMember(l,typ,bind) -> A.SVariantMember(l,typ,desugar_bind bind)

and desugar_member e =
  match e with
  | A.SMethodField(l,name,params,args,ann,doc,body,_check) ->
    A.SDataField(l,name,desugar_expr (A.SMethod(l,params,args,ann,doc,body,_check)))
  | A.SDataField(l,name,value) ->
    A.SDataField(l,name,desugar_expr value)
  | _ -> failwith ("NYI(desugar-member): " ^ (PPrint.pretty_string (Ast.member_tosource e) 40))

and ds_curry_args l args =
  let params_and_args = List.fold_left (fun acc arg ->
      match acc with
      | Pair(left,right) ->
        if (is_underscore arg) then
          let (_,arg_id_b,arg_id_e) = mk_id l "arg_" in
          Pair(arg_id_b::left, arg_id_e::right)
        else
          Pair(left, arg::right)) (Pair([],[])) args in
  match params_and_args with
  | Pair(l,r) -> Pair(List.rev l, List.rev r)

and ds_curry_nullary : 'a. (A.loc * A.expr * 'a -> A.expr) -> A.loc -> A.expr -> 'a -> A.expr =
  fun rebuild_node l obj m ->
  if (is_underscore obj) then
    let (_,curried_obj_id_b,curried_obj_id_e) = mk_id l "recv_" in
    A.SLam(l, [], [curried_obj_id_b], A.ABlank, "", rebuild_node(l,curried_obj_id_e,m), None)
  else
    rebuild_node(l, desugar_expr obj, m)

and ds_curry_binop s e1 e2 rebuild =
  match (ds_curry_args s [e1;e2]) with
  | Pair(params, curry_args) ->
    match params with
    | [] -> rebuild e1 e2
    | f::r ->
      A.SLam(s,[],params,A.ABlank,"",
             rebuild (List.hd curry_args) (List.hd (List.tl curry_args)), None)

and ds_curry l f args =
  let fallthrough () =
    match (ds_curry_args l args) with
    | Pair(left,right) ->
      if (is_underscore f) then
        let (_,f_id_id_b,f_id_id_e) = mk_id l "f_" in
        A.SLam(l,[],f_id_id_b::left,A.ABlank,"",A.SApp(l,f_id_id_e,right),None)
      else
        let ds_f = desugar_expr f in
        match left with
        | [] -> A.SApp(l,ds_f,args)
        | _ -> A.SLam(l,[],left,A.ABlank,"",A.SApp(l,ds_f,right),None) in
  match f with
  | A.SDot(l2,obj,m) ->
    if (is_underscore obj) then
      let (_,curried_obj_id_b,curried_obj_id_e) = mk_id l "recv_" in
      match (ds_curry_args l args) with
      | Pair(params,right) ->
        A.SLam(l,[],curried_obj_id_b::params,A.ABlank,"",
               A.SApp(l,A.SDot(l,curried_obj_id_e,m),right),None)
    else
      fallthrough()
  | _ -> fallthrough()

and desugar_opt : 'a. ('a -> 'a) -> 'a option -> 'a option = fun f opt ->
  match opt with
  | None -> None
  | Some(e) -> Some(f e)

and desugar_bind = function
  | A.SBind(l,shadows,name,ann) ->
    A.SBind(l,shadows,name,desugar_ann ann)

and desugar_let_binds binds =
  List.map (function
      | A.SLetBind(l2,b,value) -> A.SLetBind(l2,desugar_bind b, desugar_expr value)
      | A.SVarBind(l2,b,value) -> A.SVarBind(l2,desugar_bind b, desugar_expr value)) binds

and desugar_letrec_binds binds =
  List.map (function
      | A.SLetrecBind(l2,b,value) -> A.SLetrecBind(l2,desugar_bind b, desugar_expr value)) binds

and desugar_expr (expr : A.expr) =
  match expr with
  | A.SModule (l,answer,dv,dt,provides,types,checks) ->
    A.SModule(l,desugar_expr answer, dv, dt, desugar_expr provides,
              List.map desugar_a_field types, desugar_expr checks)
  | A.STypeLetExpr (l,binds,body) ->
    let desugar_type_bind = function
      | A.STypeBind(l2,name,ann) -> A.STypeBind(l2,name,desugar_ann ann)
      | A.SNewtypeBind(l2,name,namet) -> A.SNewtypeBind(l2,name,namet) in
    A.STypeLetExpr(l,List.map desugar_type_bind binds, desugar_expr body)
  | A.SLetExpr (l,binds,body) ->
    let new_binds = desugar_let_binds binds in
    A.SLetExpr(l, new_binds, desugar_expr body)
  | A.SLetrec (l,binds,body) ->
    let new_binds = desugar_letrec_binds binds in
    A.SLetrec(l, new_binds, desugar_expr body)
  | A.SHintExp (_,_,_) -> failwith "NYI (desugar): SHintExp"
  | A.SInstantiate (l, inner_expr, params) ->
    A.SInstantiate(l, desugar_expr inner_expr, List.map desugar_ann params)
  | A.SBlock (l,stmts) -> A.SBlock(l,List.map desugar_expr stmts)
  | A.SUserBlock (_,body) -> desugar_expr body
  | A.SFun (_,_,_,_,_,_,_,_) -> failwith "NYI (desugar): SFun"
  | A.SType (l,name,ann) -> A.SType(l,name,desugar_ann ann)
  | A.SNewtype (l,name,namet) -> expr
  | A.SVar (_,_,_) -> failwith "SVar should have already been desugared" (* By desugar-scope *)
  | A.SRec (_,_,_) -> failwith "NYI (desugar): SRec"
  | A.SLet (_,_,_,_) -> failwith "SLet should have already been desugared" (* By desugar-scope *)
  | A.SRef (l,ann) -> A.SRef(l,desugar_opt desugar_ann ann)
  | A.SContract (_,_,_) -> failwith "NYI (desugar): SContract"
  | A.SWhen (l, test, body) ->
    check_bool l (desugar_expr test) (fun test_id_e ->
        (* If <test> <do body>, else do nothing *)
        A.SIfElse(l,[A.SIfBranch(l,test_id_e,
                                 A.SBlock(l,[desugar_expr body; gid l "nothing"]))],
                  A.SBlock(l,[gid l "nothing"])))
  | A.SAssign (l,id,value) -> A.SAssign(l,id,desugar_expr value)
  | A.SIfPipe (l,branches) -> desugar_if_pipe l branches (A.SBlock(l,[no_branches_exn l "ask"]))
  | A.SIfPipeElse (l,branches,_else) -> desugar_if_pipe l branches _else
  | A.SIf (l,branches) -> desugar_if l branches (A.SBlock(l,[no_branches_exn l "if"]))
  | A.SIfElse (l,branches,_else) -> desugar_if l branches _else
  | A.SCases (l,typ,value,branches) -> A.SCases(l,desugar_ann typ, desugar_expr value,
                                               List.map desugar_cases_branch branches)
  | A.SCasesElse (l,typ,value,branches,_else) -> A.SCasesElse(l,desugar_ann typ, desugar_expr value,
                                                              List.map desugar_cases_branch branches,
                                                              desugar_expr _else)
  | A.SOp (l,op,left,right) ->
    (match (get_arith_op op) with
     | Some(field) ->
       ds_curry_binop l (desugar_expr left) (desugar_expr right) (fun e1 e2 ->
           A.SApp(l,gid l field, [e1;e2]))
     | None ->
       let rec collect_op opname exp = match exp with
         | A.SOp(l2,op2,left2,right2) ->
           if opname == op2 then
             (collect_op opname left2) @ (collect_op opname right2)
           else
             [exp]
         | _ -> [exp] in
       let collect_ors = collect_op "opor"
       and collect_ands = collect_op "opand"
       and collect_carets = collect_op "op^" in
       let eq_op fun_name =
         ds_curry_binop l (desugar_expr left) (desugar_expr right)
           (fun e1 e2 -> A.SApp(l,gid l fun_name, [e1;e2])) in
       match op with
       | "op==" -> eq_op "equal-always"
       | "op=~" -> eq_op "equal-now"
       | "op<=>" -> eq_op "identical"
       | "op<>" ->
         ds_curry_binop l (desugar_expr left) (desugar_expr right)
           (fun e1 e2 -> A.SPrimApp(l,"not",[A.SApp(l,gid l "equal-always", [e1;e2])]))
       | "opor" ->
         let rec helper = function
           | f::[] -> check_bool l (desugar_expr f) (fun x -> x)
           | f::rst -> check_bool l (desugar_expr f) (fun or_oper ->
               A.SIfElse(l,[A.SIfBranch(l,or_oper,A.SBool(l,true))], helper rst))
           | [] -> failwith "opor helper called with empty list" in
         helper (collect_ors expr)
       | "opand" ->
         let rec helper = function
           | f::[] -> check_bool l (desugar_expr f) (fun x -> x)
           | f::rst -> check_bool l (desugar_expr f) (fun and_oper ->
               A.SIfElse(l,[A.SIfBranch(l,and_oper,helper rst)], A.SBool(l,true)))
           | [] -> failwith "opand helper called with empty list" in
         helper (collect_ands expr)
       | "op^" ->
         let operands = collect_carets expr in
         List.fold_left (fun acc f -> A.SApp(A.expr_loc f, (desugar_expr f), [acc]))
           (List.hd operands) (List.tl operands)
       | s -> failwith ("No implementation for " ^ op)
    )
  | A.SCheckTest (l,op,refinement,left,right) ->
    A.SCheckTest(l,op,desugar_opt desugar_expr refinement,
                 desugar_expr left, desugar_opt desugar_expr right)
  | A.SCheckExpr (_,_,_) -> failwith "NYI (desugar): SCheckExpr"
  | A.SParen (l,e) -> desugar_expr e
  | A.SLam (l,params,args,ann,doc,body,_check) ->
    A.SLam(l,params,List.map desugar_bind args, desugar_ann ann, doc, desugar_expr body,
           desugar_opt desugar_expr _check)
  | A.SMethod (l,params,args,ann,doc,body,_check) ->
    A.SMethod(l,params,List.map desugar_bind args, desugar_ann ann, doc, desugar_expr body,
           desugar_opt desugar_expr _check)
  | A.SExtend (l, obj, fields) -> ds_curry_nullary
                                    (fun (l, o, f) -> A.SExtend(l,o,f))
                                    l obj (List.map desugar_member fields)
  | A.SUpdate (l, obj, fields) -> ds_curry_nullary
                                    (fun (l, o, f) -> A.SUpdate(l,o,f))
                                    l obj (List.map desugar_member fields)
  | A.SObj (l,fields) -> A.SObj(l,List.map desugar_member fields)
  | A.SArray (_,_) -> failwith "NYI (desugar): SArray"
  | A.SConstruct (l,modifier,constructor,elts) ->
    let cloc = A.expr_loc constructor in
    (match modifier with
     | A.SConstructNormal ->
       let len = List.length elts in
       let desugared_elts = List.map desugar_expr elts in
       if len <= 5 then
         A.SApp(cloc, desugar_expr (A.SDot(cloc, constructor, "make" ^ (string_of_int len))),
                desugared_elts)
       else
         A.SApp(cloc, desugar_expr (A.SDot(cloc, constructor, "make")), [A.SArray(l,desugared_elts)])
     | A.SConstructLazy ->
       A.SApp(cloc, desugar_expr (A.SDot(cloc, constructor, "lazy-make")),
              List.map (fun elt -> desugar_expr (A.SLam(A.expr_loc elt, [],[],A.ABlank,"",elt,None)))
                elts))
  | A.SApp (l,f,args) -> ds_curry l f (List.map desugar_expr args)
  | A.SPrimApp (l,f,args) -> A.SPrimApp(l,f,List.map desugar_expr args)
  | A.SPrimVal (_,_) -> failwith "NYI (desugar): SPrimVal"
  | A.SId (_,_) -> expr
  | A.SIdVar (_,_) -> expr
  | A.SIdLetrec (_,_,_) -> expr
  | A.SUndefined _ -> failwith "NYI (desugar): SUndefined"
  | A.SSrcloc (_,_) -> failwith "NYI (desugar): SSrcloc"
  | A.SNum (_,_) -> expr
  | A.SFrac (_,_,_) -> expr
  | A.SBool (_,_) -> expr
  | A.SStr (_,_) -> expr
  | A.SDot (l,obj,field) -> ds_curry_nullary (fun (l,o,f) -> A.SDot(l,o,f)) l obj field
  | A.SGetBang (l,obj,field) -> ds_curry_nullary (fun (l,o,f) -> A.SGetBang(l,o,f)) l obj field
  | A.SBracket (_,_,_) -> failwith "NYI (desugar): SBracket"
  | A.SData (_,_,_,_,_,_,_) -> failwith "NYI (desugar): SData"
  | A.SDataExpr (l,name,namet,params,mixins,variants,shared,_check) ->
    let extend_variant = function
      | A.SVariant(l2,constr_loc,vname,members,with_members) ->
        A.SVariant(l2,constr_loc,vname,List.map desugar_variant_member members,
                   List.map desugar_member with_members)
      | A.SSingletonVariant(l2,vname,with_members) ->
        A.SSingletonVariant(l2,vname,List.map desugar_member with_members) in
    A.SDataExpr(l,name,namet,params,List.map desugar_expr mixins, List.map extend_variant variants,
                List.map desugar_member shared, desugar_opt desugar_expr _check)
  | A.SFor (l,iter,bindings,ann,body) ->
    let values = List.map desugar_expr (List.map (function | A.SForBind(_,_,v) -> v) bindings) in
    let the_function = A.SLam(l,[], List.map desugar_bind
                                (List.map (function | A.SForBind(_,b,_) -> b) bindings),
                              desugar_ann ann, "", desugar_expr body, None) in
    A.SApp(l,desugar_expr iter, the_function::values)
  | A.SCheck (l,name,body,keyword_check) -> A.SCheck(l,name,desugar_expr body, keyword_check)
