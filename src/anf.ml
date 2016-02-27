(** AST->ANF Conversion
  Direct port of Pyret ast-anf.arr.
  TODO: Clean up. Things are messy right now. *)
module A = Ast
module N = AstAnf
type loc = A.loc
let names = A.global_names

let bind l id = N.ABind(l, id, A.ABlank)

(** Creates a new name. Returns (id, id-b, id-e) *)
let mk_id loc base =
  let t = names base in
  (t, bind loc t, N.AId(loc, t))

let get_value = function
  | A.SDataField (_,_,v) -> v
  | A.SMutableField (_,_,_,v) -> v
  | A.SMethodField (_,_,_,_,_,_,_,_) -> failwith "Cannot get value of SMethodField"

let get_loc_name = function
  | A.SDataField(l,n,_) -> (l,n)
  | A.SMutableField(l,n,_,_) -> (l,n)
  | A.SMethodField(l,n,_,_,_,_,_,_) -> (l,n)

type anf_cont =
    KCont of (N.lettable -> N.expr)
  | KId of A.name

let apply cont (l : loc) (expr : N.lettable) =
  match cont with
  | KCont(f) -> f expr
  | KId(n) -> (match expr with
      | N.AVal(l2,v) ->
        N.ALettable(l,N.AApp(l,N.AId(l,n), [v]))
      | _ ->
        let (_, e_name_id_b, e_name_id_e) = mk_id l "cont_tail_arg" in
        N.ALet(l,e_name_id_b, expr,
               N.ALettable(l,N.AApp(l,N.AId(l,n),[e_name_id_e]))))

let rec anf_term (e : A.expr) : N.expr =
  anf e (KCont(fun x -> N.ALettable(N.lettable_loc x, x)))

and anf_bind b =
  match b with
  | A.SBind(l,shadows,id,ann) -> N.ABind(l,id,ann)

and anf_cases_bind cb =
  match cb with
  | A.SCasesBind(l,typ,b) -> N.ACasesBind(l,typ,anf_bind b)

and anf_cases_branch branch =
  match branch with
  | A.SCasesBranch(l,pat_loc,name,args,body) ->
    N.ACasesBranch(l,pat_loc,name,(List.map anf_cases_bind args), anf_term body)
  | A.SSingletonCasesBranch(l,pat_loc,name,body) ->
    N.ASingletonCasesBranch(l,pat_loc,name,anf_term body)

and anf_name expr name_hint (k : (N.value -> N.expr)) =
  anf expr (KCont(fun lettable ->
      match lettable with
      | N.AVal(_,v) -> k v
      | _ ->
        let (_,t_id_b,t_id_e) = mk_id (A.expr_loc expr) name_hint in
        N.ALet(A.expr_loc expr, t_id_b, lettable, k t_id_e)))

and anf_name_rec exprs name_hint (k : (N.value list -> N.expr)) =
  match exprs with
  | [] -> k []
  | f::r ->
    anf_name f name_hint
      (fun v -> anf_name_rec r name_hint (fun vs -> k ([v] @ vs)))

and anf_program e =
  match e with
  | A.SProgram(l,p,_,imports,block) ->
    (* Note: provides have been desugared into just names and Ann information *)
    N.AProgram(l,p,List.map anf_import imports, anf_term block)

and anf_import_type it =
  match it with
  | A.SFileImport(l,fname) -> N.AImportFile(l,fname)
  | A.SConstImport(l,m) -> N.AImportBuiltin(l,m)
  | A.SSpecialImport(l,kind,args) -> N.AImportSpecial(l,kind,args)

and anf_import i =
  match i with
  | A.SImportComplete(l,vals,types,f,val_name,types_name) ->
    let itype = (match f with
        | A.SFileImport(_,fname) -> N.AImportFile(l,fname)
        | A.SConstImport(_,m) -> N.AImportBuiltin(l,m)
        | A.SSpecialImport(_,kind,args) -> N.AImportSpecial(l,kind,args)) in
    N.AImportComplete(l,vals,types,itype,val_name,types_name)
  | A.SInclude(_,_) -> failwith "SInclude given to anf_import"
  | A.SImport(_,_,_) -> failwith "SImport given to anf_import"
  | A.SImportTypes(_,_,_,_) -> failwith "SImportTypes given to anf_import"
  | A.SImportFields(_,_,_) -> failwith "SImportFields given to anf_import"

and anf_block es_init k =
  let rec anf_block_help es =
    match es with
    | [] -> failwith "Empty block"
    | f::[] -> anf f k
    | f::r ->
      anf f (KCont (fun lettable ->
          N.ASeq(A.expr_loc f, lettable, anf_block_help r))) in
  anf_block_help es_init

and anf e k =
  (match e with
   | A.SModule(l,answer,dvs,dts,provides,types,checks) ->
     let advs = List.map (function
         | A.SDefinedValue(name,value) ->
           let aval = (match value with
               | A.SId(l,id) -> N.AId(l,id)
               | A.SIdVar(l,id) -> N.AIdVar(l,id)
               | A.SIdLetrec(l,id,safe) -> N.AIdLetrec(l,id,safe)
               | _ -> failwith "Got non-id in defined-value list") in
           N.ADefinedValue(name,aval)) dvs in
     let adts = List.map (function
         | A.SDefinedType(n,t) -> N.ADefinedType(n,t)) dts in
     anf_name answer "answer" (fun ans ->
         anf_name provides "provides" (fun provs ->
             anf_name checks "checks" (fun chks ->
                 apply k l (N.AModule(l,ans,advs,adts,provs,types,chks)))))
   | A.STypeLetExpr (l,binds,body) -> (match binds with
       | [] -> anf body k
       | f::r ->
         let new_bind = (match f with
             | A.STypeBind(l2,name,ann) -> N.ATypeBind(l2,name,ann)
             | A.SNewtypeBind(l2,name,namet) -> N.ANewtypeBind(l2,name,namet)) in
         N.ATypeLet(l,new_bind,anf (A.STypeLetExpr(l,r,body)) k))
   | A.SLetExpr (l,binds,body) ->
     let bind_id_ann = function
       | A.SBind(_,_,i,a) -> (i,a) in
     (match binds with
       | [] -> anf body k
       | f::r ->
         match f with
         | A.SVarBind(l2,b,value) ->
           let (i,a) = bind_id_ann b in
           (match a with
            | A.ABlank
            | A.AAny -> anf_name value "var" (fun new_val ->
                N.AVar(l2,N.ABind(l2,i,a), N.AVal(N.value_loc new_val, new_val),
                       anf (A.SLetExpr(l,r,body)) k))
            | _ -> let (_,vn_id_b,vn_id_e) = mk_id l2 "var" in
              anf value (KCont(fun lettable ->
                  N.ALet(l2,vn_id_b,lettable,
                         N.AVar(l2,N.ABind(l2,i,a), N.AVal(l2,vn_id_e),
                                anf (A.SLetExpr(l,r,body)) k)))))
         | A.SLetBind(l2,b,value) ->
           let (i,a) = bind_id_ann b in
           anf value (KCont(fun lettable ->
               N.ALet(l2,N.ABind(l2,i,a),lettable,
                      anf (A.SLetExpr(l,r,body)) k))))
   | A.SLetrec (l,binds,body) ->
     let bind_loc_val = function
       | A.SLetrecBind(l,b,v) -> (l,b,v) in
     let let_binds = List.map (fun b ->
         let (l,b,_) = bind_loc_val b in
         A.SVarBind(l,b,A.SUndefined(l))) binds in
     let assigns = List.map (fun b ->
         let (l,b,v) = bind_loc_val b in
         let i = (match b with | A.SBind(_,_,i,_) -> i) in
         A.SAssign(l,i,v)) binds in
     anf (A.SLetExpr(l,let_binds,A.SBlock(l, assigns @ [body]))) k
   | A.SHintExp (_,_,_) -> failwith "Missed case in anf: SHintExp"
   | A.SInstantiate (_,body,_) -> anf body k
   | A.SBlock (_,stmts) -> anf_block stmts k
   | A.SUserBlock (_,body) -> anf body k
   | A.SFun (_,_,_,_,_,_,_,_) -> failwith "Missed case in anf: SFun"
   | A.SType (_,_,_) -> failwith "Missed case in anf: SType"
   | A.SNewtype (_,_,_) -> failwith "Missed case in anf: SNewtype"
   | A.SVar (_,_,_) -> failwith "Missed case in anf: SVar"
   | A.SRec (_,_,_) -> failwith "SRec should be handled by anf_block"
   | A.SLet (_,_,_,_) -> failwith "SLet should be handled by anf_block"
   | A.SRef (l,ann) -> apply k l (N.ARef(l,ann))
   | A.SContract (_,_,_) -> failwith "Missed case in anf: SContract"
   | A.SWhen (_,_,_) -> failwith "Missed case in anf: SWhen"
   | A.SAssign (l,id,value) -> anf_name value "anf_assign" (fun v -> apply k l (N.AAssign(l,id,v)))
   | A.SIfPipe (_,_) -> failwith "Missed case in anf: SIfPipe"
   | A.SIfPipeElse (_,_,_) -> failwith "Missed case in anf: SIfPipeElse"
   | A.SIf (_,_) -> failwith "Missed case in anf: SIf"
   | A.SIfElse (l,branches,_else) ->
     let get_test_body = function
       | A.SIfBranch(_,t,b) -> (t,b) in
     let rec anf_if_branches k branches =
       match branches with
       | [] -> failwith "Empty branches"
       | f::r -> let (t,b) = get_test_body f in
         (match r with
          | [] -> anf_name t "anf_if"
                    (fun test -> apply k l (N.AIf(l,test,anf_term b, anf_term _else)))
          | f2::r2 ->
            anf_name t "anf_if"
              (fun test -> apply k l
                  (N.AIf(l,test,anf_term b,
                         anf_if_branches (KCont(fun if_expr -> N.ALettable(l,if_expr))) r)))) in
     anf_if_branches k branches
   | A.SCases (_,_,_,_) -> failwith "Missed case in anf: SCases"
   | A.SCasesElse (l,typ,value,branches,_else) ->
     anf_name value "cases_val"
       (fun v -> apply k l (N.ACases(l,typ,v,List.map anf_cases_branch branches, anf_term _else)))
   | A.SOp (_,_,_,_) -> failwith "Missed case in anf: SOp"
   | A.SCheckTest (_,_,_,_,_) -> failwith "Missed case in anf: SCheckTest"
   | A.SCheckExpr (l,expr,ann) ->
     let (id,_,_) = mk_id l "ann_check_temp" in
     let bindings = [A.SLetBind(l,A.SBind(l,false,id,ann),expr)] in
     anf (A.SLetExpr(l,bindings,A.SId(l,id))) k
   | A.SParen (_,_) -> failwith "Missed case in anf: SParen"
   | A.SLam (l,params,args,ret,doc,body,_) ->
     let anf_binding = function
       | A.SBind(l,_,n,a) -> N.ABind(l,n,a) in
     (match ret with
      | A.ABlank
      | A.AAny -> apply k l (N.ALam(l,List.map anf_binding args,ret,anf_term body))
      | _ ->
        let (id,_,_) = mk_id l "ann_check_temp" in
        apply k l (N.ALam(l,List.map anf_binding args, ret,
                          anf_term (A.SLetExpr(l,[A.SLetBind(l,A.SBind(l,false,id,ret),body)],
                                               A.SId(l,id))))))
   | A.SMethod (l,params,args,ret,doc,body,_) ->
     let anf_binding = function
       | A.SBind(l,_,n,a) -> N.ABind(l,n,a) in
     (match ret with
      | A.ABlank
      | A.AAny -> apply k l (N.AMethod(l,List.map anf_binding args,ret,anf_term body))
      | _ ->
        let (id,_,_) = mk_id l "ann_check_temp" in
        apply k l (N.AMethod(l,List.map anf_binding args, ret,
                          anf_term (A.SLetExpr(l,[A.SLetBind(l,A.SBind(l,false,id,ret),body)],
                                               A.SId(l,id))))))
   | A.SExtend (l,obj,fields) ->
     let exprs = List.map get_value fields in
     anf_name obj "anf_extend" (fun o ->
         anf_name_rec exprs "anf_obj" (fun ts ->
             let new_fields = List.map2 (fun f t ->
                 let (l,n) = get_loc_name f in
                 N.AField(l,n,t)) fields ts in
             apply k l (N.AExtend(l,o,new_fields))))
   | A.SUpdate (l,obj,fields) ->
     let exprs = List.map get_value fields in
     anf_name obj "anf_update" (fun o ->
         anf_name_rec exprs "anf_obj" (fun ts ->
             let new_fields = List.map2 (fun f t ->
                 let (l,n) = get_loc_name f in
                 N.AField(l,n,t)) fields ts in
             apply k l (N.AUpdate(l,o,new_fields))))
   | A.SObj (l,fields) ->
     let exprs = List.map get_value fields in
     anf_name_rec exprs "anf_obj" (fun ts ->
         let new_fields = List.map2 (fun f t ->
             let (l,n) = get_loc_name f in
             N.AField(l,n,t)) fields ts in
         apply k l (N.AObj(l,new_fields)))
   | A.SArray (l,values) -> anf_name_rec values "anf_array_val" (fun vs ->
       apply k l (N.AArray(l,vs)))
   | A.SConstruct (_,_,_,_) -> failwith "Missed case in anf: SConstruct"
   | A.SApp (l,f,args) -> (match f with
       | A.SDot(l2,obj,m) ->
         anf_name obj "anf_method_obj" (fun v ->
             anf_name_rec args "anf_arg" (fun vs ->
                 apply k l (N.AMethodApp(l,v,m,vs))))
       | _ -> anf_name f "anf_fun" (fun v ->
           anf_name_rec args "anf_arg" (fun vs ->
               apply k l (N.AApp(l,v,vs)))))
   | A.SPrimApp (l,f,args) -> anf_name_rec args "anf_arg" (fun vs ->
       apply k l (N.APrimApp(l,f,vs)))
   | A.SPrimVal (_,_) -> failwith "Missed case in anf: SPrimVal"
   | A.SId (l,id) -> apply k l (N.AVal(l,N.AId(l,id)))
   | A.SIdVar (l,id) -> apply k l (N.AVal(l,N.AIdVar(l,id)))
   | A.SIdLetrec (l,id,safe) -> apply k l (N.AVal(l,N.AIdLetrec(l,id,safe)))
   | A.SUndefined l -> apply k l (N.AVal(l,N.AUndefined(l)))
   | A.SSrcloc (l1,l2) -> apply k l1 (N.AVal(l1,N.ASrcloc(l1,l2)))
   | A.SNum (l,n) -> apply k l (N.AVal(l,N.ANum(l,n)))
   | A.SFrac (_,_,_) -> failwith "Rational Numbers are currently unsupported."
   | A.SBool (l,b) -> apply k l (N.AVal(l,N.ABool(l,b)))
   | A.SStr (l,s) -> apply k l (N.AVal(l,N.AStr(l,s)))
   | A.SDot (l,obj,field) -> anf_name obj "anf_bracket"
                               (fun t_obj -> apply k l (N.ADot(l,t_obj, field)))
   | A.SGetBang (l,obj,field) -> anf_name obj "anf_get_bang"
                                   (fun t -> apply k l (N.AGetBang(l,t,field)))
   | A.SBracket (l,obj,field) ->
     let fname = match field with
       | A.SStr(_,s) -> s
       | _ -> failwith "Non-string field: " ^
              (List.fold_right (^) (PPrint.pretty (A.expr_tosource field) 50) "") in
     anf_name obj "anf_bracket" (fun t_obj -> apply k l (N.ADot(l,t_obj,fname)))
   | A.SData (_,_,_,_,_,_,_) -> failwith "Missed case in anf: SData"
   | A.SDataExpr (l,data_name,data_name_t,params,mixins,variants,shared,_check) ->
     let anf_member = function
       | A.SVariantMember(l2,typ,b) ->
         let a_type = match typ with
           | A.SNormal -> N.ANormal
           | A.SMutable -> N.AMutable in
         let new_bind = match b with
           | A.SBind(l3,s,n,a) -> N.ABind(l3,n,a) in
         N.AVariantMember(l2,a_type,new_bind) in
     let anf_variant v kv = match v with
       | A.SVariant(l2,constr_loc,vname,members,with_members) ->
         let with_exprs = List.map get_value with_members in
         anf_name_rec with_exprs "anf_variant_member" (fun ts ->
             let new_fields = List.map2 (fun f t ->
                 let (l,n) = get_loc_name f in
                  N.AField(l,n,t)) with_members ts in
             kv (N.AVariant(l2,constr_loc,vname,List.map anf_member members,new_fields)))
       | A.SSingletonVariant(l2,name,with_members) ->
         let with_exprs = List.map get_value with_members in
         anf_name_rec with_exprs "anf_singleton_variant_member" (fun ts ->
             let new_fields = List.map2 (fun f t ->
                 let (l,n) = get_loc_name f in
                 N.AField(l,n,t)) with_members ts in
             kv (N.ASingletonVariant(l2,name,new_fields))) in
     let rec anf_variants vs ks =
       match vs with
       | [] -> ks []
       | f::r ->
         anf_variant f (fun v -> anf_variants r (fun rest_vs -> ks (v::rest_vs))) in
     let exprs = List.map get_value shared in
     anf_name_rec exprs "anf_shared" (fun ts ->
         let new_shared = List.map2 (fun f t ->
             let (l,n) = get_loc_name f in
             N.AField(l,n,t)) shared ts in
         anf_variants variants (fun new_variants ->
             apply k l (N.ADataExpr(l,data_name,data_name_t,new_variants,new_shared))))
   | A.SFor (_,_,_,_,_) -> failwith "Missed case in anf: SFor"
   | A.SCheck (_,_,_,_) -> failwith "Missed case in anf: SCheck")
