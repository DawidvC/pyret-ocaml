module A = Ast
module N = AstAnf
module SD = PyretUtils.StringDict
module MSD = PyretUtils.MutableStringDict

type uri = string
type loc = A.loc

type binding_info =
    BPrim of string (** Some "primitive" value supplied by the initial environment *)
  | BDict of binding_info SD.t (** Some module supplied by the initial environment *)
  | BExp of A.expr (** This name is bound to some expression that we can't interpret yet *)
  | BDot of binding_info * string (** A field lookup off some binding that isn't a BDict *)
  | BTyp (** A type *)
  | BImport of A.import_type (** Imported from a module *)
  | BUnknown (** Any unknown value *)

type binding = EBind of loc * bool * binding_info

let ok_last = function
  | A.SLet(_,_,_,_)
  | A.SVar(_,_,_)
  | A.SRec(_,_,_)
  | A.SFun(_,_,_,_,_,_,_,_)
  | A.SData(_,_,_,_,_,_,_)
  | A.SContract(_,_,_)
  | A.SCheck(_,_,_,_)
  | A.SType(_,_,_)
  | A.SNewtype(_,_,_) -> false
  | _ -> true

let print_error = Printf.fprintf stderr "%s"

let checkers l = A.SApp(l,A.SDot(l,A.SId(l,A.SName(l,"builtins")),"current-checker"),[])

let last lst = match (List.fold_left (fun _ x -> Some(x)) None lst) with
  | None -> failwith "Cannot take last of empty list"
  | Some(x) -> x

let append_nothing_if_necessary = function
  | A.SProgram(l1,_provide,_provide_types,imports,body) ->
    match body with
    | A.SBlock(l2,stmts) ->
      (match stmts with
       | [] -> Some(A.SProgram(l1,_provide,_provide_types,imports,
                               A.SBlock(l2,[A.SId(l2,A.SName(l2,"nothing"))])))
       | _ ->
         let last_stmt = last stmts in
         if (ok_last last_stmt) then None else
           Some(A.SProgram(l1,_provide,_provide_types,imports,
                           A.SBlock(l2,stmts @ [A.SId(A.dummy_loc, A.SName(l2,"nothing"))]))))
    | _ -> None

class flatten_single_blocks = object(self)
  inherit A.default_map_visitor

  method s_block(l,stmts) =
    match stmts with
    | f::[] -> self#visit_expr f
    | _ -> A.SBlock(l,List.map self#visit_expr stmts)

end

class merge_nested_blocks = object(self)
  inherit A.default_map_visitor

  method s_block(l,stmts) =
    let merged_stmts = List.fold_left (fun new_stmts s ->
        let visited = (self#visit_expr s) in
        match visited with
        | A.SBlock(l2,stmts2) -> (List.rev stmts2) @ new_stmts
        | _ -> visited::new_stmts) [] stmts in
    A.SBlock(l,List.rev merged_stmts)

end

let rec bind_exp e env =
  match e with
  | A.SDot(l,o,name) ->
    (match (bind_exp o env) with
     | Some(eb) ->
       (match eb with
        | EBind(loc,mut,info) ->
          match info with
          | BDict(dict) ->
            if SD.mem name dict then
              Some(EBind(A.dummy_loc, false, SD.find name dict))
            else Some(EBind(A.dummy_loc,false,BDot(info,name)))
          | _ -> Some(EBind(A.dummy_loc,false,BDot(info,name))))
     | None -> None)
  | A.SId(_,name)
  | A.SIdVar(_,name)
  | A.SIdLetrec(_,name,_)->
    if (SD.mem (A.name_key name) env) then Some(SD.find (A.name_key name) env)
    else None
  | _ -> Some(EBind(A.dummy_loc,false,BExp(e)))

let bind_or_unknown e env =
  match (bind_exp e env) with
  | None -> BUnknown
  | Some(bind) ->
    match bind with
    | EBind(_,_,info) -> info

let binding_type_env_from_env env =
  let acc = MSD.create 20 in
  List.iter (fun name ->
      MSD.add acc (A.name_key (A.STypeGlobal(name))) (EBind(A.dummy_loc,false,BTyp)))
    (match env with
     | CompileStructs.CompileEnvironment.CompileEnvironment(globals,_) ->
       match globals with
       | CompileStructs.Globals.Globals(_,types) ->
         List.map fst (SD.bindings types));
  MSD.freeze acc

let binding_env_from_env env =
  let acc = MSD.create 20 in
  List.iter (fun name ->
      MSD.add acc (A.name_key (A.SGlobal(name))) (EBind(A.dummy_loc,false,BPrim(name))))
    (match env with
     | CompileStructs.CompileEnvironment.CompileEnvironment(globals,_) ->
       match globals with
       | CompileStructs.Globals.Globals(values,_) ->
         List.map fst (SD.bindings values));
  MSD.freeze acc

type ('a, 'c) env_res = { val_env : 'a; type_env : 'c}

type ('a, 'c) bind_handlers = { s_letrec_bind : (Ast.letrec_bind -> 'a -> 'a);
                                s_let_bind : (Ast.let_bind -> 'a -> 'a);
                                s_bind : (Ast.bind -> 'a -> 'a);
                                s_header : (Ast.import -> 'a -> 'c -> ('a,'c) env_res);
                                s_type_let_bind : (Ast.type_let_bind -> 'a -> 'c -> ('a, 'c) env_res);
                                s_param_bind : (Ast.loc -> Ast.name -> 'c -> 'c) }

class ['a,'c] default_env_iter_visitor initial_env initial_type_env bind_handlers = object(self)
  inherit Ast.default_iter_visitor
  val env : 'a = initial_env
  val type_env : 'c = initial_type_env

  method get_env () = { val_env = env; type_env = type_env };

  method s_program(l,_provide,_provide_types,imports,body) =
    if (self#visit_provide _provide) && (self#visit_provide_types _provide_types) then
      begin
        let new_envs = self#get_env() in
        let imported_envs = List.fold_left (fun acc i ->
            bind_handlers.s_header i acc.val_env acc.type_env) new_envs imports in
        let new_visitor =
          new default_env_iter_visitor imported_envs.val_env imported_envs.type_env bind_handlers in
        List.for_all new_visitor#visit_import imports && new_visitor#visit_expr body
      end
    else
      false

  method s_type_let_expr(l,binds,body) =
    let new_envs = self#get_env() in
    let fold_fun (acc,bs) = fun b ->
      let updated = bind_handlers.s_type_let_bind b acc.val_env acc.type_env in
      let visit_envs =
        new default_env_iter_visitor updated.val_env updated.type_env bind_handlers in
      let new_bind = visit_envs#visit_type_let_bind b in
      if new_bind then
        PyretUtils.Either.Left(updated, true)
      else
        PyretUtils.Either.Right(updated, false) in
    let (bound_env, bs) = PyretUtils.fold_while fold_fun (new_envs, true) binds in
    let new_visitor =
      new default_env_iter_visitor bound_env.val_env bound_env.type_env bind_handlers in
    bs && new_visitor#visit_expr body

  method s_let_expr(l,binds,body) =
    let (bound_env,bs) = PyretUtils.fold_while (fun (e, bs) b ->
        let this_env = bind_handlers.s_let_bind b e in
        let visitor = new default_env_iter_visitor e type_env bind_handlers in
        let new_bind = visitor#visit_let_bind b in
        if new_bind then
          PyretUtils.Either.Left(this_env,true)
        else
          PyretUtils.Either.Right(this_env,false)) (env,true) binds in
    let new_visitor = new default_env_iter_visitor bound_env type_env bind_handlers in
    bs && new_visitor#visit_expr body

  method s_letrec(l,binds,body) =
    let bind_env = List.fold_left (fun acc b -> bind_handlers.s_letrec_bind b acc) env binds in
    let new_visitor = new default_env_iter_visitor bind_env type_env bind_handlers in
    let continue_binds = PyretUtils.fold_while (fun acc b ->
        if new_visitor#visit_letrec_bind b then
          PyretUtils.Either.Left(true)
        else
          PyretUtils.Either.Right(false)) true binds in
    continue_binds && new_visitor#visit_expr body

  method s_lam(l,params,args,ann,doc,body,_check) =
    let new_type_env = List.fold_left
        (fun acc param -> bind_handlers.s_param_bind l param acc) type_env params in
    let with_params = new default_env_iter_visitor env new_type_env bind_handlers in
    let visit_args = List.for_all with_params#visit_bind args in
    let with_params_env = (with_params#get_env()).val_env in
    let args_env = List.fold_left (fun acc arg -> bind_handlers.s_bind arg acc) with_params_env args in
    let with_args = new default_env_iter_visitor args_env new_type_env bind_handlers in
    visit_args
    && with_args#visit_ann ann
    && with_args#visit_expr body
    && with_args#visit_expr_option _check

  method s_cases_else(l,typ,value,branches,_else) =
    self#visit_ann typ
    && self#visit_expr value
    && List.for_all self#visit_cases_branch branches
    && self#visit_expr _else

  method s_cases_branch(l,pat_loc,name,args,body) =
    let visit_args = List.for_all self#visit_cases_bind args in
    let args_env = List.fold_left
        (fun acc arg -> bind_handlers.s_bind arg acc)
        env
        (List.map (function
             | Ast.SCasesBind(_,_,b) -> b) args) in
    let new_visitor = new default_env_iter_visitor args_env type_env bind_handlers in
    visit_args
    && new_visitor#visit_expr body

  method s_data_expr(l,name,namet,params,mixins,variants,shared_members,_check) =
    let new_type_env = List.fold_left
        (fun acc param -> bind_handlers.s_param_bind l param acc)
        type_env params in
    let with_params = new default_env_iter_visitor env new_type_env bind_handlers in
    with_params#visit_name namet
    && List.for_all with_params#visit_expr mixins
    && List.for_all with_params#visit_variant variants
    && List.for_all with_params#visit_member shared_members
    && with_params#visit_expr_option _check

  method s_method(l,params,args,ann,doc,body,_check) =
    let new_type_env = List.fold_left
        (fun acc param -> bind_handlers.s_param_bind l param acc) type_env params in
    let with_params = new default_env_iter_visitor env new_type_env bind_handlers in
    let args_env = List.fold_left
        (fun acc arg -> bind_handlers.s_bind arg acc) env args in
    let new_visitor = new default_env_iter_visitor args_env new_type_env bind_handlers in
    List.for_all with_params#visit_bind args
    && new_visitor#visit_ann ann
    && new_visitor#visit_expr body
    && new_visitor#visit_expr_option _check
end

let binding_handlers = {
  s_header = (fun imp env type_env ->
      match imp with
      | Ast.SImportComplete(l,values,types,import_type,vals_name,types_name) ->
        let (env, type_env) = (SD.unfreeze env, SD.unfreeze type_env) in
        MSD.add env (Ast.name_key vals_name) (EBind(l, false, BUnknown));
        MSD.add type_env (Ast.name_key types_name) (EBind(l, false, BTyp));
        List.iter (fun v ->
            MSD.add env (Ast.name_key v) (EBind(l, false, BImport(import_type)))
        ) values;
        List.iter (fun t ->
            MSD.add type_env (Ast.name_key t) (EBind(l, false, BImport(import_type)))
        ) types;
        { val_env = MSD.freeze env; type_env = MSD.freeze type_env }
      | _ -> failwith ("Internal Error: incomplete import given to binding_handlers")
    );
  s_param_bind = (fun l param type_env ->
      SD.add (Ast.name_key param) (EBind(l, false, BTyp)) type_env);
  s_type_let_bind = (fun tlb env type_env ->
      match tlb with
      | Ast.STypeBind(l,name,ann) ->
        {
          val_env = env;
          type_env = SD.add (Ast.name_key name) (EBind(l,false,BTyp)) type_env
        }
      | Ast.SNewtypeBind(l,tname,bname) ->
        {
          val_env = SD.add (Ast.name_key bname) (EBind(l,false,BUnknown)) env;
          type_env = SD.add (Ast.name_key tname) (EBind(l,false,BTyp)) type_env;
        }
    );
  s_let_bind = (fun lb env ->
      let bind_id = function
        | Ast.SBind(_,_,id,_) -> id in
      match lb with
      | Ast.SLetBind(l2,bind,value) -> SD.add (Ast.name_key (bind_id bind)) (EBind(l2,false,bind_or_unknown value env)) env
      | Ast.SVarBind(l2,bind,value) -> SD.add (Ast.name_key (bind_id bind)) (EBind(l2,false,BUnknown)) env);
  s_letrec_bind = (fun lrb env ->
      let bind_id = function
        | Ast.SBind(_,_,id,_) -> id in
      match lrb with
      | A.SLetrecBind(l2,bind,value) ->
        SD.add (Ast.name_key (bind_id bind)) (EBind(l2,false,bind_or_unknown value env)) env
    );
  s_bind = (fun b env ->
      match b with
      | Ast.SBind(l,_,id,_) ->
        SD.add (Ast.name_key id) (EBind(l,false,BUnknown)) env)
}


class binding_env_iter_visitor initial_env = object(self)
  inherit [binding SD.t, binding SD.t] default_env_iter_visitor
      (binding_env_from_env initial_env)
      (binding_type_env_from_env initial_env)
      binding_handlers
end

class ['a,'c] link_list_visitor initial_env = object(self)
end


class bad_assignments_visitor initial_env = object(self)
  inherit binding_env_iter_visitor initial_env
  val errors : CompileStructs.CompileError.t list ref = ref [];

  method add_error err = errors := err :: !errors
  method get_errors () = !errors

  method s_assign(loc,id,value) =
    (match (bind_exp (Ast.SId(loc,id)) env) with
    | None -> ();
    | Some(b) ->
      let (mut, bloc) = (match b with | EBind(l,b,_) -> b,l) in
      if (not mut) then
        self#add_error (CompileStructs.CompileError.BadAssignment(Ast.name_toname id, loc, bloc)););
    self#visit_expr value
end

let bad_assignments initial_env ast =
  let visitor = new bad_assignments_visitor initial_env in
  let _ = visitor#visit_program ast in
  visitor#get_errors()

class inline_lams = object(self)
  inherit Ast.default_map_visitor

  method s_app(loc,f,exps) =
    match f with
    | Ast.SLam(l,_,args,ann,_,body,_) when (List.length args) = (List.length exps) ->
      let a = Ast.global_names "inline_body" in
      let let_binds = List.map2 (fun arg exp ->
          let argloc = match arg with
            | Ast.SBind(l,_,_,_) -> l in
          A.SLetBind(argloc,arg,self#visit_expr exp)) args exps in
      (match ann with
       | A.ABlank
       | A.AAny -> A.SLetExpr(l,let_binds, self#visit_expr body)
       | _ -> A.SLetExpr(l, let_binds @ [A.SLetBind(Ast.expr_loc body, Ast.SBind(l, false, a, ann), self#visit_expr body)], A.SId(l, a)))
    | _ -> Ast.SApp(loc, self#visit_expr f, List.map self#visit_expr exps)

end

class check_unbound_class initial_env = object(self)
  inherit binding_env_iter_visitor initial_env

  val errors : CompileStructs.CompileError.t list ref = ref []
  method add_error err = errors := err :: !errors
  method get_errors () = !errors

  method handle_id this_id =
    match this_id with
    | Ast.SId(_,id)
    | Ast.SIdVar(_,id)
    | Ast.SIdLetrec(_,id,_) ->
      (match id with
       | Ast.SUnderscore(l) -> self#add_error (CompileStructs.CompileError.UnderscoreAsExpr(l))
       | _ ->
         match (bind_exp this_id env) with
          | None -> self#add_error (CompileStructs.CompileError.UnboundId(this_id))
          | Some(_) -> ())
    | _ -> failwith "non-id given to handle_id"

  method handle_type_id ann =
    match ann with
    | Ast.AName(_,id) ->
      (match id with
       | Ast.SUnderscore(l) -> self#add_error (CompileStructs.CompileError.UnderscoreAsAnn(l))
       | _ ->
         if (not (SD.mem (Ast.name_key id) env)) then
           self#add_error (CompileStructs.CompileError.UnboundTypeId(ann))
         else ())
    | _ -> failwith "non-id given to handle_type_id"

  method s_id(loc,id) =
    self#handle_id (A.SId(loc,id));
    true

  method s_id_var(loc,id) =
    self#handle_id (A.SIdVar(loc,id));
    true

  method s_id_letrec(loc,id,safe) =
    self#handle_id (A.SIdLetrec(loc,id,safe));
    true

  method s_assign(loc,id,value) =
    (match bind_exp (A.SId(loc,id)) env with
     | None -> self#add_error (CompileStructs.CompileError.UnboundVar(Ast.name_toname id, loc))
     | _ -> ());
    self#visit_expr value

  method a_name(loc,id) =
    self#handle_type_id (Ast.AName(loc,id));
    true

  method a_dot(loc,name,field) =
    self#handle_type_id (Ast.AName(loc,name));
    true
end

let check_unbound initial_env ast =
  let visitor = new check_unbound_class initial_env in
  let _ = visitor#visit_program ast in
  visitor#get_errors()

let value_delays_exec_of name = function
  | A.SLam(_,_,_,_,_,_,_)
  | A.SMethod(_,_,_,_,_,_,_) -> true
  | _ -> false

class mk_letrec_visitor env = object(self)
  inherit Ast.default_map_visitor

  val env : bool SD.t = env

  method s_letrec(l,binds,body) =
    let lrb_bind = function
      | Ast.SLetrecBind(_,b,_) -> b in
    let bind_name = function
      | Ast.SBind(_,_,i,_) -> i in
    let bind_envs = List.mapi (fun i -> function
        | Ast.SLetrecBind(_,b,value) ->
          match b with
          | Ast.SBind(loc,_,id,_) ->
            let rhs_is_delayed = value_delays_exec_of id value in
            let acc = SD.unfreeze env in
            List.iteri (fun j -> function
                | Ast.SLetrecBind(_,b,_) ->
                  match b with
                  | Ast.SBind(b2loc,_,b2id,b2value) ->
                    let key = Ast.name_key b2id in
                    if (i < j) then
                      MSD.add acc key false
                    else if i = j then
                      MSD.add acc key rhs_is_delayed
                    else
                      MSD.add acc key true) binds;
            MSD.freeze acc
      ) binds in
    let new_binds = List.map2 (fun b bind_env ->
        let visitor = new mk_letrec_visitor bind_env in
        visitor#visit_letrec_bind b) binds bind_envs in
    let body_env = SD.add (Ast.name_key (bind_name (lrb_bind (PyretUtils.last binds))))
        true (PyretUtils.last bind_envs) in
    let body_visitor = new mk_letrec_visitor body_env in
    let new_body = body_visitor#visit_expr body in
    A.SLetrec(l,new_binds,new_body)

  method s_id_letrec(l,id,_) =
    A.SIdLetrec(l,id,SD.find (Ast.name_key id) env)

end

class letrec_visitor = object inherit mk_letrec_visitor SD.empty end

class renamer replacements = object(self)
  inherit Ast.default_map_visitor

  method s_atom(base,serial) =
    let a = Ast.SAtom(base,serial) in
    let k = Ast.name_key a in
    if SD.mem k replacements then
      SD.find k replacements
    else
      a
end

let make_renamer = new renamer

let wrap_extra_imports p env =
  match p with
  | Ast.SProgram(l,prov,prov_t,prog_imports,expr) ->
    match env with
    | CompileStructs.ExtraImports.ExtraImports(imports) ->
      let full_imports = prog_imports @ (List.map (function
          | CompileStructs.ExtraImport.ExtraImport(dependency,as_name,values,types) ->
            let s_name s = Ast.SName(l,s) in
            match dependency with
            | CompileStructs.Dependency.Builtin(name) ->
              Ast.SImportComplete(l, List.map s_name values, List.map s_name types,
                                  Ast.SConstImport(l,name), s_name as_name, s_name as_name)
            | CompileStructs.Dependency.Dependency(protocol,args) ->
              Ast.SImportComplete(l, List.map s_name values, List.map s_name types,
                                  Ast.SSpecialImport(l,protocol,args), s_name as_name, s_name as_name)) imports) in
      Ast.SProgram(l,prov,prov_t,full_imports,expr)

let import_to_dep i =
  let open CompileStructs.Dependency in
  let open Ast in
  match i with
  | SFileImport(_,path) -> Dependency("legacy path", [path])
  | SConstImport(_,modname) -> Builtin(modname)
  | SSpecialImport(_,protocol,args) -> Dependency(protocol,args)

let import_to_dep_anf i =
  let open CompileStructs.Dependency in
  let open AstAnf in
  match i with
  | AImportBuiltin(_,name) -> Builtin(name)
  | AImportFile(_,name) -> Dependency("legacy-path", [name])
  | AImportSpecial(_,kind,args) -> Dependency(kind,args)

let some_pred : 'a. ('a -> bool) -> 'a option -> 'a = fun pred -> function
  | None -> raise (Invalid_argument("Expected some but got none"))
  | Some(exp) ->
    if not (pred exp) then
      failwith ("Predicate failed for Some(exp)")
    else
      exp
