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
  acc

let binding_env_from_env env =
  let acc = MSD.create 20 in
  List.iter (fun name ->
      MSD.add acc (A.name_key (A.SGlobal(name))) (EBind(A.dummy_loc,false,BPrim(name))))
    (match env with
     | CompileStructs.CompileEnvironment.CompileEnvironment(globals,_) ->
       match globals with
       | CompileStructs.Globals.Globals(values,_) ->
         List.map fst (SD.bindings values));
  acc

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
    let (bound_env, bs) = PyretUtils.fold_while (fun (acc,bs) b ->
        let updated = bind_handlers.s_type_let_bind b acc.val_env acc.type_env in
        let visit_envs =
          new default_env_iter_visitor updated.val_env updated.type_env bind_handlers in
        let new_bind = visit_envs#visit_type_let_bind b in
        if new_bind then
          PyretUtils.Either.Left(updated, true)
        else
          PyretUtils.Either.Right(updated, false)) (new_envs, true) binds in
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
        let (env, type_env) = (MSD.copy env, MSD.copy type_env) in
        MSD.add env (Ast.name_key vals_name) (EBind(l, false, BUnknown));
        MSD.add type_env (Ast.name_key types_name) (EBind(l, false, BTyp));
        List.iter (fun v ->
            MSD.add env (Ast.name_key v) (EBind(l, false, BImport(import_type)))
        ) values;
        List.iter (fun t ->
            MSD.add type_env (Ast.name_key t) (EBind(l, false, BImport(import_type)))
        ) types;
        { val_env = env; type_env = type_env }
      | _ -> failwith ("Internal Error: incomplete import given to binding_handlers")
    )
}
