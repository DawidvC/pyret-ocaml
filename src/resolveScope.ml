module A = Ast
module SD = PyretUtils.StringDict
module U = AstUtils
module G = Gensym
module T = TypeStructs

open CompileStructs

type binding_group =
  | LetBinds of Ast.let_bind list
  | LetrecBinds of Ast.letrec_bind list
  | TypeLetBinds of Ast.type_let_bind list

let mk_bind l id = A.SBind(l,false,id,A.ABlank)

type mk_id_res = {id : A.name; id_b : A.bind; id_e : A.expr }
let mk_id loc base : mk_id_res =
  let t = A.SName(loc,base) in
  {id = t; id_b = mk_bind loc t; id_e = A.SId(loc,t)}

let resolve_provide (p : A.provide) (b : A.expr) =
  match p with
  | A.SProvideAll(l) ->
    let ids = A.block_ids b in
    let obj = A.SObj(l, List.map
                       (fun id -> A.SDataField(l, A.name_tosourcestring id, A.SId(l,id))) ids) in
    A.SProvide(l, obj)
  | _ -> p

let resolve_type_provide (p : A.provide_types) (b : A.expr) =
  match p with
  | A.SProvideTypesAll(l) ->
    let ids = A.block_type_ids b in
    let type_fields = List.map (function
        | A.TypeID(_,name) -> A.AField(l, A.name_toname name, A.AName(l, name))
      ) ids in
    A.SProvideTypes(l, type_fields)
  | _ -> p

let expand_import (imp : A.import) (env : CompileEnvironment.t) : A.import =
  match imp with
  | A.SImport(l,imp,name) ->
    A.SImportComplete(l,[],[],imp,name,name)
  | A.SImportFields(l,fields,imp) ->
    A.SImportComplete(l,fields,[],imp,A.SUnderscore(l),A.SUnderscore(l))
  | A.SInclude(l,imp) ->
    let imp_name = A.SUnderscore(l) in
    let info_key = Dependency.key (U.import_to_dep imp) in
    let safe_find sd key =
      try
        Some(SD.find sd key)
      with
      | Invalid_argument(_) -> None in
    let mod_info =
      safe_find info_key (match env with | CompileEnvironment.CompileEnvironment(_,e) -> e) in
    (match mod_info with
    | None -> failwith ("No compile-time information provided for module "^info_key)
    | Some(provides) ->
      match provides with
      | Provides.Provides(_,values,aliases,_) ->
        let val_names = List.map (fun n -> A.SName(l,fst n)) (SD.bindings values) in
        let type_names = List.map (fun n -> A.SName(l, fst n)) (SD.bindings aliases) in
        A.SImportComplete(l,val_names,type_names,imp,imp_name,imp_name))
  | A.SImportComplete(_,_,_,_,_,_) -> imp
  | _ -> failwith "NYI: SImportTypes"

let desugar_toplevel_types stmts =
  let rev_type_binds = ref [] in
  let rev_stmts = ref [] in
  List.iter (function
      | (Ast.SType(l, name, ann) as s) ->
        rev_stmts := s :: !rev_stmts
      | Ast.SNewtype(l, name, namet) ->
        rev_type_binds := (A.SNewtypeBind(l, name, namet)) :: !rev_type_binds
      | Ast.SData(l, name, params, mixins, variants, shared, _check) ->
        let namet = Ast.global_names name in
        rev_type_binds := (A.SNewtypeBind(l, A.SName(l, name), namet)) :: !rev_type_binds;
        rev_stmts := (A.SDataExpr(l, name, namet, params, mixins, variants, shared, _check)) :: !rev_stmts
      | (_ as s) -> rev_stmts := s :: !rev_stmts) stmts;
  match List.rev !rev_type_binds with
  | [] -> stmts
  | ((Ast.STypeBind(l,_,_)) :: _ as type_binds)
  | ((Ast.SNewtypeBind(l,_,_)) :: _ as type_binds) ->
    let new_stmts = List.rev !rev_stmts in
    [A.STypeLetExpr(l, type_binds, A.SBlock(l, new_stmts))]

let bind_wrap bg expr =
  match bg with
  | LetBinds([])
  | LetrecBinds([])
  | TypeLetBinds([]) -> expr
  | LetBinds((Ast.SLetBind(loc,_,_)) :: _ as binds)
  | LetBinds((Ast.SVarBind(loc,_,_)) :: _ as binds) ->
    Ast.SLetExpr(loc, List.rev binds, expr)
  | LetrecBinds((Ast.SLetrecBind(loc,_,_)) :: _ as binds) ->
    Ast.SLetrec(loc, List.rev binds, expr)
  | TypeLetBinds((Ast.STypeBind(loc,_,_)) :: _ as binds)
  | TypeLetBinds((Ast.SNewtypeBind(loc,_,_)) :: _ as binds) ->
    Ast.STypeLetExpr(loc, List.rev binds, expr)

let rec add_letrec_bind bg lrb stmts = add_letrec_binds bg [lrb] stmts

and add_letrec_binds bg lrbs stmts =
  match bg with
  | LetrecBinds(binds) ->
    desugar_scope_block stmts (LetrecBinds (lrbs @ binds))
  | LetBinds(_)
  | TypeLetBinds(_) ->
    bind_wrap bg @@ desugar_scope_block stmts (LetrecBinds lrbs)

and add_let_bind bg lb stmts =
  match bg with
  | LetBinds(binds) ->
    desugar_scope_block stmts (LetBinds(lb :: binds))
  | LetrecBinds(_)
  | TypeLetBinds(_) ->
    bind_wrap bg @@ desugar_scope_block stmts (LetBinds [lb])

and add_type_let_bind bg tlb stmts =
  match bg with
  | TypeLetBinds(binds) ->
    desugar_scope_block stmts (TypeLetBinds(tlb :: binds))
  | LetBinds(_)
  | LetrecBinds(_) ->
    bind_wrap bg @@ desugar_scope_block stmts (TypeLetBinds [tlb])

(** Treating stmts as a block, resolve scope.
There should be no blocks left after this stage of the compiler pipeline. *)
and desugar_scope_block stmts binding_group =
  match stmts with
  | [] -> failwith "Should not get an empty block in desugar_scope_block"
  | f :: rest_stmts ->
    match f with
    | Ast.SType(l, name, ann) ->
      add_type_let_bind binding_group (Ast.STypeBind(l, name, ann)) rest_stmts
    | Ast.SLet(l, bind, expr, _) ->
      add_let_bind binding_group (Ast.SLetBind(l, bind, expr)) rest_stmts
    | Ast.SVar(l, bind, expr) ->
      add_let_bind binding_group (Ast.SVarBind(l, bind, expr)) rest_stmts
    | Ast.SRec(l, bind, expr) ->
      add_letrec_bind binding_group (Ast.SLetrecBind(l, bind, expr)) rest_stmts
    | Ast.SFun(l, name, params, args, ann, doc, body, _check) ->
      let sbind = Ast.SBind(l, false, Ast.SName(l, name), Ast.ABlank)
      and slam  = Ast.SLam(l, params, args, ann, doc, body, _check) in
      add_letrec_bind binding_group (Ast.SLetrecBind(l, sbind, slam)) rest_stmts
    | Ast.SDataExpr(l, name, namet, params, mixins, variants, shared, _check) ->
      let b loc id = Ast.SBind(loc, false, Ast.SName(l, id), Ast.ABlank)
      and bn loc n = Ast.SBind(loc, false, n, Ast.ABlank) in
      let variant_binds data_blob_id = function
        | Ast.SVariant(loc,_,vname,_,_) ->
          let checker_name = Ast.make_checker_name vname in
          let get_part x = Ast.SDot(loc, data_blob_id, x) in
          [Ast.SLetrecBind(loc, b loc vname, get_part vname);
           Ast.SLetrecBind(loc, b loc checker_name, get_part checker_name)]
        | _ -> failwith "Non-letrec-id given to variant_binds" in
      let blob_id = Ast.global_names name
      and data_expr = Ast.SDataExpr(l, name, namet, params, mixins, variants, shared, _check) in
      let bind_data = Ast.SLetrecBind(l, bn l blob_id, data_expr)
      and bind_data_pred = Ast.SLetrecBind(l, b l @@ Ast.make_checker_name name, Ast.SDot(l, Ast.SIdLetrec(l, blob_id, true), name))
      and bind_data_pred2 = Ast.SLetrecBind(l, b l name, Ast.SDot(l, Ast.SIdLetrec(l, blob_id, true), name)) in
      let all_binds = List.fold_left (fun acc v ->
          (variant_binds (Ast.SIdLetrec(l, blob_id, true)) v) @ acc)
          [bind_data_pred; bind_data_pred2; bind_data] variants in
      add_letrec_binds binding_group all_binds rest_stmts
    | Ast.SContract(_, _, _) ->
      desugar_scope_block rest_stmts binding_group
    | Ast.SCheck(l, name, body, keyword) ->
      let b loc = Ast.SBind(loc, false, Ast.SUnderscore(l), Ast.ABlank) in
      let lrb = Ast.SLetrecBind(l, b l, Ast.SCheck(l, name, body, keyword)) in
      add_letrec_binds binding_group [lrb] rest_stmts
    | _ ->
      match rest_stmts with
      | [] -> bind_wrap binding_group f
      | _ :: _ ->
        let rest_stmt = desugar_scope_block rest_stmts (LetBinds([])) in
        let rest_stmts = match rest_stmt with
          | Ast.SBlock(_, stmts) -> f :: stmts
          | _ -> [f; rest_stmt] in
        bind_wrap binding_group (Ast.SBlock(Ast.expr_loc f, rest_stmts))

let scope_env_from_env = function
  | CompileEnvironment.CompileEnvironment(Globals.Globals(values,_),_) ->
    List.fold_left (fun acc name ->
        SD.add name (ScopeBinding.GlobalBind(Ast.Srcloc.Builtin("pyret-builtin"), Ast.SGlobal(name), None)) acc)
      SD.empty (List.map fst (SD.bindings values))

let type_env_from_env = function
  | CompileEnvironment.CompileEnvironment(Globals.Globals(_,types),_) ->
    List.fold_left (fun acc name ->
        SD.add name (TypeBinding.GlobalTypeBind(
            Ast.Srcloc.Builtin("pyret-builtin"), Ast.STypeGlobal(name), None)) acc)
      SD.empty (List.map fst (SD.bindings types))
