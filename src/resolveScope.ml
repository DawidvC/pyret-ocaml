module A = Ast
module SD = PyretUtils.StringDict
module U = AstUtils
module G = Gensym
module T = TypeStructs

open CompileStructs
open PyretUtils

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

class desugar_scope_visitor = object(self)
  inherit Ast.default_map_visitor

  method s_block(l, stmts) =
    desugar_scope_block (List.map self#visit_expr stmts) (LetBinds [])
end

(** Remove x = e, var x = e, and fun f(): e end
and turn them into explicit let and letrec expressions.
Do this recursively through the whole program.
Preconditions on prog:
  - well-formed
Postconditions on prog:
  - contains no SProvide in headers
  - contains no SLet, SVar, SData *)
let desugar_scope prog env =
  match prog with
  | Ast.SProgram(l, _provide_raw, provide_types_raw, imports_raw, body) ->
    let imports = List.map (fun i -> expand_import i env) imports_raw in
    let prov = match resolve_provide _provide_raw body with
      | Ast.SProvideNone(_) -> Ast.SObj(l, [])
      | Ast.SProvide(_, block) -> block
      | _ -> failwith "Should have been resolved away" in
    let provides = resolve_type_provide provide_types_raw body in
    let provt = match provides with
      | Ast.SProvideTypesNone(_) -> []
      | Ast.SProvideTypes(_, anns) -> anns
      | _ -> failwith ("Should have been resolve-typed away"
                       ^ (Sexplib.Sexp.to_string_hum (Ast.sexp_of_provide_types provides))) in
    (* TODO: Need to resolve provide-types here *)
    let with_imports = match body with
      | Ast.SBlock(l2, stmts) -> Ast.SBlock(l2, desugar_toplevel_types stmts)
      | _ -> Ast.SBlock(l, desugar_toplevel_types [body]) in
    let transform_toplevel_last l2 last =
      let app = Ast.SApp(l2, Ast.SDot(l2, AstUtils.checkers l2, "results"), []) in
      Ast.SModule(l2, last, [], [], prov, provt, app) in
    let with_provides = match with_imports with
      | Ast.SBlock(l2, stmts) ->
        let last = PyretUtils.last stmts in
        (match last with
         | Ast.STypeLetExpr(l3, binds, block2) ->
           (match block2 with
            | Ast.SBlock(b2loc, b2stmts) ->
              let inner_last = PyretUtils.last b2stmts in
              let inner_block_body = (PyretUtils.drop 1 b2stmts)
                                     @ [transform_toplevel_last l3 inner_last] in
              let inner_block = Ast.SBlock(b2loc, inner_block_body) in
              let stle = Ast.STypeLetExpr(l3, binds, inner_block) in
              let block_body = (PyretUtils.drop 1 stmts) @ [stle] in
              Ast.SBlock(l2, block_body)
            | _ -> failwith "Non-SBlock in STypeLetExpr body")
         | _ -> Ast.SBlock(l2, (PyretUtils.drop 1 stmts) @ [transform_toplevel_last l2 last]))
      | _ -> failwith "Impossible" in
    Ast.SProgram(l, Ast.SProvideNone(l), Ast.SProvideTypesNone(l), imports,
                 (new desugar_scope_visitor)#visit_expr with_provides)


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

type bind_pair = { atom : Ast.name; env : CompileEnvironment.t }
class resolve_names_class initial_env =
  object(self)
    inherit Ast.default_map_visitor

    val name_errors = ref []
    val bindings : ScopeBinding.t MutableStringDict.t = MutableStringDict.create 50
    val type_bindings : TypeBinding.t MutableStringDict.t = MutableStringDict.create 50
    val datatypes : Ast.expr MutableStringDict.t = MutableStringDict.create 50

    method make_anon_import_for s env (bindings : ScopeBinding.t MutableStringDict.t) b =
      let atom = Ast.global_names s in
      MutableStringDict.add bindings (Ast.name_key atom) (b atom);
      { atom = atom; env = env }

    method make_atom_for name is_shadowing env (bindings : ScopeBinding.t MutableStringDict.t) make_binding =
      match name with
      | Ast.SName(l, s) ->
        (if SD.mem s env && not is_shadowing then
           begin
             let old_loc = ScopeBinding.loc (SD.find s env) in
             name_errors := (CompileError.ShadowId(s, l, old_loc)) :: !name_errors
           end);
        let atom = Ast.global_names s in
        let binding = make_binding l atom in
        MutableStringDict.add bindings (Ast.name_key atom) binding;
        { atom = atom; env = env }
      | Ast.SUnderscore(l) ->
        let atom = Ast.global_names "$underscore" in
        MutableStringDict.add bindings (Ast.name_key atom) (make_binding l atom);
        { atom = atom; env = env }
      | Ast.SAtom(_,_) ->
        let binding = make_binding Ast.dummy_loc name in
        (* TODO: This is probably what it should be, but that's only true if there's a bug in Pyret *)
        let env = SD.add (Ast.name_key name) binding env in
        MutableStringDict.add bindings (Ast.name_key name) binding;
        { atom = atom; env = env }
      | _ -> failwith ("Unexpected atom type: " ^ (Ast.name_tosourcestring atom))

    method update_type_binding_ann atom ann =
      let key = Ast.name_key atom in
      if MutableStringDict.mem type_bindings key then
        begin
          let set_to = MutableStringDict.add type_bindings key in
          match MutableStringDict.find type_bindings key with
          | TypeBinding.LetTypeBind(l, _, _) ->
            let ann = match ann with
              | Some(ann) -> Some(Either.Left(ann))
              | None -> None in
            set_to @@ TypeBinding.LetTypeBind(l, atom, ann)
          | TypeBinding.ModuleTypeBind(l, _, imp, _) ->
            set_to @@ TypeBinding.ModuleTypeBind(l, atom, imp, ann)
          | TypeBinding.GlobalTypeBind(l, _, _) ->
            set_to @@ TypeBinding.GlobalTypeBind(l, atom, ann)
          | TypeBinding.TypeVarBind(l, _, _) ->
            set_to @@ TypeBinding.TypeVarBind(l, atom, ann)
        end
      else
        Printf.printf "No binding for %s\n" @@ Ast.name_tosourcestring atom
end
