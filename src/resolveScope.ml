module A = Ast
module SD = Map.Make(String)
module C = CompileStructs
module U = AstUtils
module G = Gensym
module T = TypeStructs

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

let expand_import (imp : A.import) (env : C.CompileEnvironment.t) : A.import =
  match imp with
  | A.SImport(l,imp,name) ->
    A.SImportComplete(l,[],[],imp,name,name)
  | A.SImportFields(l,fields,imp) ->
    A.SImportComplete(l,fields,[],imp,A.SUnderscore(l),A.SUnderscore(l))
  | A.SInclude(l,imp) ->
    let imp_name = A.SUnderscore(l) in
    let info_key = U.key (U.import_to_dep imp) in
    let safe_find sd key =
      try
        Some(SD.find sd key)
      with
      | Invalid_argument(_) -> None in
    let mod_info =
      safe_find info_key (match env with | C.CompileEnvironment.CompileEnvironment(_,e) -> e) in
    (match mod_info with
    | None -> failwith ("No compile-time information provided for module "^info_key)
    | Some(provides) ->
      match provides with
      | C.Provides.Provides(_,values,aliases,_) ->
        let val_names = List.map (fun n -> A.SName(l,fst n)) (SD.bindings values) in
        let type_names = List.map (fun n -> A.SName(l, fst n)) (SD.bindings aliases) in
        A.SImportComplete(l,val_names,type_names,imp,imp_name,imp_name))
  | A.SImportComplete(_,_,_,_,_,_) -> imp
  | _ -> failwith "NYI: SImportTypes"
