module A = Ast
module SD = PyretUtils.StringDict
module MSD = PyretUtils.MutableStringDict
module VS = ValueSkeleton
module TS = TypeStructs
module TD = TypeDefaults
module C = CompileStructs
module LA = ListAux

type name = A.name

let dict_to_string = TS.dict_to_string
let mut_dict_to_string = TS.mut_dict_to_string

module Type = TS.Type

module Pair = PyretUtils.Pair

module TypeMember = TS.TypeMember
module TypeVariant = TS.TypeVariant
module ModuleType = TS.ModuleType

type bindings = Type.t SD.t
let empty_bindings : bindings = SD.empty

module TCInfo = struct
  type errors = { insert : (C.CompileError.t -> C.CompileError.t list);
                  get : (unit -> C.CompileError.t list) }
  type t = {typs : Type.t MSD.t; aliases : Type.t MSD.t;
            data_exprs : Type.t MSD.t; branders : Type.t MSD.t;
            modules : ModuleType.t MSD.t; mod_names : string MSD.t;
            binds : bindings; modul : ModuleType.t ref; errors : errors }

  let to_vs (tcinfo : t) =
    let msd_type_to_vs = mut_dict_to_string Type.to_vs in
    VS.VSConstr("tc-info",
                [
                  msd_type_to_vs tcinfo.typs;
                  msd_type_to_vs tcinfo.aliases;
                  msd_type_to_vs tcinfo.data_exprs;
                  msd_type_to_vs tcinfo.branders;
                  mut_dict_to_string ModuleType.to_vs tcinfo.modules;
                  dict_to_string Type.to_vs tcinfo.binds;
                  VS.VSStr("NYI: TCInfo error list")
                ])

  let empty mod_name =
    let curr_module = ModuleType.TModule(mod_name, Type.TTop(A.dummy_loc), SD.empty, SD.empty) in
    let errors =
      let err_list = ref [] in
      { insert = (fun err -> err_list := err :: !err_list; !err_list);
        get = (fun () -> !err_list )} in
    {typs = TD.make_default_typs(); aliases = TD.make_default_aliases();
     data_exprs = TD.make_default_data_exprs(); branders = MSD.create 20;
     modules = TD.make_default_modules(); mod_names = MSD.create 20;
     binds = empty_bindings; modul = ref curr_module; errors = errors}

  let add_binding_string (id : string) (bound : Type.t) (info : t) =
    let new_binds = SD.add id bound info.binds in
    { info with
      binds = new_binds
    }

  let add_binding (id : name) (bound : Type.t) (info : t) =
    add_binding_string (A.name_key id) bound info

end

module ContextItem = struct
  type t =
    | TermVar of string * Type.t
    | ExistentialAssign of Type.t * Type.t
    | DataTypeVar of string * Type.t

  let to_vs = function
    | TermVar(variable,typ) ->
      VS.VSConstr("term-var", [VS.VSStr(variable); Type.to_vs typ])
    | ExistentialAssign(variable,typ) ->
      VS.VSConstr("existential-assign", List.map Type.to_vs [variable; typ])
    | DataTypeVar(variable,typ) ->
      VS.VSConstr("data-type-var", [VS.VSStr(variable); Type.to_vs typ])

  let get_typ = function
    | TermVar(_,typ)
    | ExistentialAssign(_,typ)
    | DataTypeVar(_,typ) -> typ
end

type local_context = ContextItem.t list

type typed = Typed of A.program * TCInfo.t

module rec SynthesisResult : sig
  type t =
      SynthesisResult of A.expr * A.loc * Type.t * Context.t
    | SynthesisBindingResult of A.let_bind * Type.t * Context.t
    | SynthesisErr of C.CompileError.t list

  type 'a binder = { result : (A.expr -> A.loc -> Type.t -> Context.t -> 'a);
                     binding_result : (A.let_bind -> Type.t -> Context.t -> 'a)}

  val bind : (t binder) -> t -> t
  val map_expr : (A.expr -> A.expr) -> t -> t
  val map_typ : (Type.t -> Type.t) -> t -> t
  val check_bind : (CheckingResult.t binder) -> t -> CheckingResult.t
  val synth_bind : (A.expr -> A.loc -> Type.t -> Context.t -> SynthesisResult.t) -> t
    -> SynthesisResult.t
  val fold_bind : (A.expr -> A.loc -> Type.t -> Context.t ->'a FoldResult.t) -> t ->
    'a FoldResult.t
end = struct
  type t =
      SynthesisResult of A.expr * A.loc * Type.t * Context.t
    | SynthesisBindingResult of A.let_bind * Type.t * Context.t
    | SynthesisErr of C.CompileError.t list

  type 'a binder = { result : (A.expr -> A.loc -> Type.t -> Context.t -> 'a);
                     binding_result : (A.let_bind -> Type.t -> Context.t -> 'a)}

  let bind f = function
    | SynthesisResult(ast,loc,typ,out_context) -> f.result ast loc typ out_context
    | SynthesisBindingResult(let_bind,typ,out_context) -> f.binding_result let_bind typ out_context
    | (_ as other) -> other

  let map_expr f = function
    | SynthesisResult(ast,loc,typ,out_context) ->
      SynthesisResult(f ast,loc,typ,out_context)
    | SynthesisBindingResult(_,_,_) -> failwith "Cannot map expr on synthesis-binding-result"
    | (_ as other) -> other

  let map_typ f = function
    | SynthesisResult(ast,loc,typ,out_context) ->
      SynthesisResult(ast,loc,f typ,out_context)
    | SynthesisBindingResult(let_bind,typ,out_context) ->
      SynthesisBindingResult(let_bind,f typ,out_context)
    | (_ as other) -> other

  let check_bind f = function
    | SynthesisResult(ast,loc,typ,out_context) ->
      f.result ast loc typ out_context
    | SynthesisBindingResult(let_bind,typ,out_context) ->
      f.binding_result let_bind typ out_context
    | SynthesisErr(errors) -> failwith "NYI (check_bind) SynthesisErr"

  let synth_bind f = function
    | SynthesisResult(ast,loc,typ,out_context) ->
      f ast loc typ out_context
    | SynthesisBindingResult(_,_,_) -> failwith "Cannot fold on synthesis-binding-result"
    | SynthesisErr(errors) -> failwith "NYI (synth_bind) SynthesisErr"

  let fold_bind f = function
    | SynthesisResult(ast,loc,typ,out_context) ->
      f ast loc typ out_context
    | SynthesisBindingResult(_,_,_) -> failwith "Cannot fold on synthesis-binding-result"
    | SynthesisErr(errors) -> failwith "NYI (fold_bind) SynthesisErr"

  let to_vs = function
    | SynthesisResult(ast,loc,typ,out_context) ->
      VS.VSConstr("tc-synthesis-result",
                  [
                    VS.VSStr(A.expr_to_string ast);
                    Type.to_vs typ;
                    Context.to_vs out_context
                  ])
    | SynthesisBindingResult(let_bind, typ, out_context) ->
      VS.VSConstr("tc-synthesis-binding-result", [
          VS.VSStr(PPrint.pretty_string (A.letbind_tosource let_bind) 80);
          Type.to_vs typ;
          Context.to_vs out_context
        ])
    | SynthesisErr(errors) ->
      (* FIXME: This is very incorrect *)
      VS.VSConstr("tc-synthesis-err",
                  [VS.of_list C.CompileError.to_vs errors ()])
end

and CheckingResult : sig
  type t =
      CheckingResult of A.expr * Context.t
    | CheckingErr of C.CompileError.t list

  val bind : (A.expr -> Context.t -> t) -> t -> t
  val map : (A.expr ->  A.expr) -> t -> t
  val check_bind : (A.expr -> Context.t -> CheckingResult.t) -> t -> CheckingResult.t
  val synth_bind : (A.expr -> Context.t -> SynthesisResult.t) -> t -> SynthesisResult.t
  val fold_bind : (A.expr -> Context.t -> 'a FoldResult.t) -> t -> 'a FoldResult.t
end = struct
  type t =
      CheckingResult of A.expr * Context.t
    | CheckingErr of C.CompileError.t list

  let bind f = function
    | CheckingResult(ast, out_context) -> f ast out_context
    | (CheckingErr(_) as cres) -> cres

  let map f = function
    | CheckingResult(ast, out_context) -> CheckingResult(f ast, out_context)
    | (CheckingErr(_) as cres) -> cres

  let check_bind f = function
    | CheckingResult(ast, out_context) -> f ast out_context
    | (CheckingErr(_) as cres) -> cres

  let synth_bind f = function
    | CheckingResult(ast, out_context) -> f ast out_context
    | CheckingErr(errors) -> SynthesisResult.SynthesisErr(errors)

  let fold_bind f = function
    | CheckingResult(ast, out_context) -> f ast out_context
    | CheckingErr(errors) -> FoldResult.FoldErrors(errors)
end

and FoldResult : sig
  type 'a t =
      FoldResult of 'a
    | FoldErrors of C.CompileError.t list

  val to_vs : ('a -> VS.t) -> 'a t -> VS.t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val check_bind : ('a -> CheckingResult.t) -> 'a t -> CheckingResult.t
  val synth_bind : ('a -> SynthesisResult.t) -> 'a t -> SynthesisResult.t
end = struct
  type 'a t =
      FoldResult of 'a
    | FoldErrors of C.CompileError.t list

  let to_vs : 'a. ('a -> VS.t) -> 'a t -> VS.t = fun conv -> function
    | FoldResult(v) -> VS.VSConstr("fold-result", [conv v])
    | FoldErrors(errors) -> VS.VSConstr("fold-errors",
                                        [VS.of_list C.CompileError.to_vs errors ()])

  let bind f = function
    | FoldResult(v) -> f v
    | (FoldErrors(_) as foldres) -> foldres

  let map f = function
    | FoldResult(v) -> FoldResult(f v)
    | (FoldErrors(_) as foldres) -> foldres

  let check_bind f = function
    | FoldResult(v) -> f v
    | FoldErrors(errors) -> CheckingResult.CheckingErr(errors)

  let synth_bind f = function
    | FoldResult(v) -> f v
    | FoldErrors(errors) -> SynthesisResult.SynthesisErr(errors)
end

and Context : sig
  type t = TypingContext of local_context * TCInfo.t
  val to_vs : t -> VS.t
  val has_var_key : string -> t -> bool
  val get_var_type : string -> t -> Type.t
  val resolve_alias : Type.t -> t -> Type.t
  val get_data_type : Type.t -> t -> Type.t option
  val add_term_var : string -> Type.t -> t -> t
  val add_data_type : string -> Type.t -> t -> t
  val assign_existential : Type.t -> Type.t -> t -> t FoldResult.t
  val apply : Type.t -> t -> Type.t
end = struct
  type t = TypingContext of local_context * TCInfo.t

  let to_vs = function
    | TypingContext(local_context, info) ->
      VS.VSConstr("typing-context", [VS.of_list ContextItem.to_vs local_context ()])

  let find = PyretUtils.list_find

  let has_var_key id_key = function
    | TypingContext(local_context, info) ->
      let pred = function
        | ContextItem.TermVar(variable,_) -> variable = id_key
        | _ -> false in
      match find pred local_context with
      | Some(_) -> true
      | None -> MSD.mem info.TCInfo.typs id_key

  let get_var_type id_key = function
    | TypingContext(local_context, info) ->
      let pred = function
        | ContextItem.TermVar(variable,_) -> variable = id_key
        | _ -> false in
      let get_typ = function
        | ContextItem.TermVar(_,typ) -> typ
        | _ -> failwith "Internal error: pred failed to only choose TermVar" in
      match find pred local_context with
      | Some(item) -> get_typ item
      | None -> MSD.find info.TCInfo.typs id_key

  let rec resolve_alias t = function
    | (TypingContext(local_context, info) as context) ->
      match t with
      | Type.TName(a_mod,a_id,l) ->
        (match a_mod with
         | None ->
           (match MSD.lookup info.TCInfo.aliases (A.name_key a_id) with
            | None -> t
            | Some(aliased) -> Type.set_loc l (resolve_alias aliased context))
         | Some(m) ->
           if m = "builtin" then
             (match MSD.lookup info.TCInfo.aliases (A.name_key a_id) with
              | None -> t
              | Some(aliased) -> Type.set_loc l aliased)
           else
             let option_flat_map f = function
               | None -> None
               | Some(v) -> f v in
             let lookup_in_mod = function
               | ModuleType.TModule(_,_,_,aliases) -> SD.lookup (A.name_toname a_id) aliases in
             match option_flat_map lookup_in_mod (MSD.lookup info.TCInfo.modules m) with
             | None -> t
             | Some(aliased) ->
               match aliased with
               | Type.TName(aliased_mod,aliased_id,_) ->
                 if (aliased_mod = a_mod) && (aliased_id = a_id) then
                   Type.set_loc l aliased
                 else
                   Type.set_loc l (resolve_alias aliased context)
               | _ -> Type.set_loc l aliased)
      | _ -> t

  let rec get_data_type typ = function
    | (TypingContext(local_context, info) as context) ->
      let typ = resolve_alias typ context in
      match typ with
      | Type.TName(module_name, name, _) ->
        (match module_name with
         | Some(m) ->
           (match MSD.lookup info.TCInfo.modules m with
            | Some(t_mod) ->
              let modtypes = function | ModuleType.TModule(_,_,t,_) -> t in
              (match SD.lookup (A.name_toname name) (modtypes t_mod) with
               | Some(typ) -> Some(typ)
               | None ->
                 failwith ("No type " ^ (VS.render (Type.to_vs typ)) ^ " available on '"
                           ^ (VS.render (ModuleType.to_vs t_mod)) ^ "'"))
            | None ->
              if m = "builtin" then
                MSD.lookup info.TCInfo.data_exprs (A.name_toname name)
              else
                failwith ("No module available with the name `" ^ m ^ "'"))
         | None ->
           let id_key = A.name_key name in
           let maybe_local = Option.map ContextItem.get_typ (find (function
               | ContextItem.DataTypeVar(variable,_) -> variable = id_key
               | _ -> false) local_context) in
           match maybe_local with
           | Some(_) -> maybe_local
           | None -> MSD.lookup info.TCInfo.data_exprs id_key)
      | Type.TApp(base_typ,args,_) ->
        let base_data_typ = get_data_type base_typ context in
        Option.map (Type.introduce args) base_data_typ
      | _ -> None

  let add_term_var var_name typ = function
    | TypingContext(local_context, info) ->
      TypingContext((ContextItem.TermVar(var_name, typ)) :: local_context, info)

  let add_data_type type_name typ = function
    | TypingContext(local_context, info) ->
      TypingContext((ContextItem.DataTypeVar(type_name, typ)) :: local_context, info)

  let assign_existential existential assigned_typ = function
    | TypingContext(local_context, info) ->
      let assign = fun () ->
        FoldResult.FoldResult(
          TypingContext((ContextItem.ExistentialAssign(existential,assigned_typ)) ::
                        (List.map (let substitute = Type.substitute existential assigned_typ in
                            function
                             | ContextItem.TermVar(variable,typ) ->
                               ContextItem.TermVar(variable, substitute typ)
                             | ContextItem.ExistentialAssign(variable,typ) ->
                               ContextItem.ExistentialAssign(variable, substitute typ)
                             | ContextItem.DataTypeVar(variable,typ) ->
                               ContextItem.DataTypeVar(variable, substitute typ))
                           local_context), info)) in
      let pred = function
        | ContextItem.ExistentialAssign(variable, _) -> variable = existential
        | _ -> false in
      match find pred local_context with
      | Some(item) ->
        (* TODO: Pass in predicate (satisfies type) *)
        if (ContextItem.get_typ item) = assigned_typ then
          assign()
        else
          FoldResult.FoldErrors([]) (* TODO: Come up with actual error *)
      | None -> assign()

  let apply typ = function
    | TypingContext(local_context, _) ->
      List.fold_left (fun curr_typ -> function
          | ContextItem.ExistentialAssign(variable,assigned_typ) ->
            Type.substitute variable assigned_typ typ
          | _ -> curr_typ) typ local_context
end

let resolve_alias = Context.resolve_alias
