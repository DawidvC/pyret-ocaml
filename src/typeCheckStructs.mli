type bindings = TypeStructs.Type.t PyretUtils.StringDict.t
val empty_bindings : bindings

module TCInfo : sig
  type errors = { insert : (CompileStructs.CompileError.t -> CompileStructs.CompileError.t list);
                  get : (unit -> CompileStructs.CompileError.t list) }
  type t = {typs : TypeStructs.Type.t PyretUtils.MutableStringDict.t;
            aliases : TypeStructs.Type.t PyretUtils.MutableStringDict.t;
            data_exprs : TypeStructs.Type.t PyretUtils.MutableStringDict.t;
            branders : TypeStructs.Type.t PyretUtils.MutableStringDict.t;
            modules : TypeStructs.ModuleType.t PyretUtils.MutableStringDict.t;
            mod_names : string PyretUtils.MutableStringDict.t;
            binds : bindings; modul : TypeStructs.ModuleType.t ref; errors : errors }

  val to_vs : t -> ValueSkeleton.t

  val empty : string -> t

  val add_binding_string : string -> TypeStructs.Type.t -> t -> t

  val add_binding : Ast.name -> TypeStructs.Type.t -> t -> t

end

module ContextItem : sig
  type t =
    | TermVar of string * TypeStructs.Type.t
    | ExistentialAssign of TypeStructs.Type.t * TypeStructs.Type.t
    | DataTypeVar of string * TypeStructs.Type.t

  val to_vs : t -> ValueSkeleton.t

  val get_typ : t -> TypeStructs.Type.t
end

type local_context = ContextItem.t list

type typed = Typed of Ast.program * TCInfo.t

module rec SynthesisResult : sig
  type t =
      SynthesisResult of Ast.expr * Ast.loc * TypeStructs.Type.t * Context.t
    | SynthesisBindingResult of Ast.let_bind * TypeStructs.Type.t * Context.t
    | SynthesisErr of CompileStructs.CompileError.t list

  type 'a binder = { result : (Ast.expr -> Ast.loc -> TypeStructs.Type.t -> Context.t -> 'a);
                     binding_result : (Ast.let_bind -> TypeStructs.Type.t -> Context.t -> 'a)}

  val bind : (t binder) -> t -> t
  val map_expr : (Ast.expr -> Ast.expr) -> t -> t
  val map_typ : (TypeStructs.Type.t -> TypeStructs.Type.t) -> t -> t
  val check_bind : (CheckingResult.t binder) -> t -> CheckingResult.t
  val synth_bind : (Ast.expr -> Ast.loc -> TypeStructs.Type.t -> Context.t -> SynthesisResult.t) -> t
    -> SynthesisResult.t
  val fold_bind : (Ast.expr -> Ast.loc -> TypeStructs.Type.t -> Context.t ->'a FoldResult.t) -> t ->
    'a FoldResult.t
end

and CheckingResult : sig
  type t =
      CheckingResult of Ast.expr * Context.t
    | CheckingErr of CompileStructs.CompileError.t list

  val bind : (Ast.expr -> Context.t -> t) -> t -> t
  val map : (Ast.expr ->  Ast.expr) -> t -> t
  val check_bind : (Ast.expr -> Context.t -> CheckingResult.t) -> t -> CheckingResult.t
  val synth_bind : (Ast.expr -> Context.t -> SynthesisResult.t) -> t -> SynthesisResult.t
  val fold_bind : (Ast.expr -> Context.t -> 'a FoldResult.t) -> t -> 'a FoldResult.t
end

and FoldResult : sig
  type 'a t =
      FoldResult of 'a
    | FoldErrors of CompileStructs.CompileError.t list

  val to_vs : ('a -> ValueSkeleton.t) -> 'a t -> ValueSkeleton.t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val check_bind : ('a -> CheckingResult.t) -> 'a t -> CheckingResult.t
  val synth_bind : ('a -> SynthesisResult.t) -> 'a t -> SynthesisResult.t
end

and Context : sig
  type t = TypingContext of local_context * TCInfo.t
  val to_vs : t -> ValueSkeleton.t
  val has_var_key : string -> t -> bool
  val get_var_type : string -> t -> TypeStructs.Type.t
  val resolve_alias : TypeStructs.Type.t -> t -> TypeStructs.Type.t
  val get_data_type : TypeStructs.Type.t -> t -> TypeStructs.Type.t option
  val add_term_var : string -> TypeStructs.Type.t -> t -> t
  val add_data_type : string -> TypeStructs.Type.t -> t -> t
  val assign_existential : TypeStructs.Type.t -> TypeStructs.Type.t -> t -> t FoldResult.t
  val apply : TypeStructs.Type.t -> t -> TypeStructs.Type.t
end

val resolve_alias : TypeStructs.Type.t -> Context.t -> TypeStructs.Type.t

val fold_synthesis : ('b -> Context.t -> SynthesisResult.t) -> 'b list -> Context.t -> ((Context.t, ((Ast.expr, Ast.let_bind) PyretUtils.Either.t) list) PyretUtils.Pair.t) FoldResult.t

val fold_checking : ('b -> Context.t -> CheckingResult.t) -> 'b list ->  Context.t -> ((Context.t, Ast.expr list) PyretUtils.Pair.t) FoldResult.t

val collapse_fold_list : 'b FoldResult.t list -> 'b list FoldResult.t

val map_result : ('a -> 'b FoldResult.t) -> 'a list -> 'b list FoldResult.t
