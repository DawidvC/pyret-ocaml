val t_nothing : TypeStructs.Type.t
val t_boolean : TypeStructs.Type.t
val t_number : TypeStructs.Type.t
val t_arrow : TypeStructs.Type.t list -> TypeStructs.Type.t -> TypeStructs.Type.t
val t_top : TypeStructs.Type.t
val t_member : string -> TypeStructs.Type.t -> TypeStructs.TypeMember.t
val t_bot : TypeStructs.Type.t
val t_record : TypeStructs.TypeMember.t list -> TypeStructs.Type.t
val t_forall : TypeStructs.Type.t list -> TypeStructs.Type.t -> TypeStructs.Type.t
val t_var : TypeStructs.name -> TypeStructs.Type.t
val t_array : TypeStructs.Type.t -> TypeStructs.Type.t
val t_string : TypeStructs.Type.t
val t_option : TypeStructs.Type.t -> TypeStructs.Type.t
val t_data :
  TypeStructs.Type.t list -> TypeStructs.TypeVariant.t list -> TypeStructs.TypeMember.t list -> TypeStructs.Type.t
val t_variant :
  string -> TypeStructs.TypeMember.t list -> TypeStructs.TypeMember.t list -> TypeStructs.TypeVariant.t
val t_singleton_variant : string -> TypeStructs.TypeMember.t list -> TypeStructs.TypeVariant.t
val t_app : TypeStructs.Type.t -> TypeStructs.Type.t list -> TypeStructs.Type.t
val t_name : string option -> TypeStructs.name -> TypeStructs.Type.t
module CompileError :
sig
  type t =
    CompileError.t =
      WFErr of string * Ast.loc
    | WFErrSplit of string * Ast.loc list
    | ReservedName of Ast.loc * string
    | ZeroFraction of Ast.loc * Ast.expr
    | UnderscoreAsExpr of Ast.loc
    | UnderscoreAsAnn of Ast.loc
    | UnboundId of Ast.expr
    | UnboundVar of string * Ast.loc
    | UnboundTypeId of Ast.ann
    | UnexpectedTypeVar of Ast.loc * Ast.name
    | PointlessVar of Ast.loc
    | PointlessRec of Ast.loc
    | PointlessShadow of Ast.loc
    | BadAssignment of string * Ast.loc * Ast.loc
    | MixedIdVar of string * Ast.loc * Ast.loc
    | ShadowId of string * Ast.loc * Ast.loc
    | DuplicateId of string * Ast.loc * Ast.loc
    | DuplicateField of string * Ast.loc * Ast.loc
    | IncorrectType of string * Ast.loc * string * Ast.loc
    | BadTypeInstantiation of int * int * Ast.loc
    | IncorrectNumberOfArgs of Ast.loc
    | ApplyNonFunction of Ast.loc * Ast.ann
    | ObjectMissingField of string * string * Ast.loc * Ast.loc
    | UnneccesaryBranch of string * Ast.loc * string * Ast.loc
    | UnneccesaryElseBranch of string * Ast.loc
    | NonExhaustivePattern of string list * string * Ast.loc
    | CantMatchOn of string * Ast.loc
    | IncorrectNumberOfBindings of string * Ast.loc * int * int
    | CasesSingletonMismatch of string * Ast.loc * bool
    | GivenParameters of string * Ast.loc
    | UnableToInstantiate of Ast.loc
    | CantTypecheck of string
    | Unsupported of string * Ast.loc
    | NoModule of Ast.loc * string
  val render_reason : t -> ErrorDisplay.error_display
  val to_vs : t -> ValueSkeleton.t
end

type uri = string

module PyretDialect : sig type t = Pyret | Bootstrap end

module Dependency :
sig
  type t = Dependency of string * string list | Builtin of string
  val key : t -> string
end

module ScopeBinding :
sig
  type t =
      LetrecBind of Ast.loc * Ast.name * Ast.expr option
    | LetBind of Ast.loc * Ast.name * Ast.expr option
    | VarBind of Ast.loc * Ast.name * Ast.expr option
    | GlobalBind of Ast.loc * Ast.name * Ast.expr option
    | ModuleBind of Ast.loc * Ast.name * Ast.expr option
  val loc : t -> Ast.loc
  val atom : t -> Ast.name
  val expr : t -> Ast.expr option
end

module TypeBinding :
sig
  type t =
      GlobalTypeBind of Ast.loc * Ast.name * Ast.ann option
    | LetTypeBind of Ast.loc * Ast.name *
                     (Ast.ann, Ast.import) PyretUtils.Either.t option
    | ModuleTypeBind of Ast.loc * Ast.name * Ast.ann option
    | TypeVarBind of Ast.loc * Ast.name * Ast.ann option
  val loc : t -> Ast.loc
  val atom : t -> Ast.name
  val ann : t -> (Ast.ann, Ast.import) PyretUtils.Either.t option
end

module NameResolution :
sig
  type t =
      Resolved of Ast.program * CompileError.t list * ScopeBinding.t PyretUtils.MutableStringDict.t *
                  TypeBinding.t PyretUtils.MutableStringDict.t * Ast.expr PyretUtils.MutableStringDict.t
end

module ExtraImport :
sig
  type t = ExtraImport of Dependency.t * string * string list * string list
end

module ExtraImports : sig type t = ExtraImports of ExtraImport.t list end

module Globals :
sig
  type t = Globals of TypeStructs.Type.t PyretUtils.StringDict.t *
                      TypeStructs.Type.t PyretUtils.StringDict.t (** values, types *)
end

module Provides :
sig
  type t = Provides of uri * TypeStructs.Type.t PyretUtils.StringDict.t *
                       TypeStructs.Type.t PyretUtils.StringDict.t * TypeStructs.Type.t PyretUtils.StringDict.t
end

module CompileEnvironment :
sig type t = CompileEnvironment of Globals.t * Provides.t PyretUtils.StringDict.t end

module CompileResult :
  functor (C : sig type t end) ->
  sig type t = Ok of C.t | Err of CompileError.t list end
