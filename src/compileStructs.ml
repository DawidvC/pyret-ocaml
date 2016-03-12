module A = Ast
module ED = ErrorDisplay
module SD = Map.Make(String)
module T = TypeStructs
module U = PyretUtils

let t_nothing = T.t_nothing A.dummy_loc
let t_boolean = T.t_boolean A.dummy_loc
let t_number = T.t_number A.dummy_loc
let t_arrow params ret = T.Type.TArrow(params,ret,A.dummy_loc)
let t_top = T.Type.TTop A.dummy_loc
let t_member name value = T.TypeMember.TMember(name,value,A.dummy_loc)
let t_bot = T.Type.TBot A.dummy_loc
let t_record fields = T.Type.TRecord(fields,A.dummy_loc)
let t_forall params body = T.Type.TForall(params,body,A.dummy_loc)
let t_var name = T.Type.TVar(name,A.dummy_loc)
let t_array c = T.t_array c A.dummy_loc
let t_string = T.t_string A.dummy_loc
let t_option typ = T.t_option typ A.dummy_loc
let t_data params variants members = T.Type.TData(params,variants,members,A.dummy_loc)
let t_variant name fields with_members = T.TypeVariant.TVariant(name,fields,with_members,A.dummy_loc)
let t_singleton_variant name with_members =
  T.TypeVariant.TSingletonVariant(name,with_members,A.dummy_loc)
let t_app func args = T.Type.TApp(func,args,A.dummy_loc)
let t_name modulename name = T.Type.TName(modulename,name,A.dummy_loc)

module CompileError = CompileError

type uri = string

module PyretDialect = struct
  type t =
      Pyret
    | Bootstrap
end

module Dependency = struct
  type t =
      Dependency of string * string list
    | Builtin of string

  let key = function
    | Dependency(protocol,arguments) ->
      protocol ^ "(" ^ (U.join_str arguments ", ") ^ ")"
    | Builtin(modname) -> "builtin(" ^ modname ^ ")"
end

module NameResolution = struct
  type ('a, 'b, 'c) t = Resolved of A.program * CompileError.t list * 'a SD.t * 'b SD.t * 'c SD.t
end

module ExtraImport = struct
  type t = ExtraImport of Dependency.t * string * string list * string list
end

module ExtraImports = struct
  type t = ExtraImports of ExtraImport.t list
end

module Globals = struct
  type t = Globals of T.Type.t SD.t * T.Type.t SD.t
end

module Provides = struct
  type t = Provides of uri * T.Type.t SD.t * T.Type.t SD.t * T.Type.t SD.t
end

module CompileEnvironment = struct
  type t = CompileEnvironment of Globals.t * Provides.t SD.t
end

module CompileResult (C : sig type t end) = struct
  type t =
      Ok of C.t
    | Err of CompileError.t list
end
