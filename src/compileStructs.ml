module A = Ast
module ED = ErrorDisplay
module SD = Map.Make(String)
module U = PyretUtils

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
(*
module NameResolution = struct
  type t = Resolved of A.program * CompileError.t list * SD.t * SD.t * SD.t
end*)

module ExtraImport = struct
  type t = ExtraImport of Dependency.t * string * string list * string list
end

module ExtraImports = struct
  type t = ExtraImports of ExtraImport.t list
end

module Globals = struct
  type t = Globals of unit * unit (** FIXME: Needs to be T.Type *)
end

module Provides = struct
  type t = Provides of uri * unit SD.t * unit SD.t * unit SD.t
end

module CompileEnvironment = struct
  type t = CompileEnvironment of Globals.t * Provides.t SD.t
end

module CompileResult (C : sig type t end) = struct
  type t =
      Ok of C.t
    | Err of CompileError.t list
end
