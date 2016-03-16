module A = Ast
module SD = PyretUtils.StringDict
module CS = CompileStructs
module W = WellFormed
module D = Desugar
module CH = DesugarCheck
module AU = AstUtils

type uri = CS.uri

type pyret_code =
    PyretString of string
  | PyretAst of A.program

type 'a loadable =
    ModuleAsString of CS.Provides.t * CS.CompileEnvironment.t * CS.CompileResult(CS.Provides).t
  | PreLoaded of CS.Provides.t * CS.CompileEnvironment.t * 'a

type provides = CS.Provides.t

type 'a locator = {
  (** Could either have needs_provide be implicitly stateful, and cache the most
      recent map, or use explicit interface below *)
  needs_compile : provides SD.t -> bool;

  (** Pre-compile (skippable if get-compile returns something) *)
  get_module : unit -> pyret_code;

  (** Pre-compile (had better be known with no other help) *)
  get_dependencies : unit -> CS.Dependency.t list;

  (** Pre-compile *)
  get_extra_imports : unit -> CS.ExtraImports.t;

  (** Pre-compile, specification of available globals *)
  get_globals : unit -> CS.Globals.t;

  (** Post-compile, on-run (maybe dynamic and new namespace) *)
  get_namespace : unit -> unit; (* FIXME: Missing R.Runtime.t and N.Namespace.t *)

  (** Returns this locator's URI *)
  uri : unit -> uri;

  (** Returns this locator's name *)
  name : unit -> string;

  (** Pre-compile if needs-compile is false *)
  get_compiled : unit -> 'a loadable option;

  (** Should compare uris for locators *)
  _equals : 'a locator -> 'a locator -> bool;
}

type ('a, 'b) located = Located of 'a locator * 'b

let get_ast (p : pyret_code) (uri : uri) =
  match p with
  | PyretString(s) -> failwith "NYI" (* TODO: Implement ParsePyret module *)
  | PyretAst(a) -> a

let get_import_type i =
  let open Ast in
  match i with
  | SImport(_, f, _)
  | SImportTypes(_, f, _, _)
  | SInclude(_, f)
  | SImportComplete(_, _, _, f, _, _)
  | SImportFields(_, _, f) -> f

let get_dependencies (p : pyret_code) (uri : uri) =
  let parsed = get_ast p uri in
  match parsed with
  | Ast.SProgram(_, _, _, imports, _) ->
    List.map (fun i -> AU.import_to_dep @@ get_import_type i) imports

(* TODO: get_standard_dependencies *)

let const_dict : 'a. string list -> 'a -> 'a SD.t = fun strs value ->
  List.fold_left (fun d s -> SD.add s value d) SD.empty strs
