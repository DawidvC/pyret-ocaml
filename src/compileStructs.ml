module A = Ast
module ED = ErrorDisplay
module SD = PyretUtils.StringDict
module MSD = PyretUtils.MutableStringDict
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

module ScopeBinding = struct
  type t =
      LetrecBind of Ast.loc * Ast.name * Ast.ann * Ast.expr option
    | LetBind of Ast.loc * Ast.name * Ast.ann * Ast.expr option
    | VarBind of Ast.loc * Ast.name * Ast.ann * Ast.expr option
    | GlobalBind of Ast.loc * Ast.name * Ast.expr option
    | ModuleBind of Ast.loc * Ast.name * Ast.import_type * (Ast.expr, Ast.import) PyretUtils.Either.t option

  let loc = function
    | LetrecBind(l,_,_,_)
    | LetBind(l,_,_,_)
    | VarBind(l,_,_,_)
    | GlobalBind(l,_,_)
    | ModuleBind(l,_,_,_) -> l

  let atom = function
    | LetrecBind(_,a,_,_)
    | LetBind(_,a,_,_)
    | VarBind(_,a,_,_)
    | GlobalBind(_,a,_)
    | ModuleBind(_,a,_,_) -> a

  let ann = function
    | LetrecBind(_,_,a,_)
    | LetBind(_,_,a,_)
    | VarBind(_,_,a,_) -> a
    | ModuleBind(_,_,_,_)
    | GlobalBind(_,_,_) -> failwith "No annotation on GlobalBinds."

  let expr = function
    | LetrecBind(_,_,_,e)
    | LetBind(_,_,_,e)
    | VarBind(_,_,_,e)
    | GlobalBind(_,_,e) -> e
    | ModuleBind(_,_,_,Some(PyretUtils.Either.Left(e))) -> Some(e)
    | ModuleBind(_,_,_,None) -> None
    | ModuleBind(_,_,_,_) -> failwith "Non-expr on ModuleBind."
end

module TypeBinding = struct
  type t =
      GlobalTypeBind of Ast.loc * Ast.name * Ast.ann option
    | LetTypeBind of Ast.loc * Ast.name * (Ast.ann, Ast.import) PyretUtils.Either.t option
    | ModuleTypeBind of Ast.loc * Ast.name * Ast.import_type * Ast.ann option
    | TypeVarBind of Ast.loc * Ast.name * Ast.ann option

  let loc = function
    | GlobalTypeBind(l,_,_)
    | LetTypeBind(l,_,_)
    | ModuleTypeBind(l,_,_,_)
    | TypeVarBind(l,_,_) -> l

  let atom = function
    | GlobalTypeBind(_,a,_)
    | LetTypeBind(_,a,_)
    | ModuleTypeBind(_,a,_,_)
    | TypeVarBind(_,a,_) -> a

  let ann = function
    | LetTypeBind(_,_,a) -> a
    | GlobalTypeBind(_,_,a)
    | ModuleTypeBind(_,_,_,a)
    | TypeVarBind(_,_,a) ->
      let open PyretUtils.Either in
      match a with
      | None -> None
      | Some(a) ->
        Some(Left(a))
end


module NameResolution = struct
  type t = Resolved of A.program * CompileError.t list * ScopeBinding.t MSD.t * TypeBinding.t MSD.t * Ast.expr MSD.t
end

module ExtraImport = struct
  type t = ExtraImport of Dependency.t * string * string list * string list

  let dependency = function
    | ExtraImport(d,_,_,_) -> d

  let as_name = function
    | ExtraImport(_,n,_,_) -> n

  let values = function
    | ExtraImport(_,_,v,_) -> v

  let types = function
    | ExtraImport(_,_,_,t) -> t
end

module ExtraImports = struct
  type t = ExtraImports of ExtraImport.t list

  let imports = function
    | ExtraImports(i) -> i

  let dependencies ei = ei |> imports |> (List.map ExtraImport.dependency)

  let as_names ei = ei |> imports |> (List.map ExtraImport.as_name)

  let values ei = ei |> imports |> (List.map ExtraImport.values)

  let types ei = ei |> imports |> (List.map ExtraImport.types)
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

type compile_options =
  { check_mode : bool;
    type_check : bool;
    allow_shadowed : bool;
    collect_all : bool;
    ignore_unbound : bool;
    proper_tail_calls: bool; }

let default_compile_options =
  { check_mode = true;
    type_check = false;
    allow_shadowed = false;
    collect_all = false;
    ignore_unbound = false;
    proper_tail_calls = true; }

let t_pred = t_arrow [t_top] t_boolean
let t_pred2 = t_arrow [t_top; t_top] t_boolean

let t_number_binop = t_arrow [t_number; t_number] t_number
let t_number_unop = t_arrow [t_number] t_number
let t_number_pred1 = t_arrow [t_number] t_boolean
let t_within_num = t_arrow [t_number] @@ t_arrow [t_number; t_number] t_boolean
let t_within_any = t_arrow [t_number] @@ t_arrow [t_top; t_top] t_boolean

let runtime_types = PyretUtils.StringDict.of_list
    [
      "Number", t_top;
      "String", t_string;
      "Function", t_top;
      "Boolean", t_top;
      "Object", t_top;
      "Method", t_top;
      "Nothing", t_top;
      "RawArray", t_top
    ]

let t_forall1 f =
  let n = Ast.global_names "a" in
  t_forall [t_var n] (f (t_var n))

let runtime_builtins =
  let t_record l = t_record (List.map (fun (name, typ) -> t_member name typ) l) in
  PyretUtils.StringDict.of_list
    [
      "test-print", t_forall1 (fun a -> t_arrow [a] a);
      "print", t_forall1 (fun a -> t_arrow [a] a);
      "display", t_forall1 (fun a -> t_arrow [a] a);
      "print-error", t_forall1 (fun a -> t_arrow [a] a);
      "display-error", t_forall1 (fun a -> t_arrow [a] a);
      "tostring", t_arrow [t_top] t_string;
      "torepr", t_arrow [t_top] t_string;
      "brander", t_top;
      "nothing", t_nothing;
      "builtins", t_record [
        "has-field", t_arrow [t_record([])] t_boolean;
        "current-checker", t_arrow [] @@ t_record [
          "run-checks", t_bot;
          "check-is", t_bot;
          "check-is-refinement", t_bot;
          "check-is-not", t_bot;
          "check-is-not-refinement", t_bot;
          "check-satisfies", t_bot;
          "check-satisfies-not", t_bot;
          "check-raises-str", t_bot;
          "check-raises-not", t_bot;
          "check-raises-other-str", t_bot;
          "check-raises-satisfies", t_bot;
          "check-raises-violates", t_bot;
        ];
      ];
      "not", t_arrow [t_boolean] t_boolean;
      "is-nothing", t_pred;
      "is-number", t_pred;
      "is-string", t_pred;
      "is-boolean", t_pred;
      "is-object", t_pred;
      "is-function", t_pred;
      "is-raw-array", t_pred;
      "gensym", t_top;
      "random", t_top;
      "run-task", t_top;
      "_plus", t_top;
      "_minus", t_top;
      "_times", t_top;
      "_divide", t_top;
      "_lessthan", t_top;
      "_lessequal", t_top;
      "_greaterthan", t_top;
      "_greaterequal", t_top;
      "string-equal", t_top;
      "string-contains", t_top;
      "string-append", t_top;
      "string-length", t_top;
      "string-tonumber", t_top;
      "string-to-number", t_arrow [t_string] @@ t_option t_number;
      "string-repeat", t_top;
      "string-substring", t_top;
      "string-replace", t_top;
      "string-split", t_top;
      "string-split-all", t_top;
      "string-char-at", t_top;
      "string-toupper", t_top;
      "string-tolower", t_top;
      "string-explode", t_top;
      "string-index-of", t_top;
      "string-to-code-point", t_top;
      "string-from-code-point", t_top;
      "string-to-code-points", t_top;
      "string-from-code-points", t_top;
      "time-now", t_number_unop;
      "num-random", t_number_unop;
      "num-random-seed", t_arrow [t_number] t_nothing;
      "num-max", t_number_binop;
      "num-min", t_number_binop;
      "num-equal", t_arrow [t_number; t_number] t_boolean;
      "num-round", t_number_unop;
      "num-round-even", t_number_unop;
      "num-abs", t_number_unop;
      "num-sin", t_number_unop;
      "num-cos", t_number_unop;
      "num-tan", t_number_unop;
      "num-asin", t_number_unop;
      "num-acos", t_number_unop;
      "num-atan", t_number_unop;
      "num-modulo", t_number_binop;
      "num-truncate", t_number_unop;
      "num-sqrt", t_number_unop;
      "num-sqr", t_number_unop;
      "num-ceiling", t_number_unop;
      "num-floor", t_number_unop;
      "num-log", t_number_unop;
      "num-exp", t_number_unop;
      "num-exact", t_number_unop;
      "num-to-rational", t_number_unop;
      "num-to-roughnum", t_number_unop;
      "num-is-positive", t_number_pred1;
      "num-is-negative", t_number_pred1;
      "num-is-non-positive", t_number_pred1;
      "num-is-non-negative", t_number_pred1;
      "num-is-integer", t_number_pred1;
      "num-is-fixnum", t_number_pred1;
      "num-is-rational", t_number_pred1;
      "num-is-roughnum", t_number_pred1;
      "num-expt", t_number_binop;
      "num-tostring", t_arrow [t_number] t_string;
      "num-to-string", t_arrow [t_number] t_string;
      "num-to-string-digits", t_arrow [t_number; t_number] t_string;
      "num-within", t_within_num;
      "num-within-rel", t_within_num;
      "num-within-abs", t_within_num;
      "within-rel", t_within_any;
      "within-rel-now", t_within_any;
      "within-abs", t_within_any;
      "within-abs-now", t_within_any;
      "within", t_within_any;
      "raw-array-get", t_top;
      "raw-array-set", t_top;
      "raw-array-of", t_top;
      "raw-array-length", t_top;
      "raw-array-to-list", t_top;
      "raw-array-fold", t_top;
      "raw-array", t_record [
        "make" , t_forall1 (fun a -> t_arrow [t_array a] @@ t_array a);
        "make0", t_forall1 (fun a -> t_arrow [] @@ t_array a);
        "make1", t_forall1 (fun a -> t_arrow [a] @@ t_array a);
        "make2", t_forall1 (fun a -> t_arrow [a; a] @@ t_array a);
        "make3", t_forall1 (fun a -> t_arrow [a; a; a] @@ t_array a);
        "make4", t_forall1 (fun a -> t_arrow [a; a; a; a] @@ t_array a);
        "make5", t_forall1 (fun a -> t_arrow [a; a; a; a; a] @@ t_array a);
      ];
      "ref-get", t_top;
      "ref-set", t_top;
      "ref-freeze", t_top;
      "equal-always", t_pred2;
      "equal-always3", t_top;
      "equal-now", t_pred2;
      "equal-now3", t_top;
      "identical", t_pred2;
      "identical3", t_top;
      "exn-unwrap", t_top;
    ]

let no_builtins =
  let open CompileEnvironment in
  let open Globals in
  let open PyretUtils in
  CompileEnvironment(Globals(StringDict.empty, StringDict.empty), StringDict.empty)

let minimal_builtins =
  let open CompileEnvironment in
  let open Globals in
  let open PyretUtils in
  CompileEnvironment(Globals(runtime_builtins, runtime_types), StringDict.empty)

let standard_globals = Globals.Globals(runtime_builtins, runtime_types)
let standard_builtins =
  let open CompileEnvironment in
  let open Globals in
  let open PyretUtils in
  CompileEnvironment(standard_globals, StringDict.empty)

let minimal_imports = ExtraImports.ExtraImports([])

let standard_imports =
  let open Dependency in
  let open ExtraImport in
  ExtraImports.ExtraImports([
      ExtraImport(Builtin("arrays"), "arrays", [
          "array";
          "build-array";
          "array-from-list";
          "is-array";
          "array-of";
          "array-set-now";
          "array-get-now";
          "array-length";
          "array-to-list-now";
        ], ["Array"]);

      ExtraImport(Builtin("lists"), "lists", [
          "list";
          "is-empty";
          "is-link";
          "empty";
          "link";
          "range";
          "range-by";
          "repeat";
          "filter";
          "partition";
          "split-at";
          "any";
          "find";
          "map";
          "map2";
          "map3";
          "map4";
          "map_n";
          "map2_n";
          "map3_n";
          "map4_n";
          "each";
          "each2";
          "each3";
          "each4";
          "each_n";
          "each2_n";
          "each3_n";
          "each4_n";
          "fold";
          "fold2";
          "fold3";
          "fold4";
        ], ["List"]);

      ExtraImport(Builtin("option"), "option", [
          "Option";
          "is-none";
          "is-some";
          "none";
          "some";
        ], ["Option"]);

      ExtraImport(Builtin("error"), "error", [], []);

      ExtraImport(Builtin("sets"), "sets",[
          "set";
          "tree-set";
          "list-set";
        ], ["Set"]);
  ])
