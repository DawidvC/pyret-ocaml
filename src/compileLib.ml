module A = Ast
module SD = Map.Make(String)
module CS = CompileStructs
module W = WellFormed
module D = Desugar
module CH = DesugarCheck
module AU = AstUtils

type pyret_code =
    PyretString of string
  | PyretAst of A.program

type 'a loadable =
    ModuleAsString of CS.Provides.t * CS.CompileEnvironment.t * CS.CompileResult(CS.Provides).t
  | PreLoaded of CS.Provides.t * CS.CompileEnvironment.t * 'a
