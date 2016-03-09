module ED = ErrorDisplay

type loc = Ast.loc

type t =
    WFErr of string * loc
  | WFErrSplit of string * loc list
  | ReservedName of loc * string
  | ZeroFraction of loc * Ast.expr
  | UnderscoreAsExpr of loc
  | UnderscoreAsAnn of loc
  | UnboundId of Ast.expr
  | UnboundVar of string * loc
  | UnboundTypeId of Ast.ann
  | UnexpectedTypeVar of loc * Ast.name
  | PointlessVar of loc
  | PointlessRec of loc
  | PointlessShadow of loc
  | BadAssignment of string * loc * loc
  | MixedIdVar of string * loc * loc
  | ShadowId of string * loc * loc
  | DuplicateId of string * loc * loc
  | DuplicateField of string * loc * loc
  | IncorrectType of string * loc * string * loc
  | BadTypeInstantiation of int * int * loc
  | IncorrectNumberOfArgs of loc
  | ApplyNonFunction of loc * Ast.ann
  | ObjectMissingField of string * string * loc * loc
  | UnneccesaryBranch of string * loc * string * loc
  | UnneccesaryElseBranch of string * loc
  | NonExhaustivePattern of string list * string * loc
  | CantMatchOn of string * loc
  | IncorrectNumberOfBindings of string * loc * int * int
  | CasesSingletonMismatch of string * loc * bool
  | GivenParameters of string * loc
  | UnableToInstantiate of loc
  | CantTypecheck of string
  | Unsupported of string * loc
  | NoModule of loc * string

let render_reason =
  let draw_and_highlight l = ED.LocDisplay(l,"error-highlight",ED.Loc(l)) in
  function
  | WFErr(msg,loc) ->
    ED.error [ED.para [ED.Text("Well-formedness:");ED.Text(msg);ED.Text("at")]; draw_and_highlight loc]
  | _ -> failwith "NYI (render_reason)"
