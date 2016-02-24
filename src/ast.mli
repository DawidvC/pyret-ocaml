type loc = {
  source : string;
  start_line : int;
  start_col : int;
  start_char : int;
  end_line : int;
  end_col : int;
  end_char : int;
}
val dummy_loc : loc
val use_dummy : bool ref
val make_loc : Lexing.position -> Lexing.position -> loc
type name =
    SUnderscore of loc
  | SName of loc * string
  | SGlobal of string
  | STypeGlobal of string
  | SAtom of string * int
val name_key : name -> string
val compare_names : name -> name -> int
val make_name : int -> string -> name
val global_names : string -> name
type program = SProgram of loc * provide * provide_types * import list * expr
and import_type =
    SFileImport of loc * string
  | SConstImport of loc * string
  | SSpecialImport of loc * string * string list
and import =
    SInclude of loc * import_type
  | SImport of loc * import_type * name
  | SImportTypes of loc * import_type * name * name
  | SImportFields of loc * name list * import_type
  | SImportComplete of loc * name list * name list * import_type * name *
      name
and provided_value = PValue of loc * name * ann
and provided_alias = PAlias of loc * name * name * import_type option
and provided_datatype = PData of loc * name * import_type option
and provide =
    SProvide of loc * expr
  | SProvideComplete of loc * provided_value list * provided_alias list *
      provided_datatype list
  | SProvideAll of loc
  | SProvideNone of loc
and provide_types =
    SProvideTypes of loc * a_field list
  | SProvideTypesAll of loc
  | SProvideTypesNone of loc
and hint = HUseLoc of loc
and let_bind = SLetBind of loc * bind * expr | SVarBind of loc * bind * expr
and letrec_bind = SLetrecBind of loc * bind * expr
and type_let_bind =
    STypeBind of loc * name * ann
  | SNewtypeBind of loc * name * name
and defined_value = SDefinedValue of string * expr
and defined_type = SDefinedType of string * ann
and expr =
    SModule of loc * expr * defined_value list * defined_type list * 
      expr * a_field list * expr
  | STypeLetExpr of loc * type_let_bind list * expr
  | SLetExpr of loc * let_bind list * expr
  | SLetrec of loc * letrec_bind list * expr
  | SHintExp of loc * hint list * expr
  | SInstantiate of loc * expr * ann list
  | SBlock of loc * expr list
  | SUserBlock of loc * expr
  | SFun of loc * string * name list * bind list * ann * string * expr *
      expr option
  | SType of loc * name * ann
  | SNewtype of loc * name * name
  | SVar of loc * bind * expr
  | SRec of loc * bind * expr
  | SLet of loc * bind * expr * bool
  | SRef of loc * ann option
  | SContract of loc * name * ann
  | SWhen of loc * expr * expr
  | SAssign of loc * name * expr
  | SIfPipe of loc * if_pipe_branch list
  | SIfPipeElse of loc * if_pipe_branch list * expr
  | SIf of loc * if_branch list
  | SIfElse of loc * if_branch list * expr
  | SCases of loc * ann * expr * cases_branch list
  | SCasesElse of loc * ann * expr * cases_branch list * expr
  | SOp of loc * string * expr * expr
  | SCheckTest of loc * check_op * expr option * expr * expr option
  | SCheckExpr of loc * expr * ann
  | SParen of loc * expr
  | SLam of loc * name list * bind list * ann * string * expr * expr option
  | SMethod of loc * name list * bind list * ann * string * expr *
      expr option
  | SExtend of loc * expr * member list
  | SUpdate of loc * expr * member list
  | SObj of loc * member list
  | SArray of loc * expr list
  | SConstruct of loc * construct_modifier * expr * expr list
  | SApp of loc * expr * expr list
  | SPrimApp of loc * string * expr list
  | SPrimVal of loc * string
  | SId of loc * name
  | SIdVar of loc * name
  | SIdLetrec of loc * name * bool
  | SUndefined of loc
  | SSrcloc of loc * loc
  | SNum of loc * Num.num
  | SFrac of loc * int * int
  | SBool of loc * bool
  | SStr of loc * string
  | SDot of loc * expr * string
  | SGetBang of loc * expr * string
  | SBracket of loc * expr * expr
  | SData of loc * string * name list * expr list * variant list *
      member list * expr option
  | SDataExpr of loc * string * name * name list * expr list * variant list *
      member list * expr option
  | SFor of loc * expr * for_bind list * ann * expr
  | SCheck of loc * string option * expr * bool
and construct_modifier = SConstructNormal | SConstructLazy
and bind = SBind of loc * bool * name * ann
and member =
    SDataField of loc * string * expr
  | SMutableField of loc * string * ann * expr
  | SMethodField of loc * string * name list * bind list * ann * string *
      expr * expr option
and for_bind = SForBind of loc * bind * expr
and variant_member_type = SNormal | SMutable
and variant_member = SVariantMember of loc * variant_member_type * bind
and variant =
    SVariant of loc * loc * string * variant_member list * member list
  | SSingletonVariant of loc * string * member list
and datatype_variant =
    SDatatypeVariant of loc * string * variant_member list * constructor
  | SDatatypeSingletonVariant of loc * string * constructor
and constructor = SDatatypeConstructor of loc * string * expr
and if_branch = SIfBranch of loc * expr * expr
and if_pipe_branch = SIfPipeBranch of loc * expr * expr
and cases_bind_type = SCasesBindRef | SCasesBindNormal
and cases_bind = SCasesBind of loc * cases_bind_type * bind
and cases_branch =
    SCasesBranch of loc * loc * string * cases_bind list * expr
  | SSingletonCasesBranch of loc * loc * string * expr
and check_op =
    SOpIs
  | SOpIsOp of string
  | SOpIsNot
  | SOpIsNotOp of string
  | SOpSatisfies
  | SOpSatisfiesNot
  | SOpRaises
  | SOpRaisesOther
  | SOpRaisesNot
  | SOpRaisesSatisfies
  | SOpRaisesViolates
and ann =
    ABlank
  | AAny
  | AName of loc * name
  | ATypeVar of loc * name
  | AArrow of loc * ann list * ann * bool
  | AMethod of loc * ann list * ann
  | ARecord of loc * a_field list
  | AApp of loc * ann * ann list
  | APred of loc * ann * expr
  | ADot of loc * name * string
  | AChecked of ann * ann
and a_field = AField of loc * string * ann
type bind_type = BTNormal | BTData
type type_id = TypeID of bind_type * name
val make_checker_name : string -> string
val bind_id : bind -> name
val binding_type_ids : expr -> type_id list
val block_type_ids : expr -> type_id list
val binding_ids : expr -> name list
val block_ids : expr -> name list
val toplevel_ids : program -> name list
val tosource : program -> PPrint.pprintdoc
val prog_to_string : program -> string
