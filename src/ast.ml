(** Implementation of syntax types *)
open PPrint
open Sexplib
open Printf

(* TODO: Fix (String.sub ...) calls to be less unwieldly *)
(* Pretty-Printing Values *)
let indent = 2
let break_one = sbreak 1
let str_any = str "Any"
let str_arrow = str "->"
let str_arrowspace = str "-> "
let str_as = str "as"
let str_blank = str ""
let str_let = str "let"
let str_type_let = str "type-let"
let str_letrec = str "letrec"
let str_block = str "block:"
let str_brackets = str "[list: ]"
let str_cases = str "cases"
let str_caret = str "^"
let str_checkcolon = str "check:"
let str_examplescolon = str "examples:"
let str_colon = str ":"
let str_coloncolon = str "::"
let str_colonspace = str ": "
let str_comment = str "# "
let str_constructor = str "with constructor"
let str_data = str "data "
let str_data_expr = str "data-expr "
let str_datatype = str "datatype "
let str_deriving = str "deriving "
let str_doc = str "doc: "
let str_elsebranch = str "| else =>"
let str_elsecolon = str "else:"
let str_otherwisecolon = str "otherwise:"
let str_elsespace = str "else "
let str_end = str "end"
let str_except = str "except"
let str_for = str "for "
let str_from = str "from"
let str_fun = str "fun"
let str_lam = str "lam"
let str_if = str "if "
let str_askcolon = str "ask:"
let str_import = str "import"
let str_include = str "include"
let str_method = str "method"
let str_mutable = str "mutable"
let str_period = str "."
let str_bang = str "!"
let str_pipespace = str "| "
let str_provide = str "provide"
let str_provide_types = str "provide-types"
let str_provide_star = str "provide *"
let str_provide_types_star = str "provide-types *"
let str_sharing = str "sharing:"
let str_space = str " "
let str_spacecolonequal = str " :="
let str_spaceequal = str " ="
let str_thencolon = str "then:"
let str_thickarrow = str "=>"
let str_use_loc = str "UseLoc"
let str_var = str "var "
let str_rec = str "rec "
let str_newtype = str "type "
let str_type = str "type "
let str_val = str "val "
let str_when = str "when"
let str_where = str "where:"
let str_with = str "with:"
let str_is = str "is"
let str_is_not = str "is-not"
let str_satisfies = str "satisfies"
let str_satisfies_not = str "violates"
let str_raises = str "raises"
let str_raises_other = str "raises-other-than"
let str_raises_not = str "does-not-raise"
let str_raises_satisfies = str "raises-satisfies"
let str_raises_violates = str "raises-violates"
let str_percent = str "%"

(** Source Location Type *)
type loc = {
  source : string;
  start_line : int;
  start_col : int;
  start_char : int;
  end_line : int;
  end_col : int;
  end_char : int }

let use_dummy = ref false

let dummy_loc : loc = {source="dummy"; start_line=(-1); start_col=(-1); start_char=(-1);
                 end_line=0; end_col=0; end_char=0}

let make_loc (start : Lexing.position) (ending : Lexing.position) : loc =
  if !use_dummy then (* For Parser Tests *)
    dummy_loc
  else
    {source     = start.Lexing.pos_fname;
     start_line = start.Lexing.pos_lnum;
     start_col  = start.Lexing.pos_cnum;
     start_char = start.Lexing.pos_bol;
     end_line   = ending.Lexing.pos_lnum;
     end_col    = ending.Lexing.pos_cnum;
     end_char   = ending.Lexing.pos_bol}

(** Syntax Type for names *)
type name =
    SUnderscore of loc (** Location of underscore *)
  | SName of loc * string (** Location of name and name itself *)
  | SGlobal of string (** Global names (string is name) *)
  | STypeGlobal of string (** Global type names (string is name) *)
  | SAtom of string * int (** Atomic name; (base, serial) *)

(** Returns key representing the given name *)
let name_key n = match n with
  | SUnderscore _ -> "underscore#"
  | SName(_,s) -> "name#" ^ s
  | SGlobal(s) -> "global#" ^ s
  | STypeGlobal(s) -> "tglobal#" ^ s
  | SAtom(b,s) -> "atom#" ^ b ^ "#" ^ (string_of_int s)

(** Comparison function for name types *)
let compare_names n1 n2 = String.compare (name_key n1) (name_key n2)

let make_name start =
  let count = ref start in
  let atom base =
    count := !count + 1;
    SAtom(base, !count) in
  atom

let global_names = make_name 0


(** Syntax Type for program *)
type program = SProgram of loc * provide * provide_types * import list * expr

(** Data definition for different types of imports *)
and import_type =
    SFileImport of loc * string (** location and file *)
  | SConstImport of loc * string (** location and module *)
  | SSpecialImport of loc * string * string list (** location, kind, and args *)

(** Syntax Type for import statements *)
and import =
    SInclude of loc * import_type
  | SImport of loc * import_type * name
  | SImportTypes of loc * import_type * name * name (** (location,type,name,types) *)
  | SImportFields of loc * name list * import_type
  | SImportComplete of loc * name list * name list * import_type * name * name (** (location,values,types,import-type,vals-name,types-name) *)

(** Simple provided value
    (INVARIANT: all a-times in Ann are defined in the lists of ProvidedAlias or ProvidedDatatype) *)
and provided_value = PValue of loc * name * ann

(** Provided aliased name *)
and provided_alias = PAlias of loc * name * name * import_type option (** (location,in-name,out-name,mod) *)

(** Provided data definition *)
and provided_datatype = PData of loc * name * import_type option

(** Syntax Type for provide statement *)
and provide =
    SProvide of loc * expr
  | SProvideComplete of loc * provided_value list * provided_alias list * provided_datatype list
  | SProvideAll of loc
  | SProvideNone of loc

(** Syntax Type for provide-types statement *)
and provide_types =
    SProvideTypes of loc * a_field list
  | SProvideTypesAll of loc
  | SProvideTypesNone of loc

(* TODO(Philip): What are these? *)
(** Syntax type for hints *)
and hint = HUseLoc of loc

(** Syntax type for non-recursive let bindings *)
and let_bind =
    SLetBind of loc * bind * expr
  | SVarBind of loc * bind * expr

(** Syntax type for recursive let bindings *)
and letrec_bind =
    SLetrecBind of loc * bind * expr

(** Syntax type for type aliasing/creation *)
and type_let_bind =
    STypeBind of loc * name * ann
  | SNewtypeBind of loc * name * name (** (loc,name,namet) *)

(** Syntax type for value definitions *)
and defined_value =
    SDefinedValue of string * expr

(** Syntax type for type definitions *)
and defined_type =
    SDefinedType of string * ann

(** Syntax type for expressions *)
and expr =
    SModule of loc * expr * defined_value list * defined_type list * expr * a_field list * expr (** (loc,answer,defined-values,defined-types,provided-values,provided-types,checks) *)
  | STypeLetExpr of loc * type_let_bind list * expr
  | SLetExpr of loc * let_bind list * expr
  | SLetrec of loc * letrec_bind list * expr
  | SHintExp of loc * hint list * expr
  | SInstantiate of loc * expr * ann list
  | SBlock of loc * expr list
  | SUserBlock of loc * expr
  | SFun of loc * string * name list * bind list * ann * string * expr * expr option (** (loc,name,params,args,ann,doc,body,_check) *)
  | SType of loc * name * ann
  | SNewtype of loc * name * name (** (loc,name,namet) *)
  | SVar of loc * bind * expr
  | SRec of loc * bind * expr
  | SLet of loc * bind * expr * bool (** (loc,name,value,keyword-val) *)
  | SRef of loc * ann option
  | SContract of loc * name * ann
  | SWhen of loc * expr * expr (** (loc,test,block) *)
  | SAssign of loc * name * expr
  | SIfPipe of loc * if_pipe_branch list
  | SIfPipeElse of loc * if_pipe_branch list * expr (** (loc,branches,_else) *)
  | SIf of loc * if_branch list
  | SIfElse of loc * if_branch list * expr (** (loc,branches,_else) *)
  | SCases of loc * ann * expr * cases_branch list
  | SCasesElse of loc * ann * expr * cases_branch list * expr (** (loc,typ,val,branches,_else) *)
                  (* FIXME: Change SOp to use some Op variant *)
  | SOp of loc * string * expr * expr (** (loc,op,left,right) *)
  | SCheckTest of loc * check_op * expr option * expr * expr option (** (loc,op,refinement,left,right) *)
  | SCheckExpr of loc * expr * ann
  | SParen of loc * expr (** Parenthesized expression *)
  | SLam of loc * name list * bind list * ann * string * expr * expr option (** (loc, type parameters, value parameters, return type, doc, body, _check) *)
  | SMethod of loc * name list * bind list * ann * string * expr * expr option (** (loc, type parameters, value parameters, return type, doc, body, _check) *)
  | SExtend of loc * expr * member list (** Object extension; (loc, super, fields) *)
  | SUpdate of loc * expr * member list (** Object update (e.g. `x!{foo:2}') (loc,super,fields) *)
  | SObj of loc * member list
  | SArray of loc * expr list
  | SConstruct of loc * construct_modifier * expr * expr list
  | SApp of loc * expr * expr list
  | SPrimApp of loc * string * expr list
  | SPrimVal of loc * string
  | SId of loc * name
  | SIdVar of loc * name
  | SIdLetrec of loc * name * bool (** (loc,id,safe) *)
  | SUndefined of loc
  | SSrcloc of loc * loc
  | SNum of loc * BatNum.num
  | SFrac of loc * int * int
  | SBool of loc * bool
  | SStr of loc * string
  | SDot of loc * expr * string
  | SGetBang of loc * expr * string
  | SBracket of loc * expr * expr
  | SData of loc * string * name list * expr list * variant list * member list * expr option (** (loc,name,params,mixins,variants,shared-members,_check) *)
  | SDataExpr of loc * string * name * name list * expr list * variant list * member list * expr option (** (loc,name,namet,params,mixins,variants,shared-members,_check) *)
  | SFor of loc * expr * for_bind list * ann * expr (** (loc,iterator,bindings,ann,body) *)
  | SCheck of loc * string option * expr * bool (** (loc,name,body,keyword-check) *)

(** Types of construction *)
and construct_modifier =
    SConstructNormal
  | SConstructLazy

(** Syntax type for bindings *)
and bind =
    SBind of loc * bool * name * ann (** (loc, shadows?, id, ann)*)

(** Syntax type for object fields *)
and member =
    SDataField of loc * string * expr (** (loc,name,value) *)
  | SMutableField of loc * string * ann * expr (** (loc,name,ann,value) *)
  | SMethodField of loc * string * name list * bind list * ann * string * expr * expr option

(** Syntax type for bindings in for expression *)
and for_bind =
    SForBind of loc * bind * expr

(** Types of data variants *)
and variant_member_type =
    SNormal
  | SMutable

(** Syntax type for data variant members *)
and variant_member =
    SVariantMember of loc * variant_member_type * bind

(** Syntax type for data variants *)
and variant =
    SVariant of loc * loc * string * variant_member list * member list (** (loc,constr-loc,name,members,with-members) *)
  | SSingletonVariant of loc * string * member list

(** Syntax type for if expression branches*)
and if_branch =
    SIfBranch of loc * expr * expr (** (loc, test, body) *)

(** Syntax type for ask expression branches *)
and if_pipe_branch =
    SIfPipeBranch of loc * expr * expr (** (loc, test, body) *)

(** Syntax type for cases binding types *)
and cases_bind_type =
    SCasesBindRef
  | SCasesBindNormal

(** Syntax type for cases bindings *)
and cases_bind =
    SCasesBind of loc * cases_bind_type * bind

(** Syntax type for cases branches *)
and cases_branch =
    SCasesBranch of loc * loc * string * cases_bind list * expr (** (loc,pat-loc,name,args,body) *)
  | SSingletonCasesBranch of loc * loc * string * expr (** (loc,pat-loc,name,body) *)

(** Syntax type for testing operators *)
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

(** Syntax type for annotations *)
and ann =
    ABlank
  | AAny
  | AName of loc * name
  | ATypeVar of loc * name
  | AArrow of loc * ann list * ann * bool (** (loc,args,ret,use-parens) *)
  | AMethod of loc * ann list * ann (** (loc,args,ret) *)
  | ARecord of loc * a_field list
  | AApp of loc * ann * ann list (** (loc,ann,args) *)
  | APred of loc * ann * expr
  | ADot of loc * name * string
  | AChecked of ann * ann

(** Syntax type for annotations on records *)
and a_field = AField of loc * string * ann

(** Different types of type identifier bindings *)
type bind_type = BTNormal | BTData

(** Type for type identifier bindings *)
type type_id = TypeID of bind_type * name

let make_checker_name name = "is-" ^ name

let bind_id b = match b with
  | SBind(_,_,id,_) -> id

let binding_type_ids stmt = match stmt with
  | SNewtype(_,name,_) -> [TypeID(BTNormal,name)]
  | SType(_,name,_) -> [TypeID(BTNormal,name)]
  | SData(l,name,_,_,_,_,_) -> [TypeID(BTData,SName(l,name))]
  | _ -> []

let block_type_ids b = match b with
  | SBlock(_,stmts) -> List.flatten (List.map binding_type_ids stmts)
  | _ -> failwith "Non-block given to block_type_ids"

let binding_ids stmt =
  let variant_ids variant = match variant with
    | SVariant(_,l2,name,_,_) -> [SName(l2,name);SName(l2,make_checker_name name)]
    | SSingletonVariant(l,name,_) -> [SName(l,name);SName(l,make_checker_name name)] in
  match stmt with
  | SLet(_,b,_,_) -> [bind_id b]
  | SVar(_,b,_) -> [bind_id b]
  | SRec(_,b,_) -> [bind_id b]
  | SFun(l,name,_,_,_,_,_,_) -> [SName(l,name)]
  | SData(l,name,_,_,variants,_,_) ->
    [SName(l,name);SName(l,make_checker_name name)]
    @ (List.flatten (List.map variant_ids variants))
  | _ -> []

let block_ids b = match b with
  | SBlock(_,stmts) -> List.flatten(List.map binding_ids stmts)
  | _ -> failwith "Non-block given to block_ids"

let toplevel_ids prog = match prog with
  | SProgram(_,_,_,_,b) -> block_ids b

class virtual ['name_ret,
               'program_ret,
               'import_type_ret,
               'import_ret,
               'provide_ret,
               'provide_types_ret,
               'let_bind_ret,
               'letrec_bind_ret,
               'type_let_bind_ret,
               'defined_value_ret,
               'defined_type_ret,
               'expr_ret,
               'bind_ret,
               'member_ret,
               'for_bind_ret,
               'variant_member_ret,
               'variant_ret,
               'if_branch_ret,
               'if_pipe_branch_ret,
               'cases_bind_ret,
               'cases_branch_ret,
               'ann_ret,
               'a_field_ret] ast_visitor_base = object(self)
    method visit_name = function
      | SUnderscore(l) -> self#s_underscore l
      | SName(l,s) -> self#s_name (l,s)
      | SGlobal(s) -> self#s_global s
      | STypeGlobal(s) -> self#s_type_global s
      | SAtom(s,i) -> self#s_atom (s,i)

    method visit_program = function
      | SProgram(l,p,pt,il,e) -> self#s_program (l,p,pt,il,e)

    method visit_import_type = function
      | SFileImport(l,s) -> self#s_file_import(l,s)
      | SConstImport(l,s) -> self#s_const_import(l,s)
      | SSpecialImport(l,s,sl) -> self#s_special_import(l,s,sl)

    method visit_import = function
      | SInclude(a,b) -> self#s_include(a,b)
      | SImport(a,b,c) -> self#s_import(a,b,c)
      | SImportTypes(a,b,c,d) -> self#s_import_types(a,b,c,d)
      | SImportFields(a,b,c) -> self#s_import_fields(a,b,c)
      | SImportComplete(a,b,c,d,e,f) -> self#s_import_complete(a,b,c,d,e,f)

    method visit_provide = function
      | SProvide(a,b) -> self#s_provide(a,b)
      | SProvideComplete(a,b,c,d) -> self#s_provide_complete(a,b,c,d)
      | SProvideAll(a) -> self#s_provide_all(a)
      | SProvideNone(a) -> self#s_provide_none(a)

    method visit_provide_types = function
      | SProvideTypes(a,b) -> self#s_provide_types(a,b)
      | SProvideTypesAll(a) -> self#s_provide_types_all(a)
      | SProvideTypesNone(a) -> self#s_provide_types_none(a)

    method visit_let_bind = function
      | SLetBind (a,b,c) -> self#s_let_bind(a,b,c)
      | SVarBind (a,b,c) -> self#s_var_bind(a,b,c)

    method visit_letrec_bind = function
      | SLetrecBind(l,b,e) -> self#s_letrec_bind(l,b,e)

    method visit_type_let_bind = function
      | STypeBind(l,n,a) -> self#s_type_bind(l,n,a)
      | SNewtypeBind(l,n1,n2) -> self#s_newtype_bind(l,n1,n2)

    method visit_defined_value = function
      | SDefinedValue(s,e) -> self#s_defined_value(s,e)

    method visit_defined_type = function
      | SDefinedType(s,a) -> self#s_defined_type(s,a)

    method visit_expr = function
      | SModule(a,b,c,d,e,f,g) -> self#s_module(a,b,c,d,e,f,g)
      | STypeLetExpr(a,b,c) -> self#s_type_let_expr(a,b,c)
      | SLetExpr(a,b,c) -> self#s_let_expr(a,b,c)
      | SLetrec(a,b,c) -> self#s_letrec(a,b,c)
      | SHintExp(a,b,c) -> self#s_hint_exp(a,b,c)
      | SInstantiate(a,b,c) -> self#s_instantiate(a,b,c)
      | SBlock(a,b) -> self#s_block(a,b)
      | SUserBlock(a,b) -> self#s_user_block(a,b)
      | SFun(a,b,c,d,e,f,g,h) -> self#s_fun(a,b,c,d,e,f,g,h)
      | SType(a,b,c) -> self#s_type(a,b,c)
      | SNewtype(a,b,c) -> self#s_newtype(a,b,c)
      | SVar(a,b,c) -> self#s_var(a,b,c)
      | SRec(a,b,c) -> self#s_rec(a,b,c)
      | SLet(a,b,c,d) -> self#s_let(a,b,c,d)
      | SRef(a,b) -> self#s_ref(a,b)
      | SContract(a,b,c) -> self#s_contract(a,b,c)
      | SWhen(a,b,c) -> self#s_when(a,b,c)
      | SAssign(a,b,c) -> self#s_assign(a,b,c)
      | SIfPipe(a,b) -> self#s_if_pipe(a,b)
      | SIfPipeElse(a,b,c) -> self#s_if_pipe_else(a,b,c)
      | SIf(a,b) -> self#s_if(a,b)
      | SIfElse(a,b,c) -> self#s_if_else(a,b,c)
      | SCases(a,b,c,d) -> self#s_cases(a,b,c,d)
      | SCasesElse(a,b,c,d,e) -> self#s_cases_else(a,b,c,d,e)
      | SOp(a,b,c,d) -> self#s_op(a,b,c,d)
      | SCheckTest(a,b,c,d,e) -> self#s_check_test(a,b,c,d,e)
      | SCheckExpr(a,b,c) -> self#s_check_expr(a,b,c)
      | SParen(a,b) -> self#s_paren(a,b)
      | SLam(a,b,c,d,e,f,g) -> self#s_lam(a,b,c,d,e,f,g)
      | SMethod(a,b,c,d,e,f,g) -> self#s_method(a,b,c,d,e,f,g)
      | SExtend(a,b,c) -> self#s_extend(a,b,c)
      | SUpdate(a,b,c) -> self#s_update(a,b,c)
      | SObj(a,b) -> self#s_obj(a,b)
      | SArray(a,b) -> self#s_array(a,b)
      | SConstruct(a,b,c,d) -> self#s_construct(a,b,c,d)
      | SApp(a,b,c) -> self#s_app(a,b,c)
      | SPrimApp(a,b,c) -> self#s_prim_app(a,b,c)
      | SPrimVal(a,b) -> self#s_prim_val(a,b)
      | SId(a,b) -> self#s_id(a,b)
      | SIdVar(a,b) -> self#s_id_var(a,b)
      | SIdLetrec(a,b,c) -> self#s_id_letrec(a,b,c)
      | SUndefined(a) -> self#s_undefined(a)
      | SSrcloc(a,b) -> self#s_srcloc(a,b)
      | SNum(a,b) -> self#s_num(a,b)
      | SFrac(a,b,c) -> self#s_frac(a,b,c)
      | SBool(a,b) -> self#s_bool(a,b)
      | SStr(a,b) -> self#s_str(a,b)
      | SDot(a,b,c) -> self#s_dot(a,b,c)
      | SGetBang(a,b,c) -> self#s_get_bang(a,b,c)
      | SBracket(a,b,c) -> self#s_bracket(a,b,c)
      | SData(a,b,c,d,e,f,g) -> self#s_data(a,b,c,d,e,f,g)
      | SDataExpr(a,b,c,d,e,f,g,h) -> self#s_data_expr(a,b,c,d,e,f,g,h)
      | SFor(a,b,c,d,e) -> self#s_for(a,b,c,d,e)
      | SCheck(a,b,c,d) -> self#s_check(a,b,c,d)

    method visit_bind = function
      | SBind(l,b,n,a) -> self#s_bind(l,b,n,a)

    method visit_member = function
      | SDataField(l,s,e) -> self#s_data_field(l,s,e)
      | SMutableField(l,s,a,e) -> self#s_mutable_field(l,s,a,e)
      | SMethodField(a,b,c,d,e,f,g,h) -> self#s_method_field(a,b,c,d,e,f,g,h)

    method visit_for_bind = function
      | SForBind(l,b,e) -> self#s_for_bind(l,b,e)

    method visit_variant_member = function
      | SVariantMember(l,v,b) -> self#s_variant_member(l,v,b)

    method visit_variant = function
      | SVariant(a,b,c,d,e) -> self#s_variant(a,b,c,d,e)
      | SSingletonVariant(l,s,m) -> self#s_singleton_variant(l,s,m)

    method visit_if_branch = function
      | SIfBranch(l,e1,e2) -> self#s_if_branch(l,e1,e2)

    method visit_if_pipe_branch = function
      | SIfPipeBranch(l,e1,e2) -> self#s_if_pipe_branch(l,e1,e2)

    method visit_cases_bind = function
      | SCasesBind(l,c,b) -> self#s_cases_bind(l,c,b)

    method visit_cases_branch = function
      | SCasesBranch(a,b,c,d,e) -> self#s_cases_branch(a,b,c,d,e)
      | SSingletonCasesBranch(a,b,c,d) -> self#s_singleton_cases_branch(a,b,c,d)

    method visit_ann = function
      | ABlank  -> self#a_blank()
      | AAny  -> self#a_any()
      | AName (a,b) -> self#a_name(a,b)
      | ATypeVar (a,b) -> self#a_type_var(a,b)
      | AArrow (a,b,c,d) -> self#a_arrow(a,b,c,d)
      | AMethod (a,b,c) -> self#a_method(a,b,c)
      | ARecord (a,b) -> self#a_record(a,b)
      | AApp (a,b,c) -> self#a_app(a,b,c)
      | APred (a,b,c) -> self#a_pred(a,b,c)
      | ADot (a,b,c) -> self#a_dot(a,b,c)
      | AChecked (a,b) -> self#a_checked(a,b)

    method visit_a_field = function
      | AField(l,s,a) -> self#a_field(l,s,a)

    method virtual s_underscore : loc -> 'name_ret
    method virtual s_name : (loc * string) -> 'name_ret
    method virtual s_global : string -> 'name_ret
    method virtual s_type_global : string -> 'name_ret
    method virtual s_atom : (string * int) -> 'name_ret

    method virtual s_program : (loc * provide * provide_types * import list * expr) -> 'program_ret

    method virtual s_file_import : (loc * string) -> 'import_type_ret
    method virtual s_const_import : (loc * string) -> 'import_type_ret
    method virtual s_special_import : (loc * string * string list) -> 'import_type_ret

    method virtual s_include : (loc * import_type) -> 'import_ret
    method virtual s_import : (loc * import_type * name) -> 'import_ret
    method virtual s_import_types : (loc * import_type * name * name) -> 'import_ret
    method virtual s_import_fields : (loc * name list * import_type) -> 'import_ret
    method virtual s_import_complete : (loc * name list * name list *
                                        import_type * name * name) -> 'import_ret

    method virtual s_provide : (loc * expr) -> 'provide_ret
    method virtual s_provide_complete : (loc * provided_value list * provided_alias list *
                                         provided_datatype list) -> 'provide_ret
    method virtual s_provide_all : loc -> 'provide_ret
    method virtual s_provide_none : loc -> 'provide_ret

    method virtual s_provide_types : (loc * a_field list) -> 'provide_types_ret
    method virtual s_provide_types_all : loc -> 'provide_types_ret
    method virtual s_provide_types_none : loc -> 'provide_types_ret

    method virtual s_let_bind : (loc * bind * expr) -> 'let_bind_ret
    method virtual s_var_bind : (loc * bind * expr) -> 'let_bind_ret

    method virtual s_letrec_bind : (loc * bind * expr) -> 'letrec_bind_ret

    method virtual s_type_bind : (loc * name * ann) -> 'type_let_bind_ret
    method virtual s_newtype_bind : (loc * name * name) -> 'type_let_bind_ret

    method virtual s_defined_value : (string * expr) -> 'defined_value_ret

    method virtual s_defined_type : (string * ann) -> 'defined_type_ret

    method virtual s_module : (loc * expr * defined_value list * defined_type list *
                               expr * a_field list * expr) -> 'expr_ret
    method virtual s_type_let_expr : (loc * type_let_bind list * expr) -> 'expr_ret
    method virtual s_let_expr : (loc * let_bind list * expr) -> 'expr_ret
    method virtual s_letrec : (loc * letrec_bind list * expr) -> 'expr_ret
    method virtual s_hint_exp : (loc * hint list * expr) -> 'expr_ret
    method virtual s_instantiate : (loc * expr * ann list) -> 'expr_ret
    method virtual s_block : (loc * expr list) -> 'expr_ret
    method virtual s_user_block : (loc * expr) -> 'expr_ret
    method virtual s_fun : (loc * string * name list * bind list * ann *
                            string * expr * expr option) -> 'expr_ret
    method virtual s_type : (loc * name * ann) -> 'expr_ret
    method virtual s_newtype : (loc * name * name) -> 'expr_ret
    method virtual s_var : (loc * bind * expr) -> 'expr_ret
    method virtual s_rec : (loc * bind * expr) -> 'expr_ret
    method virtual s_let : (loc * bind * expr * bool) -> 'expr_ret
    method virtual s_ref : (loc * ann option) -> 'expr_ret
    method virtual s_contract : (loc * name * ann) -> 'expr_ret
    method virtual s_when : (loc * expr * expr) -> 'expr_ret
    method virtual s_assign : (loc * name * expr) -> 'expr_ret
    method virtual s_if_pipe : (loc * if_pipe_branch list) -> 'expr_ret
    method virtual s_if_pipe_else : (loc * if_pipe_branch list * expr) -> 'expr_ret
    method virtual s_if : (loc * if_branch list) -> 'expr_ret
    method virtual s_if_else : (loc * if_branch list * expr) -> 'expr_ret
    method virtual s_cases : (loc * ann * expr * cases_branch list) -> 'expr_ret
    method virtual s_cases_else : (loc * ann * expr * cases_branch list * expr) -> 'expr_ret
    method virtual s_op : (loc * string * expr * expr) -> 'expr_ret
    method virtual s_check_test : (loc * check_op * expr option * expr * expr option) -> 'expr_ret
    method virtual s_check_expr : (loc * expr * ann) -> 'expr_ret
    method virtual s_paren : (loc * expr) -> 'expr_ret
    method virtual s_lam : (loc * name list * bind list * ann * string *
                            expr * expr option) -> 'expr_ret
    method virtual s_method : (loc * name list * bind list * ann * string *
                               expr * expr option) -> 'expr_ret
    method virtual s_extend : (loc * expr * member list) -> 'expr_ret
    method virtual s_update : (loc * expr * member list) -> 'expr_ret
    method virtual s_obj : (loc * member list) -> 'expr_ret
    method virtual s_array : (loc * expr list) -> 'expr_ret
    method virtual s_construct : (loc * construct_modifier * expr * expr list) -> 'expr_ret
    method virtual s_app : (loc * expr * expr list) -> 'expr_ret
    method virtual s_prim_app : (loc * string * expr list) -> 'expr_ret
    method virtual s_prim_val : (loc * string) -> 'expr_ret
    method virtual s_id : (loc * name) -> 'expr_ret
    method virtual s_id_var : (loc * name) -> 'expr_ret
    method virtual s_id_letrec : (loc * name * bool) -> 'expr_ret
    method virtual s_undefined : loc -> 'expr_ret
    method virtual s_srcloc : (loc * loc) -> 'expr_ret
    method virtual s_num : (loc * BatNum.num) -> 'expr_ret
    method virtual s_frac : (loc * int * int) -> 'expr_ret
    method virtual s_bool : (loc * bool) -> 'expr_ret
    method virtual s_str : (loc * string) -> 'expr_ret
    method virtual s_dot : (loc * expr * string) -> 'expr_ret
    method virtual s_get_bang : (loc * expr * string) -> 'expr_ret
    method virtual s_bracket : (loc * expr * expr) -> 'expr_ret
    method virtual s_data : (loc * string * name list * expr list * variant list * member list *
                             expr option) -> 'expr_ret
    method virtual s_data_expr : (loc * string * name * name list * expr list * variant list *
                                  member list * expr option) -> 'expr_ret
    method virtual s_for : (loc * expr * for_bind list * ann * expr) -> 'expr_ret
    method virtual s_check : (loc * string option * expr * bool) -> 'expr_ret

    method virtual s_bind : (loc * bool * name * ann) -> 'bind_ret

    method virtual s_data_field : (loc * string * expr) -> 'member_ret
    method virtual s_mutable_field : (loc * string * ann * expr) -> 'member_ret
    method virtual s_method_field : (loc * string * name list * bind list * ann * string *
                                     expr * expr option) -> 'member_ret

    method virtual s_for_bind : (loc * bind * expr) -> 'for_bind_ret

    method virtual s_variant_member : (loc * variant_member_type * bind) -> 'variant_member_ret

    method virtual s_variant : (loc * loc * string * variant_member list * member list) -> 'variant_ret
    method virtual s_singleton_variant : (loc * string * member list) -> 'variant_ret

    method virtual s_if_branch : (loc * expr * expr) -> 'if_branch_ret

    method virtual s_if_pipe_branch : (loc * expr * expr) -> 'if_pipe_branch_ret

    method virtual s_cases_bind : (loc * cases_bind_type * bind) -> 'cases_bind_ret

    method virtual s_cases_branch : (loc * loc * string * cases_bind list * expr) -> 'cases_branch_ret
    method virtual s_singleton_cases_branch : (loc * loc * string * expr) -> 'cases_branch_ret

    method virtual a_blank : unit -> 'ann_ret
    method virtual a_any : unit -> 'ann_ret
    method virtual a_name : (loc * name) -> 'ann_ret
    method virtual a_type_var : (loc * name) -> 'ann_ret
    method virtual a_arrow : (loc * ann list * ann * bool) -> 'ann_ret
    method virtual a_method : (loc * ann list * ann) -> 'ann_ret
    method virtual a_record : (loc * a_field list) -> 'ann_ret
    method virtual a_app : (loc * ann * ann list) -> 'ann_ret
    method virtual a_pred : (loc * ann * expr) -> 'ann_ret
    method virtual a_dot : (loc * name * string) -> 'ann_ret
    method virtual a_checked : (ann * ann) -> 'ann_ret

    method virtual a_field : (loc * string * ann) -> 'a_field_ret
end

class default_map_visitor = object(self)
  inherit [name,
           program,
           import_type,
           import,
           provide,
           provide_types,
           let_bind,
           letrec_bind,
           type_let_bind,
           defined_value,
           defined_type,
           expr,
           bind,
           member,
           for_bind,
           variant_member,
           variant,
           if_branch,
           if_pipe_branch,
           cases_bind,
           cases_branch,
           ann,
           a_field] ast_visitor_base
  method private visit_expr_option = function
    | None -> None
    | Some(v) -> Some(self#visit_expr v)

  method private visit_ann_option = function
    | None -> None
    | Some(v) -> Some(self#visit_ann v)

  method s_check_expr _ = failwith "SCheckExpr visitor unimplemented"
  method a_checked _ = failwith "AChecked visitor unimplemented"

  method s_underscore l = SUnderscore(l)
  method s_name(l,s) = SName(l,s)
  method s_type_global s = STypeGlobal(s)
  method s_global s = SGlobal(s)
  method s_atom(b,s) = SAtom(b,s)

  method s_defined_value(n,v) = SDefinedValue(n,self#visit_expr v)
  method s_defined_type(n,t) = SDefinedType(n,self#visit_ann t)

  method s_module(l,a,dv,dt,p,t,c) =
    SModule(l,self#visit_expr a, List.map self#visit_defined_value dv,
            List.map self#visit_defined_type dt,
            self#visit_expr p, List.map self#visit_a_field t, self#visit_expr c)

  method s_program(l,p,pt,i,b) = SProgram(l,self#visit_provide p, self#visit_provide_types pt,
                                          List.map self#visit_import i, self#visit_expr b)

  method s_include(l,it) = SInclude(l,self#visit_import_type it)
  method s_import(l,it,n) = SImport(l,self#visit_import_type it,self#visit_name n)
  method s_import_complete(l,v,t,m,vn,tn) =
    SImportComplete(l,List.map self#visit_name v,
                    List.map self#visit_name v,
                    self#visit_import_type m, self#visit_name vn, self#visit_name tn)

  method s_file_import(l,f) = SFileImport(l,f)
  method s_const_import(l,m) = SConstImport(l,m)
  method s_special_import(l,k,a) = SSpecialImport(l,k,a)
  method s_import_types(l,it,n,t) = SImportTypes(l,it,self#visit_name n, self#visit_name t)
  method s_import_fields(l,f,it) = SImportFields(l,List.map self#visit_name f, it)

  method s_provide_complete(l,v,t,d) = SProvideComplete(l,v,t,d)
  method s_provide(l,e) = SProvide(l,self#visit_expr e)
  method s_provide_all(l) = SProvideAll(l)
  method s_provide_none(l) = SProvideNone(l)

  method s_provide_types(l,anns) = SProvideTypes(l,List.map self#visit_a_field anns)
  method s_provide_types_all(l) = SProvideTypesAll(l)
  method s_provide_types_none(l) = SProvideTypesNone(l)

  method s_bind(l,s,n,a) = SBind(l,s,self#visit_name n, self#visit_ann a)

  method s_var_bind(l,b,e) = SVarBind(l,self#visit_bind b, self#visit_expr e)
  method s_let_bind(l,b,e) = SLetBind(l,self#visit_bind b, self#visit_expr e)

  method s_newtype_bind(l,n,nt) = SNewtypeBind(l,self#visit_name n, self#visit_name nt)
  method s_type_bind(l,n,a) = STypeBind(l,self#visit_name n, self#visit_ann a)

  method s_type_let_expr(l,bnd,b) = STypeLetExpr(l,List.map self#visit_type_let_bind bnd,
                                                 self#visit_expr b)

  method s_let_expr(l,bnd,b) = SLetExpr(l,List.map self#visit_let_bind bnd, self#visit_expr b)

  method s_letrec_bind(l,b,e) = SLetrecBind(l,self#visit_bind b, self#visit_expr e)

  method s_letrec(l,bnd,b) = SLetrec(l,List.map self#visit_letrec_bind bnd, self#visit_expr b)
  method s_hint_exp(l,h,e) = SHintExp(l, h, self#visit_expr e)
  method s_instantiate(l,e,p) = SInstantiate(l,self#visit_expr e, List.map self#visit_ann p)
  method s_block(l,s) = SBlock(l,List.map self#visit_expr s)
  method s_user_block(l,b) = SUserBlock(l,self#visit_expr b)
  method s_fun(l,n,p,ar,an,d,b,c) =
    SFun(l,n,p,List.map self#visit_bind ar,self#visit_ann an, d,self#visit_expr b,
         self#visit_expr_option c)
  method s_type(l,n,a) = SType(l,self#visit_name n, self#visit_ann a)
  method s_newtype(l,n,nt) = SNewtype(l,self#visit_name n, self#visit_name nt)
  method s_var(l,n,v) = SVar(l,self#visit_bind n,self#visit_expr v)
  method s_rec(l,n,v) = SRec(l,self#visit_bind n,self#visit_expr v)
  method s_let(l,n,v,kv) = SLet(l,self#visit_bind n, self#visit_expr v, kv)
  method s_ref(l,a) = SRef(l,self#visit_ann_option a)
  method s_when(l,t,b) = SWhen(l,self#visit_expr t, self#visit_expr b)
  method s_contract(l,n,a) = SContract(l,self#visit_name n, self#visit_ann a)
  method s_assign(l,i,v) = SAssign(l,self#visit_name i, self#visit_expr v)
  method s_if_branch(l,t,b) = SIfBranch(l,self#visit_expr t, self#visit_expr b)
  method s_if_pipe_branch(l,t,b) = SIfPipeBranch(l,self#visit_expr t, self#visit_expr b)
  method s_if(l,b) = SIf(l,List.map self#visit_if_branch b)
  method s_if_else(l,b,e) = SIfElse(l,List.map self#visit_if_branch b, self#visit_expr e)
  method s_if_pipe(l,b) = SIfPipe(l,List.map self#visit_if_pipe_branch b)
  method s_if_pipe_else(l,b,e) = SIfPipeElse(l,List.map self#visit_if_pipe_branch b, self#visit_expr e)
  method s_cases_bind(l,t,b) = SCasesBind(l,t,self#visit_bind b)
  method s_cases_branch(l,pl,n,a,b) =
    SCasesBranch(l,pl,n,List.map self#visit_cases_bind a, self#visit_expr b)
  method s_singleton_cases_branch(l,pl,n,b) =
    SSingletonCasesBranch(l,pl,n,self#visit_expr b)
  method s_cases(l,t,v,b) =
    SCases(l,self#visit_ann t, self#visit_expr v, List.map self#visit_cases_branch b)
  method s_cases_else(l,t,v,b,e) =
    SCasesElse(l,self#visit_ann t, self#visit_expr v,
               List.map self#visit_cases_branch b, self#visit_expr e)
  method s_op(l,o,left,right) = SOp(l,o,self#visit_expr left, self#visit_expr right)
  method s_check_test(l,o,r,lft,rt) =
    SCheckTest(l,o,self#visit_expr_option r, self#visit_expr lft, self#visit_expr_option rt)
  method s_paren(l,e) = SParen(l,self#visit_expr e)
  method s_lam(l,p,ar,an,d,b,c) =
    SLam(l,List.map self#visit_name p, List.map self#visit_bind ar,
         self#visit_ann an, d, self#visit_expr b, self#visit_expr_option c)
  method s_method(l,p,ar,an,d,b,c) =
    SMethod(l,List.map self#visit_name p, List.map self#visit_bind ar,
            self#visit_ann an, d, self#visit_expr b, self#visit_expr_option c)
  method s_extend(l,s,f) = SExtend(l,self#visit_expr s, List.map self#visit_member f)
  method s_update(l,s,f) = SUpdate(l,self#visit_expr s, List.map self#visit_member f)
  method s_obj(l,f) = SObj(l, List.map self#visit_member f)
  method s_array(l,v) = SArray(l, List.map self#visit_expr v)
  method s_construct(l,m,c,v) = SConstruct(l,m,self#visit_expr c, List.map self#visit_expr v)
  method s_app(l,f,a) = SApp(l,self#visit_expr f, List.map self#visit_expr a)
  method s_prim_app(l,f,a) = SPrimApp(l,f,List.map self#visit_expr a)
  method s_prim_val(l,n) = SPrimVal(l,n)
  method s_id(l,i) = SId(l,self#visit_name i)
  method s_id_var(l,i) = SIdVar(l,self#visit_name i)
  method s_id_letrec(l,i,s) = SIdLetrec(l,self#visit_name i, s)
  method s_undefined(l) = SUndefined(l)
  method s_srcloc(l1,l2) = SSrcloc(l1,l2)
  method s_num(l,n) = SNum(l,n)
  method s_frac(l,n,d) = SFrac(l,n,d)
  method s_bool(l,b) = SBool(l,b)
  method s_str(l,s) = SStr(l,s)
  method s_dot(l,o,f) = SDot(l,self#visit_expr o, f)
  method s_get_bang(l,o,f) = SGetBang(l, self#visit_expr o, f)
  method s_bracket(l,o,f) = SBracket(l, self#visit_expr o, self#visit_expr f)
  method s_data(l,n,p,m,v,s,c) =
    SData(l,n,List.map self#visit_name p,
          List.map self#visit_expr m,
          List.map self#visit_variant v,
          List.map self#visit_member s,
          self#visit_expr_option c)
  method s_data_expr(l,n,nt,p,m,v,s,c) =
    SDataExpr(l,n,self#visit_name nt,List.map self#visit_name p,
              List.map self#visit_expr m,
              List.map self#visit_variant v,
              List.map self#visit_member s,
              self#visit_expr_option c)
  method s_for(l,i,bnd,a,b) = SFor(l,self#visit_expr i,
                                   List.map self#visit_for_bind bnd,
                                   self#visit_ann a, self#visit_expr b)
  method s_check(l,n,b,k) = SCheck(l,n,self#visit_expr b, k)

  method s_data_field(l,n,v) = SDataField(l,n,self#visit_expr v)
  method s_mutable_field(l,n,a,v) = SMutableField(l,n,self#visit_ann a, self#visit_expr v)
  method s_method_field(l,n,p,ar,an,d,b,c) =
    SMethodField(l,n,List.map self#visit_name p, List.map self#visit_bind ar,
                 self#visit_ann an, d, self#visit_expr b, self#visit_expr_option c)

  method s_for_bind(l,b,v) = SForBind(l,self#visit_bind b, self#visit_expr v)

  method s_variant_member(l,m,b) = SVariantMember(l,m,self#visit_bind b)

  method s_variant(l,c,n,m,w) =
    SVariant(l,c,n,List.map self#visit_variant_member m,List.map self#visit_member w)
  method s_singleton_variant(l,n,w) =
    SSingletonVariant(l,n,List.map self#visit_member w)

  method a_blank () = ABlank
  method a_any () = AAny
  method a_name(l,i) = AName(l,self#visit_name i)
  method a_type_var(l,i) = ATypeVar(l,self#visit_name i)
  method a_arrow(l,a,r,u) = AArrow(l,List.map self#visit_ann a, self#visit_ann r, u)
  method a_method(l,a,r) = AMethod(l,List.map self#visit_ann a, self#visit_ann r)
  method a_record(l,f) = ARecord(l,List.map self#visit_a_field f)
  method a_app(l,an,ar) = AApp(l,self#visit_ann an, List.map self#visit_ann ar)
  method a_pred(l,a,e) = APred(l,self#visit_ann a, self#visit_expr e)
  method a_dot(l,o,f) = ADot(l,self#visit_name o, f)

  method a_field(l,n,a) = AField(l,n,self#visit_ann a)

end

(* Pretty Printing Functions *)
let name_tosource n = match n with
  | SUnderscore(_) -> str "_"
  | SName(_,s) -> str s
  | SGlobal(s) -> str s
  | STypeGlobal(s) -> str s
  | SAtom(b,s) -> str (b ^ (string_of_int s))

let rec funlam_tosource funtype name params args ann doc body _check =
  let typarams = match params with
    | [] -> mt_doc
    | _ -> surround_separate indent 0 mt_doc langle commabreak rangle (List.map name_tosource params) in
  let arg_list = nest indent
      (surround_separate indent 0 (lparen +^ rparen) lparen commabreak rparen (List.map bind_tosource args)) in
  let ftype = funtype +^ typarams in
  let fname = match (name, ftype) with
    | (None,_) -> mt_doc
    | (Some(n),MtDoc(_,_)) -> str n
    | (Some(n),_) -> ftype +^ str (" " ^ n) in
  let fann = match ann with
    | ABlank -> mt_doc
    | _ -> break_one +^ str_arrowspace +^ (ann_tosource ann) in
  let header = group (fname +^ arg_list +^ fann +^ str_colon) in
  let checker = match _check with
    | None -> mt_doc
    | Some(chk) -> (expr_tosource chk) in
  let footer = match checker with
    | MtDoc(_,_) -> str_end
    | _ -> surround indent 1 str_where checker str_end in
  let docstr = match doc with
    | "" -> mt_doc
    | _ -> str_doc +^ (dquote (str doc)) +^ hardline in
  surround indent 1 header (docstr +^ (expr_tosource body)) footer

and tosource p = match p with
  | SProgram(_,_provide,ptypes,imports,block) ->
    group (vert ([(provide_tosource _provide);(provide_types_tosource ptypes)] @
                 (List.map import_tosource imports) @ [expr_tosource block]))

and import_tosource i = match i with
  | SInclude(_,m) -> flow [str_include; (import_type_tosource m)]
  | SImport(_,file,name) -> flow [str_import;(import_type_tosource file);str_as;(name_tosource name)]
  | SImportTypes(_,file,name,types) ->
    flow [str_import;(import_type_tosource file);str_as;(name_tosource name);comma;(name_tosource types)]
  | SImportFields(_,fields,file) ->
    flow [str_import; flow_map commabreak name_tosource fields; str_from; import_type_tosource file]
  | SImportComplete(_,values,types,it,vn,tn) ->
    flow [str_import;flow_map commabreak name_tosource (values @ types);
          str_from; import_type_tosource it; str_as; name_tosource vn; name_tosource tn]

and provided_value_tosource pv = match pv with
  | PValue(_,v,ann) -> infix indent 1 str_coloncolon (name_tosource v) (ann_tosource ann)

and provided_alias_tosource pa = match pa with
  | PAlias(_,iname,oname,m) -> infix indent 1 str_as (name_tosource iname) (name_tosource oname)

and provided_datatype_tosource pd = match pd with
  | PData(_,d,_) -> name_tosource d

and provide_tosource p = match p with
  | SProvide(_,b) -> soft_surround indent 1 str_provide (expr_tosource b) str_end
  | SProvideComplete(_,v,a,d) ->
    (str "provide-complete") +^ (parens (flow_map commabreak (fun x -> x) [
        infix indent 1 str_colon (str "Values")
          (brackets (flow_map commabreak provided_value_tosource v));
        infix indent 1 str_colon (str "Aliases")
          (brackets (flow_map commabreak provided_alias_tosource a));
        infix indent 1 str_colon (str "Data")
          (brackets (flow_map commabreak provided_datatype_tosource d))
      ]))
  | SProvideAll(_) -> str_provide_star
  | SProvideNone(_) -> mt_doc

and provide_types_tosource pt : pprintdoc = match pt with
  | SProvideTypes(_,ann) ->
    surround_separate indent 1 (str_provide_types +^ break_one +^ lbrack +^ rbrace)
      (str_provide_types +^ break_one +^ lbrace) commabreak rbrace
      (List.map afield_tosource ann)
  | SProvideTypesAll(_) -> str_provide_types_star
  | SProvideTypesNone(_) -> mt_doc

and import_type_tosource it = match it with
  | SFileImport(_,file) -> str ("\"" ^ file ^ "\"")
  | SConstImport(_,m) -> str m
  | SSpecialImport(_,k,args) ->
    group ((str k) +^ (parens (nest indent (separate commabreak (List.map str args)))))

and hint_tosource h = match h with
  | HUseLoc(_) -> str_use_loc +^ (parens (str "<loc>"))

and letbind_tosource lb = match lb with
  | SLetBind(_,b,v) ->
    group (nest indent ((bind_tosource b) +^ str_spaceequal +^ break_one +^ (expr_tosource v)))
  | SVarBind(_,b,v) ->
    group (nest indent ((str "var ") +^ (bind_tosource b) +^ str_spaceequal +^ break_one +^ (expr_tosource v)))

and letrecbind_tosource lb = match lb with
  | SLetrecBind(_,b,v) ->
    group (nest indent ((bind_tosource b) +^ str_spaceequal +^ break_one +^ (expr_tosource v)))

and typeletbind_tosource tlb = match tlb with
  | STypeBind(_,n,a) ->
    group (nest indent ((name_tosource n) +^ str_spaceequal +^ break_one +^ (ann_tosource a)))
  | SNewtypeBind(_,n,nt) ->
    group (nest indent (str_newtype +^ (name_tosource n) +^ break_one +^ str_as +^ break_one +^ (name_tosource nt)))

and definedvalue_tosource dv = match dv with
  | SDefinedValue(n,v) -> infix indent 1 str_colon (str n) (expr_tosource v)

and definedtype_tosource dt = match dt with
  | SDefinedType(n,t) -> infix indent 1 str_coloncolon (str n) (ann_tosource t)

and expr_tosource e = match e with
  | SModule(_,a,dv,dt,pv,pt,c) ->
    (str "Module") +^ (parens (flow_map commabreak (fun x -> x) [
        infix indent 1 str_colon (str "Answer") (expr_tosource a);
        infix indent 1 str_colon (str "DefinedValues")
          (brackets (flow_map commabreak definedvalue_tosource dv));
        infix indent 1 str_colon (str "DefinedTypes")
          (brackets (flow_map commabreak definedtype_tosource dt));
        infix indent 1 str_colon (str "Provides") (expr_tosource pv);
        infix indent 1 str_colon (str "Types")
          (brackets (flow_map commabreak afield_tosource pt));
        infix indent 1 str_colon (str "checks") (expr_tosource c)
      ]))
  | STypeLetExpr(_,binds,body) ->
    let header = (surround_separate (2 * indent) 1 str_type_let
                    (str_type_let +^ (str " ")) commabreak mt_doc
                    (List.map typeletbind_tosource binds)) +^ str_colon in
    surround indent 1 header (expr_tosource body) str_end
  | SLetExpr(_,binds,body) ->
    let header = (surround_separate (2 * indent) 1 str_let
                    (str_let +^ (str " ")) commabreak mt_doc
                    (List.map letbind_tosource binds)) +^ str_colon in
    surround indent 1 header (expr_tosource body) str_end
  | SLetrec(_,binds,body) ->
    let header = (surround_separate (2 * indent) 1 str_letrec
                    (str_letrec +^ (str " ")) commabreak mt_doc
                    (List.map letrecbind_tosource binds)) +^ str_colon in
    surround indent 1 header (expr_tosource body) str_end
  | SHintExp(_,hints,exp) ->
    (flow_map hardline (fun h -> str_comment +^ (hint_tosource h)) hints) +^ hardline +^ (expr_tosource exp)
  | SInstantiate(_,e,p) ->
    group ((expr_tosource e) +^ (surround_separate indent 0 mt_doc langle commabreak rangle
                                   (List.map ann_tosource p)))
  | SBlock(_,s) ->
    flow_map hardline expr_tosource s
  | SUserBlock(_,b) -> surround indent 1 str_block (expr_tosource b) str_end
  | SFun(_,n,p,ar,an,d,b,_c) ->
    funlam_tosource str_fun (Some(n)) p ar an d b _c
  | SType(_,n,a) -> group (nest indent
                             (str_type +^ (name_tosource n) +^
                              str_spaceequal +^ break_one +^ (ann_tosource a)))
  | SNewtype(_,n,nt) ->
    group (nest indent (str_newtype +^ (name_tosource n)
                        +^ break_one +^ str_as +^ break_one +^ (name_tosource nt)))
  | SVar(_,n,v) -> str_var +^ (group (nest indent ((bind_tosource n) +^ str_spaceequal
                                                   +^ break_one +^ (expr_tosource v))))
  | SRec(_,n,v) -> str_rec +^ (group (nest indent ((bind_tosource n) +^ str_spaceequal
                                                   +^ break_one +^ (expr_tosource v))))
  | SLet(_,n,v,kv) ->
    group (nest indent ((if kv then str_val else mt_doc) +^ (bind_tosource n)
                        +^ str_spaceequal +^ break_one +^ (expr_tosource v)))
  | SRef(_,a) -> begin match a with
      | None -> str "bare-ref"
      | Some(ann) -> group((str "ref ") +^ (ann_tosource ann))
    end
  | SContract(_,n,a) -> infix indent 1 str_coloncolon (name_tosource n) (ann_tosource a)
  | SWhen(_,t,b) -> soft_surround indent 1 (str_when +^ (parens (expr_tosource t)) +^ str_colon)
                      (expr_tosource b) str_end
  | SAssign(_,i,v) ->
    group (nest indent ((name_tosource i) +^ str_spacecolonequal +^ break_one +^ (expr_tosource v)))
  | SIfPipe(_,b) ->
    surround_separate indent 1 (str_askcolon +^ str_space +^ str_end)
      (group str_askcolon) break_one str_end
      (List.map (fun x -> (group (ifpipebranch_tosource x))) b)
  | SIfPipeElse(_,b,e) ->
    let body = (separate break_one (List.map (fun x -> (group (ifpipebranch_tosource x))) b))
                                   +^ break_one +^ (group (str_pipespace +^ str_otherwisecolon +^
                                                           break_one +^ (expr_tosource e))) in
    surround indent 1 (group str_askcolon) body str_end
  | SIf(_,b) ->
    let branches = separate (break_one +^ str_elsespace) (List.map ifbranch_tosource b) in
    group (branches +^ break_one +^ str_end)
  | SIfElse(_,b,e) ->
    let branches = separate (break_one +^ str_elsespace) (List.map ifbranch_tosource b) in
    let els = str_elsecolon +^ (nest indent (break_one +^ (expr_tosource e))) in
    group (branches +^ break_one +^ els +^ break_one +^ str_end)
  | SCases(_,t,v,b) ->
    let header = str_cases +^ (parens (ann_tosource t)) +^ break_one
                 +^ (expr_tosource v) +^ str_colon in
    surround_separate indent 1 (header +^ str_space +^ str_end)
      (group header) break_one str_end
      (List.map (fun x -> group (casesbranch_tosource x)) b)
  | SCasesElse(_,t,v,b,e) ->
    let header = str_cases +^ (parens (ann_tosource t)) +^ break_one
                 +^ (expr_tosource v) +^ str_colon in
    let body = (separate break_one (List.map (fun x -> group (casesbranch_tosource x)) b)) +^
               break_one +^ (group (str_elsebranch +^ break_one +^ (expr_tosource e))) in
    surround indent 1 (group header) body str_end
  | SOp(_,op,l,r) ->
    let rec collect_same_operands exp = match exp with
      | SOp(_,op2,l2,r2) -> if op2 == op then (collect_same_operands l2) @ (collect_same_operands r2)
        else [exp]
      | _ -> [exp] in
    let operands = (collect_same_operands l) @ (collect_same_operands r) in 
    begin match operands with
      | [] -> mt_doc
      | fst::[] -> (expr_tosource fst)
      | fst::(snd::rest2) ->
        let op = break_one +^ (str (String.sub op 2 ((String.length op) - 2))) +^ break_one in
        let nested = List.fold_left (fun acc operand ->
            acc +^ (group (op +^ (expr_tosource operand)))) (expr_tosource snd) rest2 in
        group ((expr_tosource fst) +^ op +^ (nest indent nested))
    end
  | SCheckTest(_,op,r,l,right) ->
    let option_tosource opt = match opt with
      | None -> mt_doc
      | Some(ast) -> (expr_tosource ast) in
    begin match right with
      | None ->
        infix indent 1 (checkop_tosource op) (expr_tosource l) (option_tosource right)
      | Some(refinement) ->
        infix indent 1
          (infix indent 0 str_percent (checkop_tosource op) (parens (expr_tosource refinement)))
          (expr_tosource l) (option_tosource right)
    end
  | SCheckExpr(_,e,a) ->
    infix indent 1 str_coloncolon (expr_tosource e) (ann_tosource a)
  | SParen(_,e) -> parens (expr_tosource e)
  | SLam(_,p,ar,an,d,b,c) ->
    funlam_tosource str_lam None p ar an d b c
  | SMethod(_,p,ar,an,d,b,c) ->
    funlam_tosource str_method None p ar an d b c
  | SExtend(_,s,f) ->
    group ((expr_tosource s) +^ str_period +^
           (surround_separate indent 1 (lbrace +^ rbrace)
              lbrace commabreak rbrace (List.map member_tosource f)))
  | SUpdate(_,s,f) ->
    group ((expr_tosource s) +^ str_bang +^
           (surround_separate indent 1 (lbrace +^ rbrace)
              lbrace commabreak rbrace (List.map member_tosource f)))
  | SObj(_,f) ->
    surround_separate indent 1 (lbrace +^ rbrace) lbrace commabreak rbrace
      (List.map member_tosource f)
  | SArray(_,v) ->
    surround_separate indent 0 (str "[raw-array: ]") (str "[raw-array: ]") commabreak rbrack
      (List.map expr_tosource v)
  | SConstruct(_,m,c,v) ->
    let prefix = lbrack +^ (group (separate (sbreak 1) [(constructmodifier_tosource m); (expr_tosource c)])) +^ str_colonspace in
    begin match v with
      | [] -> prefix +^ rbrack
      | _ -> surround indent 0 prefix (separate commabreak (List.map expr_tosource v)) rbrack
    end
  | SApp(_,f,a) ->
    group ((expr_tosource f) +^ (parens (nest indent (separate commabreak (List.map expr_tosource a)))))
  | SPrimApp(_,f,a) ->
    group ((str f) +^ (parens (nest indent (separate commabreak (List.map expr_tosource a)))))
  | SPrimVal(_,n) -> str n
  | SId(_,n) -> name_tosource n
  | SIdVar(_,n) -> (str "!") +^ (name_tosource n)
  | SIdLetrec(_,n,_) -> (str "~") +^ (name_tosource n)
  | SUndefined(_) -> (str "undefined")
  | SSrcloc(_,_) -> (str "<srcloc>")
  | SNum(_,n) -> (str (BatNum.to_string n))
  | SFrac(_,n,d) -> (str (Printf.sprintf "%d/%d" n d))
  | SBool(_,b) -> str (if b then "true" else "false")
  | SStr(_,s) -> str (Printf.sprintf "\"%s\"" s)
  | SDot(_,o,f) -> infix_break indent 0 str_period (expr_tosource o) (str f)
  | SGetBang(_,o,f) -> infix_break indent 0 str_bang (expr_tosource o) (str f)
  | SBracket(_,o,f) -> infix_break indent 0 str_period (expr_tosource o)
                         (surround indent 0 lbrack (expr_tosource f) rbrack)
  | SData(_,_,_,_,_,_,_) -> (str "<data>") (* TODO *)
  | SDataExpr(_,_,_,_,_,_,_,_) -> (str "<data-expr>") (* TODO *)
  | SFor(_,_,_,_,_) -> (str "<for>") (* TODO *)
  | SCheck(_,_,_,_) -> (str "<check>") (* TODO *)

and constructmodifier_tosource c = match c with
  | SConstructNormal -> mt_doc
  | SConstructLazy -> str "lazy"

and bind_tosource b = match b with
  | SBind(_,s,i,a) -> begin match a with
      | ABlank -> if s then ((str "shadow ") +^ (name_tosource i))
        else (name_tosource i)
      | _ -> if s then
            (infix indent 1 str_coloncolon ((str "shadow ") +^ (name_tosource i)) (ann_tosource a))
          else
            (infix indent 1 str_coloncolon (name_tosource i) (ann_tosource a))
    end

and member_tosource m = match m with
  | SDataField(_,n,v) ->
    nest indent ((str n) +^ str_colonspace +^ (expr_tosource v))
  | SMutableField(_,n,a,v) ->
    nest indent (str_mutable +^ (str n) +^ str_coloncolon +^ (ann_tosource a)
                 +^ str_colonspace +^ (expr_tosource v))
  | SMethodField(_,n,p,ar,an,d,b,c) ->
    funlam_tosource (str n) None p ar an d b c

and forbind_tosource f = match f with
  | SForBind(_, b, v) ->
    group ((bind_tosource b) +^ break_one +^ str_from +^ break_one +^ (expr_tosource v))

and variantmembertype_tosource vmt = match vmt with
  | SNormal -> mt_doc
  | SMutable -> str "mutable "

and variantmember_tosource vm = match vm with
  | SVariantMember(_,mt,b) ->
    (variantmembertype_tosource mt) +^ (bind_tosource b)

and variant_tosource v = match v with
  | SVariant(_,_,_,_,_) -> str "<s-variant>" (* TODO *)
  | SSingletonVariant(_,_,_) -> str "<s-singleton-variant>" (* TODO *)

and ifbranch_tosource i = match i with
  | SIfBranch(_,t,b) ->
    str_if +^ (nest (2 * indent) (expr_tosource t) +^ str_colon)
    +^ (nest indent (break_one +^ (expr_tosource b)))

and ifpipebranch_tosource i = match i with
  | SIfPipeBranch(_,t,b) ->
    str_pipespace +^ (nest (2 * indent) ((expr_tosource t) +^ break_one +^ str_thencolon))
    +^ (nest indent (break_one +^ (expr_tosource b)))

and casesbindtype_tosource cbt = match cbt with
  | SCasesBindRef -> str "ref"
  | SCasesBindNormal -> str ""

and casesbind_tosource cb = match cb with
  | SCasesBind(_,ft,b) ->
    (casesbindtype_tosource ft) +^ (str " ") +^ (bind_tosource b)

and casesbranch_tosource cb = match cb with
  | SCasesBranch(_,_,n,a,b) ->
    nest indent
      ((group ((str ("| " ^ n)) +^ (surround_separate indent 0 (str "()") lparen commabreak rparen
                                    (List.map casesbind_tosource a)) +^ break_one +^ str_thickarrow))
       +^ break_one +^ (expr_tosource b))
  | SSingletonCasesBranch(_,_,n,b) ->
    nest indent
      (group ((str ("| " ^ n)) +^ break_one +^ str_thickarrow) +^ break_one +^ (expr_tosource b))

and checkop_tosource c = match c with
  | SOpIs -> str_is
  | SOpIsOp(o) -> str_is +^ (str (String.sub o 2 ((String.length o) - 2)))
  | SOpIsNot -> str_is_not
  | SOpIsNotOp(o) -> str_is_not +^ (str (String.sub o 2 ((String.length o) - 2)))
  | SOpSatisfies -> str_satisfies
  | SOpSatisfiesNot -> str_satisfies_not
  | SOpRaises -> str_raises
  | SOpRaisesOther -> str_raises_other
  | SOpRaisesNot -> str_raises_not
  | SOpRaisesSatisfies -> str_raises_satisfies
  | SOpRaisesViolates -> str_raises_violates

and ann_tosource a = match a with
  | ABlank -> str_any
  | AAny -> str_any
  | AName(_,n) -> name_tosource n
  | ATypeVar(_,i) -> name_tosource i
  | AArrow(_,a,r,up) ->
    let ann = separate str_space
        ([separate commabreak (List.map ann_tosource a)]
         @ [str_arrow;(ann_tosource r)]) in
    if up then (surround indent 0 lparen ann rparen)
    else ann
  | AMethod(_,a,r) -> str "NYI: A-method"
  | ARecord(_,f) ->
    surround_separate indent 1 (lbrace +^ rbrace) lbrace commabreak rbrace
      (List.map afield_tosource f)
  | AApp(_,a,ar) ->
    group ((ann_tosource a) +^ (group (langle +^ (nest indent
                                                    ((separate commabreak
                                                        (List.map ann_tosource ar)) +^ rangle)))))
  | APred(_,a,e) ->
    (ann_tosource a) +^ (parens (expr_tosource e))
  | ADot(_,o,f) ->
    (name_tosource o) +^ (str ("." ^ f))
  | AChecked(c,r) -> (ann_tosource r)

and afield_tosource a = match a with
  | AField(_,n,ABlank) -> str n
  | AField(_,n,ann) -> infix indent 1 str_coloncolon (str n) (ann_tosource ann)

(* S-Expression Conversion Functions (Used for Debug Output) *)
let str_of_loc {source=src;start_line=sl;start_col=sc;start_char=sch;
                   end_line=el;end_col=ec;end_char=ech} =
  sprintf "[LOC<%s>: (%d:%d:%d)-(%d:%d:%d)]"
    src sl sc sch el ec ech

let sexp_of_loc l = Sexp.Atom (str_of_loc l)

let sexp_of_bool b = Sexp.Atom (if b then "true" else "false")

let sexp_of_int i = Sexp.Atom (sprintf "%d" i)

let sexp_of_num n = Sexp.Atom (BatNum.to_string n)

let sexp_of_str s = Sexp.Atom ("\""^s^"\"")

let sexp_with_loc node_name loc =
  Sexp.List [Sexp.Atom node_name; sexp_of_loc loc]

let sexp_node node_name =
  Sexp.List [Sexp.Atom node_name]

let sexp_of_name n =
  let a x = Sexp.Atom x
  and lst x = Sexp.List x in
  match n with
  | SUnderscore(l) -> sexp_with_loc "SUnderscore" l
  | SName(l,n) -> lst [a "SName"; sexp_of_loc l; sexp_of_str n]
  | SGlobal(n) -> lst [a "SGlobal"; sexp_of_str n]
  | STypeGlobal(n) -> lst [a "STypeGlobal"; sexp_of_str n]
  | SAtom(n,i) -> lst [a "SAtom"; sexp_of_str n; sexp_of_int i]

let sexp_of_list f l = Sexp.List ((Sexp.Atom "list")::(List.map f l))

let sexp_of_option f o = match o with
  | Some(x) -> f x
  | None -> sexp_node "None"

let rec sexp_of_program p =
  match p with
  | SProgram(l,p,pt,il,e) ->
    Sexp.List [Sexp.Atom "SProgram"; sexp_of_loc l;
               sexp_of_provide p; sexp_of_provide_types pt;
               sexp_of_list sexp_of_import il; sexp_of_expr e]

and sexp_of_ann a =
  let at x = Sexp.Atom x
  and lst x = Sexp.List x in
  let sexp_of_ann_list = sexp_of_list sexp_of_ann in
  match a with
  | ABlank -> sexp_node "ABlank"
  | AAny -> sexp_node "AAny"
  | AName(l,n) -> lst [at "AName"; sexp_of_loc l; sexp_of_name n]
  | ATypeVar(l,n) -> lst [at "ATypeVar"; sexp_of_loc l; sexp_of_name n]
  | AArrow(l,al,a,b) ->
    lst [at "AArrow"; sexp_of_loc l; sexp_of_ann_list al; sexp_of_ann a; sexp_of_bool b]
  | AMethod(l,al,a) -> lst [at "AMethod"; sexp_of_loc l; sexp_of_ann_list al; sexp_of_ann a]
  | ARecord(l,afl) -> lst [at "ARecord"; sexp_of_loc l; lst (List.map sexp_of_a_field afl)]
  | AApp(l,a,al) -> lst [at "AApp"; sexp_of_loc l; sexp_of_ann a; sexp_of_ann_list al]
  | APred(l,a,e) -> lst [at "APred"; sexp_of_loc l; sexp_of_ann a; sexp_of_str "<<TODO>>"]
  | ADot(l,n,s) -> lst [at "ADot"; sexp_of_loc l; sexp_of_name n; sexp_of_str s]
  | AChecked(a1,a2) -> lst [at "AChecked"; sexp_of_ann a1; sexp_of_ann a2]

and sexp_of_a_field af =
  let at x = Sexp.Atom x
  and lst x = Sexp.List x in
  match af with
  | AField(l,s,a) -> lst [at "AField"; sexp_of_loc l; sexp_of_str s; sexp_of_ann a]

and sexp_of_provided_value pv =
  match pv with
  | PValue(l,n,a) -> Sexp.List [Sexp.Atom "PValue"; sexp_of_loc l; sexp_of_name n; sexp_of_ann a]

and sexp_of_provided_alias pa =
  match pa with
  | PAlias(l,n1,n2,it) ->
    Sexp.List [Sexp.Atom "PAlias"; sexp_of_loc l; sexp_of_name n1;
               sexp_of_name n2; sexp_of_option sexp_of_import_type it]

and sexp_of_provided_datatype pd =
  match pd with
  | PData(l,n,it) -> Sexp.List [Sexp.Atom "PData"; sexp_of_loc l; sexp_of_name n;
                                sexp_of_option sexp_of_import_type it]

and sexp_of_import_type it =
  let at x = Sexp.Atom x
  and lst x = Sexp.List x in
  match it with
  | SFileImport(l,s) -> lst [at "SFileImport"; sexp_of_loc l; sexp_of_str s]
  | SConstImport(l,s) -> lst [at "SConstImport"; sexp_of_loc l; sexp_of_str s]
  | SSpecialImport(l,s,strs) -> lst [at "SSpecialImport"; sexp_of_loc l; sexp_of_str s;
                                     sexp_of_list sexp_of_str strs]

and sexp_of_import i =
  let at x = Sexp.Atom x
  and lst x = Sexp.List x
  and sexp_of_name_list = sexp_of_list sexp_of_name in
  match i with
  | SInclude(l,it) -> lst [at "SInclude"; sexp_of_loc l; sexp_of_import_type it]
  | SImport(l,it,n) -> lst [at "SImport"; sexp_of_loc l; sexp_of_import_type it; sexp_of_name n]
  | SImportTypes(l,it,n1,n2) ->
    lst [at "SImportTypes"; sexp_of_loc l; sexp_of_import_type it; sexp_of_name n1; sexp_of_name n2]
  | SImportFields(l,nl,it) ->
    lst [at "SImportFields"; sexp_of_loc l; sexp_of_name_list nl; sexp_of_import_type it]
  | SImportComplete(l,nl1,nl2,it,n1,n2) ->
    lst [at "SImportComplete"; sexp_of_loc l;
         sexp_of_name_list nl1; sexp_of_name_list nl2;
         sexp_of_import_type it; sexp_of_name n1; sexp_of_name n2]

and sexp_of_provide p =
  let at x = Sexp.Atom x
  and lst x = Sexp.List x in
  match p with
  | SProvide(l,e) -> lst [at "SProvide"; sexp_of_loc l; sexp_of_expr e]
  | SProvideComplete(l,pvl,pal,pdl) ->
    lst [at "SProvideComplete"; sexp_of_loc l;
         sexp_of_list sexp_of_provided_value pvl;
         sexp_of_list sexp_of_provided_alias pal;
         sexp_of_list sexp_of_provided_datatype pdl]
  | SProvideAll(l) -> sexp_with_loc "SProvideAll" l
  | SProvideNone(l) -> sexp_with_loc "SProvideNone" l

and sexp_of_provide_types pt =
  let at x = Sexp.Atom x
  and lst x = Sexp.List x in
  match pt with
  | SProvideTypes(l,afl) ->
    lst [at "SProvideTypes"; sexp_of_loc l; sexp_of_list sexp_of_a_field afl]
  | SProvideTypesAll(l) -> sexp_with_loc "SProvideTypesAll" l
  | SProvideTypesNone(l) -> sexp_with_loc "SProvideTypesNone" l

and sexp_of_let_bind lb =
  let at x = Sexp.Atom x
  and lst x = Sexp.List x in
  match lb with
  | SLetBind(l,b,e) ->
    lst [at "SLetBind"; sexp_of_loc l; sexp_of_bind b; sexp_of_expr e]
  | SVarBind(l,b,e) ->
    lst [at "SVarBind"; sexp_of_loc l; sexp_of_bind b; sexp_of_expr e]

and sexp_of_letrec_bind lrb =
  let at x = Sexp.Atom x
  and lst x = Sexp.List x in
  match lrb with
  | SLetrecBind(l,b,e) -> lst [at "SLetrecBind"; sexp_of_loc l; sexp_of_bind b; sexp_of_expr e]

and sexp_of_type_let_bind tlb =
  let at x = Sexp.Atom x
  and lst x = Sexp.List x in
  match tlb with
  | STypeBind(l,n,a) ->
    lst [at "STypeBind"; sexp_of_loc l; sexp_of_name n; sexp_of_ann a]
  | SNewtypeBind(l,n1,n2) ->
    lst [at "SNewtypeBind"; sexp_of_loc l; sexp_of_name n1; sexp_of_name n2]

and sexp_of_defined_value dv =
  match dv with
  | SDefinedValue(s,e) -> Sexp.List [Sexp.Atom "SDefinedValue"; sexp_of_str s; sexp_of_expr e]

and sexp_of_defined_type dt =
  match dt with
  | SDefinedType(s,a) -> Sexp.List [Sexp.Atom "SDefinedType"; sexp_of_str s; sexp_of_ann a]

and sexp_of_bind b =
  match b with
  | SBind(l,b,n,a) -> Sexp.List [Sexp.Atom "SBind"; sexp_of_loc l;
                                 sexp_of_bool b; sexp_of_name n; sexp_of_ann a]

and sexp_of_member m =
  let at x = Sexp.Atom x
  and lst x = Sexp.List x in
  let sexp_of_name_list = sexp_of_list sexp_of_name
  and sexp_of_bind_list = sexp_of_list sexp_of_bind
  and sexp_of_expr_option = sexp_of_option sexp_of_expr in
  match m with
  | SDataField(l,s,e) -> lst [at "SDataField"; sexp_of_loc l; sexp_of_str s; sexp_of_expr e]
  | SMutableField(l,s,a,e) ->
    lst [at "SMutableField"; sexp_of_loc l; sexp_of_str s; sexp_of_ann a; sexp_of_expr e]
  | SMethodField(l,s1,nl,bl,a,s2,e,eo) ->
    lst [at "SMethodField"; sexp_of_loc l; sexp_of_str s1; sexp_of_name_list nl;
         sexp_of_bind_list bl; sexp_of_ann a; sexp_of_str s2; sexp_of_expr e;
         sexp_of_expr_option eo]

and sexp_of_for_bind fb =
  match fb with
  | SForBind(l,b,e) ->
    Sexp.List [Sexp.Atom "SForBind"; sexp_of_loc l; sexp_of_bind b; sexp_of_expr e]

and sexp_of_variant_member vm =
  match vm with
  | SVariantMember(l,vmt,b) ->
    Sexp.List [Sexp.Atom "SVariantMember"; sexp_of_loc l;
               sexp_of_variant_member_type vmt; sexp_of_bind b]

and sexp_of_variant v =
  let at x = Sexp.Atom x
  and lst x = Sexp.List x in
  let sexp_of_vm_list = sexp_of_list sexp_of_variant_member
  and sexp_of_member_list = sexp_of_list sexp_of_member in
  match v with
  | SVariant(l1,l2,s,vml,ml) ->
    lst [at "SVariant"; sexp_of_loc l1; sexp_of_loc l2; sexp_of_str s;
         sexp_of_vm_list vml; sexp_of_member_list ml]
  | SSingletonVariant(l,s,ml) ->
    lst [at "SSingletonVariant"; sexp_of_loc l; sexp_of_str s;
         sexp_of_member_list ml]

and sexp_of_if_branch ib =
  match ib with
  | SIfBranch(l,e1,e2) ->
    Sexp.List [Sexp.Atom "SIfBranch"; sexp_of_loc l; sexp_of_expr e1; sexp_of_expr e2]

and sexp_of_if_pipe_branch ipb =
  match ipb with
  | SIfPipeBranch(l,e1,e2) ->
    Sexp.List [Sexp.Atom "SIfPipeBranch"; sexp_of_loc l; sexp_of_expr e1; sexp_of_expr e2]

and sexp_of_cases_bind cb =
  match cb with
  | SCasesBind(l,cbt,b) ->
    Sexp.List [Sexp.Atom "SCasesBind"; sexp_of_loc l;
               sexp_of_cases_bind_type cbt; sexp_of_bind b]

and sexp_of_cases_branch cb =
  match cb with
  | SCasesBranch(l1,l2,s,cbl,e) ->
    Sexp.List [Sexp.Atom "SCasesBranch"; sexp_of_loc l1;
               sexp_of_loc l2; sexp_of_str s;
               sexp_of_list sexp_of_cases_bind cbl;
               sexp_of_expr e]
  | SSingletonCasesBranch(l1,l2,s,e) ->
    Sexp.List [Sexp.Atom "SSingletonCasesBranch"; sexp_of_loc l1;
               sexp_of_loc l2; sexp_of_str s;
               sexp_of_expr e]

and sexp_of_hint h =
  match h with
  | HUseLoc(l) -> sexp_with_loc "HUseLoc" l

and sexp_of_construct_modifier cm =
  sexp_node (match cm with
      | SConstructNormal -> "SConstructNormal"
      | SConstructLazy -> "SConstructLazy")

and sexp_of_variant_member_type vmt =
  sexp_node (match vmt with
      | SNormal -> "SNormal"
      | SMutable -> "SMutable")

and sexp_of_cases_bind_type cbt =
  sexp_node (match cbt with
      | SCasesBindRef -> "SCasesBindRef"
      | SCasesBindNormal -> "SCasesBindNormal")

and sexp_of_check_op co =
  match co with
  | SOpIsOp(s) -> Sexp.List [Sexp.Atom "SOpIsOp"; sexp_of_str s]
  | SOpIsNotOp(s) -> Sexp.List [Sexp.Atom "SOpIsNotOp"; sexp_of_str s]
  | SOpIs -> sexp_node "SOpIs"
  | SOpIsNot -> sexp_node "SOpIsNot"
  | SOpSatisfies -> sexp_node "SOpSatisfies"
  | SOpSatisfiesNot -> sexp_node "SOpSatisfiesNot"
  | SOpRaises -> sexp_node "SOpRaises"
  | SOpRaisesOther -> sexp_node "SOpRaisesOther"
  | SOpRaisesNot -> sexp_node "SOpRaisesNot"
  | SOpRaisesSatisfies -> sexp_node "SOpRaisesSatisfies"
  | SOpRaisesViolates -> sexp_node "SOpRaisesViolates"

and sexp_of_expr e =
  let at x = Sexp.Atom x
  and lst x = Sexp.List x in
  let sexp_of_af_list = sexp_of_list sexp_of_a_field
  and sexp_of_bind_list = sexp_of_list sexp_of_bind
  and sexp_of_name_list = sexp_of_list sexp_of_name
  and sexp_of_expr_list = sexp_of_list sexp_of_expr
  and sexp_of_expr_option = sexp_of_option sexp_of_expr in
  match e with
  | SModule(l,e1,dvl,dtl,e2,afl,e3) ->
    lst [at "SModule"; sexp_of_loc l; sexp_of_expr e1;
         sexp_of_list sexp_of_defined_value dvl;
         sexp_of_list sexp_of_defined_type dtl;
         sexp_of_expr e2; sexp_of_af_list afl; sexp_of_expr e3]
  | STypeLetExpr(l,tlbl,e) ->
    lst [at "STypeLetExpr"; sexp_of_loc l;
         sexp_of_list sexp_of_type_let_bind tlbl; sexp_of_expr e]
  | SLetExpr(l,lbl,e) ->
    lst [at "SLetExpr"; sexp_of_loc l;
         sexp_of_list sexp_of_let_bind lbl; sexp_of_expr e]
  | SLetrec(l,lrbl,e) ->
    lst [at "SLetrecExpr"; sexp_of_loc l;
         sexp_of_list sexp_of_letrec_bind lrbl; sexp_of_expr e]
  | SHintExp(l,hl,e) ->
    lst [at "SHintExp"; sexp_of_loc l;
         sexp_of_list sexp_of_hint hl; sexp_of_expr e]
  | SInstantiate(l,e,al) ->
    lst [at "SInstantiate"; sexp_of_loc l; sexp_of_expr e;
         sexp_of_list sexp_of_ann al]
  | SBlock(l,el) ->
    lst [at "SBlock"; sexp_of_loc l; sexp_of_expr_list el]
  | SUserBlock(l,e) ->
    lst [at "SUserBlock"; sexp_of_loc l; sexp_of_expr e]
  | SFun(l,s1,nl,bl,a,s2,e,eo) ->
    lst [at "SFun"; sexp_of_str s1; sexp_of_name_list nl;
         sexp_of_bind_list bl; sexp_of_ann a; sexp_of_str s2;
         sexp_of_expr e; sexp_of_expr_option eo]
  | SType(l,n,a) ->
    lst [at "SType"; sexp_of_loc l; sexp_of_name n; sexp_of_ann a]
  | SNewtype(l,n1,n2) ->
    lst [at "SNewtype"; sexp_of_loc l; sexp_of_name n1; sexp_of_name n2]
  | SVar(l,b,e) ->
    lst [at "SVar"; sexp_of_loc l; sexp_of_bind b; sexp_of_expr e]
  | SRec(l,b,e) ->
    lst [at "SRec"; sexp_of_loc l; sexp_of_bind b; sexp_of_expr e]
  | SLet(l,b,e,bo) ->
    lst [at "SLet"; sexp_of_loc l; sexp_of_bind b; sexp_of_expr e; sexp_of_bool bo]
  | SRef(l,ao) ->
    lst [at "SRef"; sexp_of_loc l; sexp_of_option sexp_of_ann ao]
  | SContract(l,n,a) ->
    lst [at "SContract"; sexp_of_loc l; sexp_of_name n; sexp_of_ann a]
  | SWhen(l,e1,e2) ->
    lst [at "SWhen"; sexp_of_loc l; sexp_of_expr e1; sexp_of_expr e2]
  | SAssign(l,n,e) ->
    lst [at "SAssign"; sexp_of_loc l; sexp_of_name n; sexp_of_expr e]
  | SIfPipe(l,ipbl) ->
    lst [at "SIfPipe"; sexp_of_loc l; sexp_of_list sexp_of_if_pipe_branch ipbl]
  | SIfPipeElse(l,ipbl,e) ->
    lst [at "SIfPipeElse"; sexp_of_loc l;
         sexp_of_list sexp_of_if_pipe_branch ipbl; sexp_of_expr e]
  | SIf(l,ibl) ->
    lst [at "SIf"; sexp_of_loc l; sexp_of_list sexp_of_if_branch ibl]
  | SIfElse(l,ibl,e) ->
    lst [at "SIfElse"; sexp_of_loc l;
         sexp_of_list sexp_of_if_branch ibl; sexp_of_expr e]
  | SCases(l,a,e,cbl) ->
    lst [at "SCases"; sexp_of_loc l; sexp_of_ann a; sexp_of_expr e;
         sexp_of_list sexp_of_cases_branch cbl]
  | SCasesElse(l,a,e1,cbl,e2) ->
    lst [at "SCasesElse"; sexp_of_loc l; sexp_of_ann a; sexp_of_expr e1;
           sexp_of_list sexp_of_cases_branch cbl; sexp_of_expr e2]
  | SOp(l,s,e1,e2) ->
    lst [at "SOp"; sexp_of_loc l; sexp_of_str s;
         sexp_of_expr e1; sexp_of_expr e2]
  | SCheckTest(l,co,eo1,e,eo2) ->
    lst [at "SCheckTest"; sexp_of_loc l; sexp_of_check_op co;
         sexp_of_expr_option eo1; sexp_of_expr e;
         sexp_of_expr_option eo2]
  | SCheckExpr(l,e,a) ->
    lst [at "SCheckExpr"; sexp_of_loc l; sexp_of_expr e; sexp_of_ann a]
  | SParen(l,e) ->
    lst [at "SParen"; sexp_of_loc l; sexp_of_expr e]
  | SLam(l,nl,bl,a,s,e,eo) ->
    lst [at "SLam"; sexp_of_name_list nl;
         sexp_of_bind_list bl; sexp_of_ann a; sexp_of_str s;
         sexp_of_expr e; sexp_of_expr_option eo]
  | SMethod(l,nl,bl,a,s,e,eo) ->
    lst [at "SMethod"; sexp_of_name_list nl;
         sexp_of_bind_list bl; sexp_of_ann a; sexp_of_str s;
         sexp_of_expr e; sexp_of_expr_option eo]
  | SExtend(l,e,ml) ->
    lst [at "SExtend"; sexp_of_loc l; sexp_of_expr e;
         sexp_of_list sexp_of_member ml]
  | SUpdate(l,e,ml) ->
    lst [at "SUpdate"; sexp_of_loc l; sexp_of_expr e;
         sexp_of_list sexp_of_member ml]
  | SObj(l,ml) ->
    lst [at "SObj"; sexp_of_loc l;
         sexp_of_list sexp_of_member ml]
  | SArray(l,el) ->
    lst [at "SArray"; sexp_of_loc l; sexp_of_expr_list el]
  | SConstruct(l,cm,e,el) ->
    lst [at "SConstruct"; sexp_of_loc l; sexp_of_construct_modifier cm;
         sexp_of_expr e; sexp_of_expr_list el]
  | SApp(l,e,el) ->
    lst [at "SApp"; sexp_of_loc l; sexp_of_expr e; sexp_of_expr_list el]
  | SPrimApp(l,s,el) ->
    lst [at "SPrimApp"; sexp_of_loc l; sexp_of_str s; sexp_of_expr_list el]
  | SPrimVal(l,s) ->
    lst [at "SPrimVal"; sexp_of_loc l; sexp_of_str s]
  | SId(l,n) ->
    lst [at "SId"; sexp_of_loc l; sexp_of_name n]
  | SIdVar(l,n) ->
    lst [at "SIdVar"; sexp_of_loc l; sexp_of_name n]
  | SIdLetrec(l,n,b) ->
    lst [at "SIdLetrec"; sexp_of_loc l; sexp_of_name n; sexp_of_bool b]
  | SUndefined(l) -> sexp_with_loc "SUndefined" l
  | SSrcloc(l1,l2) ->
    lst [at "Srcloc"; sexp_of_loc l1; sexp_of_loc l2]
  | SNum(l,n) ->
    lst [at "SNum"; sexp_of_loc l; sexp_of_num n]
  | SFrac(l,i1,i2) ->
    lst [at "SFrac"; sexp_of_loc l; sexp_of_int i1; sexp_of_int i2]
  | SBool(l,b) -> lst [at "SBool"; sexp_of_loc l; sexp_of_bool b]
  | SStr(l,s) -> lst [at "SStr"; sexp_of_loc l; sexp_of_str s]
  | SDot(l,e,s) -> lst [at "SDot"; sexp_of_loc l; sexp_of_expr e; sexp_of_str s]
  | SGetBang(l,e,s) ->
    lst [at "SGetBang"; sexp_of_loc l; sexp_of_expr e; sexp_of_str s]
  | SBracket(l,e1,e2) ->
    lst [at "SBracket"; sexp_of_loc l; sexp_of_expr e1; sexp_of_expr e1]
  | SData(l,s,nl,el,vl,ml,eo) ->
    lst [at "SData"; sexp_of_loc l; sexp_of_str s;
         sexp_of_name_list nl; sexp_of_expr_list el;
         sexp_of_list sexp_of_variant vl;
         sexp_of_list sexp_of_member ml;
         sexp_of_expr_option eo]
  | SDataExpr(l,s,n,nl,el,vl,ml,eo) ->
    lst [at "SDataExpr"; sexp_of_loc l; sexp_of_str s; sexp_of_name n;
         sexp_of_name_list nl; sexp_of_expr_list el;
         sexp_of_list sexp_of_variant vl;
         sexp_of_list sexp_of_member ml;
         sexp_of_expr_option eo]
  | SFor(l,e1,fbl,a,e2) ->
    lst [at "SFor"; sexp_of_loc l; sexp_of_expr e1;
         sexp_of_list sexp_of_for_bind fbl; sexp_of_ann a;
         sexp_of_expr e2]
  | SCheck(l,so,e,b) ->
    lst [at "SCheck"; sexp_of_loc l;
         sexp_of_option sexp_of_str so;
         sexp_of_expr e; sexp_of_bool b]


let prog_to_string p = Sexp.to_string_hum (sexp_of_program p)
