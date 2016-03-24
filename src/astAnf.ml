(** A-Normal Form Type Definitions *)
module SM = Map.Make(String)

(** Source Locations *)
type loc = Ast.loc

type program =
    AProgram of loc * Ast.provide * import list * expr

and import_type =
    AImportBuiltin of loc * string (** Location, Library *)
  | AImportFile of loc * string (** Location, File *)
  | AImportSpecial of loc * string * string list (** Location, kind, args *)

and import =
  | AImportComplete of loc * Ast.name list * Ast.name list * import_type *
                       Ast.name * Ast.name (** Location, Values, Types, Import Type,
                                                     Vals-name, Types-name *)
and type_bind =
    ATypeBind of loc * Ast.name * Ast.ann (** Location, Name, Ann *)
  | ANewtypeBind of loc * Ast.name * Ast.name (** Location, Name, NameT *)

and expr =
    ATypeLet of loc * type_bind * expr (** Location, Bind, Body *)
  | ALet of loc * bind * lettable * expr (** Location, Bind, Value, Body *)
  | AVar of loc * bind * lettable * expr (** Location, Bind, Value, Body *)
  | ASeq of loc * lettable * expr (** Location, Value, Value *)
  | ALettable of loc * lettable (** Location, Value *)

and bind =
    ABind of loc * Ast.name * Ast.ann (** Location, Identifier, Ann *)

and variant =
    AVariant of loc * loc * string * variant_member list * field list (** Location,
                                                                          Constructor Location,
                                                                          Name, Members, With-Members*)
  | ASingletonVariant of loc * string * field list (** Location, Name, With-Members*)

and member_type = ANormal | AMutable

and variant_member =
    AVariantMember of loc * member_type * bind (** Location, Member-Type, Binding *)

and cases_bind =
    ACasesBind of loc * Ast.cases_bind_type * bind

and cases_branch =
    ACasesBranch of loc * loc * string * cases_bind list * expr (** Location, Pattern Location,
                                                                    Name, Args, Body*)
  | ASingletonCasesBranch of loc * loc * string * expr (** Location, Pattern Location, Name, Body *)

and defined_value =
    ADefinedValue of string * value (** Name, Value *)

and defined_type =
    ADefinedType of string * Ast.ann (** Name, Type *)

and lettable =
    AModule of loc * value * defined_value list * defined_type list * value * Ast.a_field list * value (** Location, Answer,
                                                                                                Defined-Values, Defined-Types,
                                                                                                Provided-Values, Provided-Types*)
  | ACases of loc * Ast.ann * value * cases_branch list * expr (** Location, Type, Value, Branches, Else*)
  | AIf of loc * value * expr * expr (** Location, Cond, True, False *)
  | ADataExpr of loc * string * Ast.name * variant list * field list (** Location, Name, NameT, Variants, Shared *)
  | AAssign of loc * Ast.name * value (** Location, Name, Value *)
  | AApp of loc * value * value list (** Location, Function, Args *)
  | AMethodApp of loc * value * string * value list (** Location, Object, Method, Args *)
  | APrimApp of loc * string * value list (** Location, Function, Args *)
  | AArray of loc * value list (** Location, Values *)
  | ARef of loc * Ast.ann option (** Location, Ann *)
  | AObj of loc * field list (** Location, Fields *)
  | AUpdate of loc * value * field list (** Location, Object, Fields *)
  | AExtend of loc * value * field list (** Location, Object, Fields *)
  | ADot of loc * value * string (** Location, Object, Field *)
  | AColon of loc * value * string (** Location, Object, Field *)
  | AGetBang of loc * value * string (** Location, Object, Field *)
  | ALam of loc * bind list * Ast.ann * expr (** Location, Args, Return Ann, Body *)
  | AMethod of loc * bind list * Ast.ann * expr (** Location, Args, Return Ann, Body *)
  | AVal of loc * value (** Location, Value *)

and field =
    AField of loc * string * value (** Location, Name, Value *)

and value =
    ASrcloc of loc * loc (** Location, Value *)
  | ANum of loc * BatNum.num (** Location, Value *)
  | AStr of loc * string (** Location, Value *)
  | ABool of loc * bool (** Location, Value *)
  (* Used for Letrec *)
  | AUndefined of loc (** Location *)
  | AId of loc * Ast.name (** Location, Value *)
  | AIdVar of loc * Ast.name (** Location, Value *)
  | AIdLetrec of loc * Ast.name * bool (** Location, Value, Safe *)

let lettable_loc lbl = match lbl with
  | AModule(l,_,_,_,_,_,_) -> l
  | ACases(l,_,_,_,_) -> l
  | AIf(l,_,_,_) -> l
  | ADataExpr(l,_,_,_,_) -> l
  | AAssign(l,_,_) -> l
  | AApp(l,_,_) -> l
  | AMethodApp(l,_,_,_) -> l
  | APrimApp(l,_,_) -> l
  | AArray(l,_) -> l
  | ARef(l,_) -> l
  | AObj(l,_) -> l
  | AUpdate(l,_,_) -> l
  | AExtend(l,_,_) -> l
  | ADot(l,_,_) -> l
  | AColon(l,_,_) -> l
  | AGetBang(l,_,_) -> l
  | ALam(l,_,_,_) -> l
  | AMethod(l,_,_,_) -> l
  | AVal(l,_) -> l

let expr_loc e =
  match e with
  | ATypeLet(l,_,_) -> l
  | ALet(l,_,_,_) -> l
  | AVar(l,_,_,_) -> l
  | ASeq(l,_,_) -> l
  | ALettable(l,_) -> l

let value_loc (v : value) =
  match v with
  | ASrcloc (l,_) -> l
  | ANum (l,_) -> l
  | AStr (l,_) -> l
  | ABool (l,_) -> l
  | AUndefined l -> l
  | AId (l,_) -> l
  | AIdVar (l,_) -> l
  | AIdLetrec (l,_,_) -> l

class virtual ['prog_ret,
               'import_ret,
               'type_bind_ret,
               'expr_ret,
               'bind_ret,
               'variant_ret,
               'variant_member_ret,
               'cases_bind_ret,
               'cases_branch_ret,
               'defined_value_ret,
               'defined_type_ret,
               'lettable_ret,
               'field_ret,
               'value_ret] visitor = object(self)
  method visit_program = function
    | AProgram(loc, prov, il, body) -> self#a_program(loc, prov, il, body)

  method visit_import = function
    | AImportComplete(loc, dv, dt, it, vn, tn) -> self#a_import_complete(loc, dv, dt, it, vn, tn)

  method visit_type_bind = function
    | ATypeBind(loc, name, ann) -> self#a_type_bind(loc, name, ann)
    | ANewtypeBind(loc, name, namet) -> self#a_newtype_bind(loc, name, namet)

  method visit_expr = function
    | ATypeLet(loc, bind, body) -> self#a_type_let(loc, bind, body)
    | ALet(loc, bind, value, body) -> self#a_let(loc, bind, value, body)
    | AVar(loc, bind, value, body) -> self#a_var(loc, bind, value, body)
    | ASeq(loc, v1, v2) -> self#a_seq(loc, v1, v2)
    | ALettable(loc, value) -> self#a_lettable(loc, value)

  method visit_bind = function
    | ABind(loc, id, ann) -> self#a_bind(loc, id, ann)

  method visit_variant = function
    | AVariant(loc, pat_loc, name, mems, with_mems) -> self#a_variant(loc, pat_loc, name, mems, with_mems)
    | ASingletonVariant(loc, name, with_mems) -> self#a_singleton_variant(loc, name, with_mems)

  method visit_variant_member = function
    | AVariantMember(loc, mem_type, bind) -> self#a_variant_member(loc, mem_type, bind)

  method visit_cases_bind = function
    | ACasesBind(loc, typ, bind) -> self#a_cases_bind(loc, typ, bind)

  method visit_cases_branch = function
    | ACasesBranch(loc, pat_loc, name, args, body) -> self#a_cases_branch(loc, pat_loc, name, args, body)
    | ASingletonCasesBranch(loc, pat_loc, name, body) -> self#a_singleton_cases_branch(loc, pat_loc, name, body)

  method visit_defined_value = function
    | ADefinedValue(name, value) -> self#a_defined_value(name, value)

  method visit_defined_type = function
    | ADefinedType(name, typ) -> self#a_defined_type(name, typ)

  method visit_lettable = function
    | AModule(loc, ans, v, dv, dt, pv, pt) -> self#a_module(loc, ans, v, dv, dt, pv, pt)
    | ACases(loc, typ, value, branches, _else) -> self#a_cases(loc, typ, value, branches, _else)
    | AIf(loc, cond, tru, fals) -> self#a_if(loc, cond, tru, fals)
    | ADataExpr(loc, name, namet, variants, shared) -> self#a_data_expr(loc, name, namet, variants, shared)
    | AAssign(loc, name, value) -> self#a_assign(loc, name, value)
    | AApp(loc, func, args) -> self#a_app(loc, func, args)
    | AMethodApp(loc, obj, meth, args) -> self#a_method_app(loc, obj, meth, args)
    | APrimApp(loc, func, args) -> self#a_prim_app(loc, func, args)
    | AArray(loc, vals) -> self#a_array(loc, vals)
    | ARef(loc, ann) -> self#a_ref(loc, ann)
    | AObj(loc, fields) -> self#a_obj(loc, fields)
    | AUpdate(loc, obj, fields) -> self#a_update(loc, obj, fields)
    | AExtend(loc, obj, fields) -> self#a_extend(loc, obj, fields)
    | ADot(loc, obj, field) -> self#a_dot(loc, obj, field)
    | AColon(loc, obj, field) -> self#a_colon(loc, obj, field)
    | AGetBang(loc, obj, field) -> self#a_get_bang(loc, obj, field)
    | ALam(loc, args, ret_ann, body) -> self#a_lam(loc, args, ret_ann, body)
    | AMethod(loc, args, ret_ann, body) -> self#a_method(loc, args, ret_ann, body)
    | AVal(loc, value) -> self#a_val(loc, value)

  method visit_field = function
    | AField(loc, name, value) -> self#a_field(loc, name, value)

  method visit_value = function
    | ASrcloc(loc, value) -> self#a_srcloc(loc, value)
    | ANum(loc, value) -> self#a_num(loc, value)
    | AStr(loc, value) -> self#a_str(loc, value)
    | ABool(loc, value) -> self#a_bool(loc, value)
    | AUndefined(loc) -> self#a_undefined(loc)
    | AId(loc, value) -> self#a_id(loc, value)
    | AIdVar(loc, value) -> self#a_id_var(loc, value)
    | AIdLetrec(loc, value, safe) -> self#a_id_letrec(loc, value, safe)
  (* Virtual method declarations *)
  method virtual a_program : (loc * Ast.provide * import list * expr) -> 'prog_ret

  method virtual a_import_complete : (loc * Ast.name list * Ast.name list * import_type * Ast.name * Ast.name)
    -> 'import_ret

  method virtual a_type_bind : (loc * Ast.name * Ast.ann) -> 'type_bind_ret
  method virtual a_newtype_bind : (loc * Ast.name * Ast.name) -> 'type_bind_ret

  method virtual a_type_let : (loc * type_bind * expr) -> 'expr_ret
  method virtual a_let : (loc * bind * lettable * expr) -> 'expr_ret
  method virtual a_var : (loc * bind * lettable * expr) -> 'expr_ret
  method virtual a_seq : (loc * lettable * expr) -> 'expr_ret
  method virtual a_lettable : (loc * lettable) -> 'expr_ret

  method virtual a_bind : (loc * Ast.name * Ast.ann) -> 'bind_ret

  method virtual a_variant : (loc * loc * string * variant_member list * field list) -> 'variant_ret
  method virtual a_singleton_variant : (loc * string * field list) -> 'variant_ret

  method virtual a_variant_member : (loc * member_type * bind) -> 'variant_member_ret

  method virtual a_cases_bind : (loc * Ast.cases_bind_type * bind) -> 'cases_bind_ret

  method virtual a_cases_branch : (loc * loc * string * cases_bind list * expr) -> 'cases_branch_ret
  method virtual a_singleton_cases_branch : (loc * loc * string * expr) -> 'cases_branch_ret

  method virtual a_defined_value : (string * value) -> 'defined_value_ret

  method virtual a_defined_type : (string * Ast.ann) -> 'defined_type_ret

  method virtual a_module : (loc * value * defined_value list *
                             defined_type list * value * Ast.a_field list * value) -> 'lettable_ret
  method virtual a_cases : (loc * Ast.ann * value * cases_branch list * expr) -> 'lettable_ret
  method virtual a_if : (loc * value * expr * expr) -> 'lettable_ret
  method virtual a_data_expr : (loc * string * Ast.name * variant list * field list) -> 'lettable_ret
  method virtual a_assign : (loc * Ast.name * value) -> 'lettable_ret
  method virtual a_app : (loc * value * value list) -> 'lettable_ret
  method virtual a_method_app : (loc * value * string * value list) -> 'lettable_ret
  method virtual a_prim_app : (loc * string * value list) -> 'lettable_ret
  method virtual a_array : (loc * value list) -> 'lettable_ret
  method virtual a_ref : (loc * Ast.ann option) -> 'lettable_ret
  method virtual a_obj : (loc * field list) -> 'lettable_ret
  method virtual a_update : (loc * value * field list) -> 'lettable_ret
  method virtual a_extend : (loc * value * field list) -> 'lettable_ret
  method virtual a_dot : (loc * value * string) -> 'lettable_ret
  method virtual a_colon : (loc * value * string) -> 'lettable_ret
  method virtual a_get_bang : (loc * value * string) -> 'lettable_ret
  method virtual a_lam : (loc * bind list * Ast.ann * expr) -> 'lettable_ret
  method virtual a_method : (loc * bind list * Ast.ann * expr) -> 'lettable_ret
  method virtual a_val : (loc * value) -> 'lettable_ret

  method virtual a_field : (loc * string * value) -> 'field_ret

  method virtual a_srcloc : (loc * loc) -> 'value_ret
  method virtual a_num : (loc * BatNum.num) -> 'value_ret
  method virtual a_str : (loc * string) -> 'value_ret
  method virtual a_bool : (loc * bool) -> 'value_ret
  method virtual a_undefined : loc -> 'value_ret
  method virtual a_id : (loc * Ast.name) -> 'value_ret
  method virtual a_id_var : (loc * Ast.name) -> 'value_ret
  method virtual a_id_letrec : (loc * Ast.name * bool) -> 'value_ret
end

class virtual ['a] folding_visitor = object(self)
  inherit ['a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a] visitor
end

class ['a] default_folding_visitor = object(self)
  inherit ['a] folding_visitor
  val visitor_name = "Folding Visitor"

  method a_program _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_program"
  method a_import_complete _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_import_complete"
  method a_type_bind _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_type_bind"
  method a_newtype_bind _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_newtype_bind"
  method a_type_let _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_type_let"
  method a_let _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_let"
  method a_var _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_var"
  method a_seq _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_seq"
  method a_lettable _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_lettable"
  method a_bind _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_bind"
  method a_variant _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_variant"
  method a_singleton_variant _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_singleton_variant"
  method a_variant_member _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_variant_member"
  method a_cases_bind _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_cases_bind"
  method a_cases_branch _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_cases_branch"
  method a_singleton_cases_branch _ =
    failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_singleton_cases_branch"
  method a_defined_value _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_defined_value"
  method a_defined_type _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_defined_type"
  method a_module _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_module"
  method a_cases _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_cases"
  method a_if _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_if"
  method a_data_expr _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_data_expr"
  method a_assign _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_assign"
  method a_app _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_app"
  method a_method_app _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_method_app"
  method a_prim_app _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_prim_app"
  method a_array _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_array"
  method a_ref _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_ref"
  method a_obj _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_obj"
  method a_update _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_update"
  method a_extend _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_extend"
  method a_dot _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_dot"
  method a_colon _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_colon"
  method a_get_bang _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_get_bang"
  method a_lam _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_lam"
  method a_method _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_method"
  method a_val _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_val"
  method a_field _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_field"
  method a_srcloc _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_srcloc"
  method a_num _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_num"
  method a_str _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_str"
  method a_bool _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_bool"
  method a_undefined _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_undefined"
  method a_id _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_id"
  method a_id_var _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_id_var"
  method a_id_letrec _ = failwith @@ "Visitor " ^ visitor_name ^ " has no method for a_id_letrec"
end

class default_map_visitor = object(self)
  inherit [program,
           import,
           type_bind,
           expr,
           bind,
           variant,
           variant_member,
           cases_bind,
           cases_branch,
           defined_value,
           defined_type,
           lettable,
           field,
           value] visitor

  method a_module(l, a, dv, dt, pr, typs, checks) =
    AModule(l, self#visit_value a, dv, dt, self#visit_value pr, typs, self#visit_value checks)

  method a_import_complete(l, v, t, it, vn, tn) =
    AImportComplete(l, v, t, it, vn, tn)

  method a_defined_value(n, v) =
    ADefinedValue(n, v)

  method a_defined_type(n, t) =
    ADefinedType(n, t)

  method a_program(l, p, imports, body) =
    AProgram(l, p, List.map self#visit_import imports, self#visit_expr body)

  method a_type_bind(l, name, ann) =
    ATypeBind(l, name, ann)

  method a_newtype_bind(l, name, nameb) =
    ANewtypeBind(l, name, nameb)

  method a_type_let(l, bind, body) =
    ATypeLet(l, self#visit_type_bind bind, self#visit_expr body)

  method a_let(l, bind, e, body) =
    ALet(l, self#visit_bind bind, self#visit_lettable e, self#visit_expr body)

  method a_var(l, bind, e, body) =
    AVar(l, self#visit_bind bind, self#visit_lettable e, self#visit_expr body)

  method a_seq(l, e1, e2) =
    ASeq(l, self#visit_lettable e1, self#visit_expr e2)

  method a_cases(l, typ, value, branches, _else) =
    ACases(l, typ, self#visit_value value, List.map self#visit_cases_branch branches, self#visit_expr _else)

  method a_cases_bind(l, typ, bind) =
    ACasesBind(l, typ, self#visit_bind bind)

  method a_cases_branch(l, pat_loc, name, args, body) =
    ACasesBranch(l, pat_loc, name, List.map self#visit_cases_bind args, self#visit_expr body)

  method a_singleton_cases_branch(l, pat_loc, name, body) =
    ASingletonCasesBranch(l, pat_loc, name, self#visit_expr body)

  method a_data_expr(l, name, namet, variants, shared) =
    ADataExpr(l, name, namet, List.map self#visit_variant variants, List.map self#visit_field shared)

  method a_variant(l, constr_loc, name, members, with_members) =
    AVariant(l, constr_loc, name, List.map self#visit_variant_member members,
             List.map self#visit_field with_members)

  method a_singleton_variant(l, name, with_members) =
    ASingletonVariant(l, name, List.map self#visit_field with_members)

  method a_variant_member(l, member_type, bind) =
    AVariantMember(l, member_type, self#visit_bind bind)

  method a_if(l, c, t, e) =
    AIf(l, self#visit_value c, self#visit_expr t, self#visit_expr e)

  method a_lettable(l, e) =
    ALettable(l, self#visit_lettable e)

  method a_assign(l, id, value) =
    AAssign(l, id, self#visit_value value)

  method a_app(l, _fun, args) =
    AApp(l, self#visit_value _fun, List.map self#visit_value args)

  method a_method_app(l, obj, meth, args) =
    AMethodApp(l, self#visit_value obj, meth, List.map self#visit_value args)

  method a_prim_app(l, f, args) =
    APrimApp(l, f, List.map self#visit_value args)

  method a_ref(l, ann) =
    ARef(l, ann)

  method a_obj(l, fields) =
    AObj(l, List.map self#visit_field fields)

  method a_update(l, supe, fields) =
    AUpdate(l, self#visit_value supe, List.map self#visit_field fields)

  method a_extend(l, supe, fields) =
    AExtend(l, self#visit_value supe, List.map self#visit_field fields)

  method a_dot(l, obj, field) =
    ADot(l, self#visit_value obj, field)

  method a_colon(l, obj, field) =
    AColon(l, self#visit_value obj, field)

  method a_get_bang(l, obj, field) =
    AGetBang(l, self#visit_value obj, field)

  method a_lam(l, args, ret, body) =
    ALam(l, List.map self#visit_bind args, ret, self#visit_expr body)

  method a_method(l, args, ret, body) =
    AMethod(l, List.map self#visit_bind args, ret, self#visit_expr body)

  method a_val(l, v) =
    AVal(l, self#visit_value v)

  method a_bind(l, id, ann) =
    ABind(l, id, ann)

  method a_field(l, name, value) =
    AField(l, name, self#visit_value value)

  method a_srcloc(l, loc) =
    ASrcloc(l, loc)

  method a_num(l, n) =
    ANum(l, n)

  method a_array(l, vals) =
    AArray(l, List.map self#visit_value vals)

  method a_str(l, s) =
    AStr(l, s)

  method a_bool(l, b) =
    ABool(l, b)

  method a_undefined(l) =
    AUndefined(l)

  method a_id(l, id) =
    AId(l, id)

  method a_id_var(l, id) =
    AIdVar(l, id)

  method a_id_letrec(l, id, safe) =
    AIdLetrec(l, id, safe)
end


(* TODO: Is this used? If so, why was freevars_ann_acc incompatible with the current Ast.ann defn? *)
let rec freevars_list_acc (anns : Ast.ann list) seen_so_far =
  List.fold_left (fun acc a -> freevars_ann_acc a acc) seen_so_far anns

and freevars_ann_acc (ann : Ast.ann) (seen_so_far : Ast.name SM.t) =
  let lst_a = fun x -> freevars_list_acc x seen_so_far in
  let afield_ann af = match af with
    | Ast.AField(_,_,a) -> a in
  match ann with
  | Ast.ABlank -> seen_so_far
  | Ast.AAny -> seen_so_far
  | Ast.AName(_,name) -> SM.add (Ast.name_key name) name seen_so_far
  | Ast.ATypeVar(_,_) -> seen_so_far
  | Ast.ADot(_,left,_) -> SM.add (Ast.name_key left) left seen_so_far
  | Ast.AArrow(_,args,ret,_) -> lst_a (ret::args)
  | Ast.AMethod(_,args,ret) -> lst_a (ret::args)
  | Ast.ARecord(_,fields) -> lst_a (List.map afield_ann fields)
  | Ast.AApp(_,a,args) -> lst_a (a::args)
  | Ast.APred(_,a,pred) ->
    let name = (match pred with
        | Ast.SId(_,n) -> n
        | Ast.SIdLetrec(_,n,_) -> n
        | _ -> failwith "Invalid APred") in
    freevars_ann_acc a (SM.add (Ast.name_key name) name seen_so_far)
  | Ast.AChecked(_,_) -> seen_so_far
