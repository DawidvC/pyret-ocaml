open PyretUtils

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
  | UnableToInfer of loc
  | CantTypecheck of string * loc
  | Unsupported of string * loc
  | NoModule of loc * string

let render_reason =
  let open ErrorDisplay in
  let draw_and_highlight l = LocDisplay(l,"error-highlight",Loc(l)) in
  let lst x = [x] in
  function
  | WFErr(msg,loc) ->
    error [para [Text("Well-formedness:");Text(msg);Text("at")]; draw_and_highlight loc]
  | WFErrSplit(msg, loc) ->
    error [para [Text("Well-formedness:"); Text(msg); Text("at")];
           VSequence(List.map (para ||> lst ||> draw_and_highlight) loc)]
  | ReservedName(loc, id) ->
    error [para [Text("Well-formedness: Pyret disallows the use of");
                 Code(Text(id)); Text("as an identifier")]; draw_and_highlight loc]
  | ZeroFraction(loc, numerator) ->
    error [para [Text("Well-formedness: fraction literal with zero denominator (numerator was");
                 Text(Ast.expr_to_string numerator);
                 Text(") at")]; draw_and_highlight loc]
  | UnderscoreAsExpr(l) ->
    error [para [Text("Underscore used as an expression, which is not allowed, at ")];
           draw_and_highlight l]
  | UnderscoreAsAnn(l) ->
    error [para [Text("Underscore used as an annotation, which is not allowed, at ")];
           draw_and_highlight l]
  | UnboundId(id) ->
    begin
      match id with
      | Ast.SId(l, name)
      | Ast.SIdVar(l, name)
      | Ast.SIdLetrec(l, name, _) ->
        (match l with
         | Ast.Srcloc.Builtin(_) ->
           error [para [Text("ERROR: should not be allowed to have a builtin that's unbound:");
                        Text(Ast.name_toname name)]; draw_and_highlight l]
         | Ast.Srcloc.Srcloc(_, _, _, _, _, _, _) ->
           error [para [Text("The name"); Code(Text(Ast.name_toname name));
                        Text("is used but not defined at")]; draw_and_highlight l])
      | _ -> failwith "Impossible: Non-id given to UnboundId"
    end
  | UnboundVar(id, loc) ->
    begin
      match loc with
      | Ast.Srcloc.Builtin(_) ->
        error [para [Text("ERROR: should not be allowed to have a builtin that's unbound:");
                     Text(id)]; draw_and_highlight loc]
      | Ast.Srcloc.Srcloc(_, _, _, _, _, _, _) ->
        error [para [Text("The variable"); Code(Text(id));
                     Text("is assigned to, but not defined, at")]; draw_and_highlight loc]
    end
  | UnboundTypeId(ann) ->
    begin
      let loc = Ast.ann_loc ann in
      match loc with
      | Ast.Srcloc.Builtin(_) ->
        error [para [Text("ERROR: should not be allowed to have a builtin that's unbound:");
                     Text(PPrint.pretty_string (Ast.ann_tosource ann) 1000)];
               draw_and_highlight loc]
      | Ast.Srcloc.Srcloc(_, _, _, _, _, _, _) ->
        match ann with
        | Ast.ATypeVar(_, id)
        | Ast.AName(_, id) ->
          error [para [Text("The name"); Code(Text(Ast.name_toname id));
                       Text("is used as a type but not defined as one, at")];
                 draw_and_highlight loc]
        | _ -> failwith ("Impossible: Non-TypeVar/Name given to UnboundTypeId: " ^
                         (Sexplib.Sexp.to_string_hum @@ Ast.sexp_of_ann ann))
    end
  | UnexpectedTypeVar(loc, name) ->
    error [para [Text("Identifier"); Code(Text(Ast.name_toname name));
                 Text("is used in a dot-annotation at");
                 draw_and_highlight loc;
                 Text(", but is bound as a type variable")]]
  | PointlessVar(loc) ->
    begin
      match loc with
      | Ast.Srcloc.Builtin(_) ->
        error [para [Text("ERROR: should not be allowed to have a builtin that's anonymous:");
                     draw_and_highlight loc]]
      | Ast.Srcloc.Srcloc(_, _, _, _, _, _, _) ->
        error [para [Text("Defining an anonymous variable is pointless: there is no name to modify.");
                     Text("Either give this expression a name, or "
                          ^ "bind it to an identifier rather than a variable")]; draw_and_highlight loc]
    end
  | PointlessRec(loc) ->
    begin
      match loc with
      | Ast.Srcloc.Builtin(_) ->
        error [para [Text("ERROR: should not be allowed to have a builtin that's anonymous:");
                     draw_and_highlight loc]]
      | Ast.Srcloc.Srcloc(_, _, _, _, _, _, _) ->
        error [para [Text("Defining an anonymous recursive identifier is pointless: "
                            ^ "there is no name to call recursively.");
                     Text("Either give this expression a name, or remove the rec annotation.")];
               draw_and_highlight loc]
    end
  | PointlessShadow(loc) ->
    begin
      match loc with
      | Ast.Srcloc.Builtin(_) ->
        error [para [Text("ERROR: should not be allowed to have a builtin that's anonymous:");
                     draw_and_highlight loc]]
      | Ast.Srcloc.Srcloc(_, _, _, _, _, _, _) ->
        error [para [Text("Anonymous identifier cannot shadow anything: there is no name to shadow.");
                     Text("Either give this expression a name, or remove the shadow annotation")];
               draw_and_highlight loc]
    end
  | BadAssignment(id, loc, prev_loc) ->
    let is_bad =
      para [Text("The name"); Code(Text(id)); Text("is defined as an identifier,");
            Text("but it is assigned as if it were a variable at"); draw_and_highlight loc] in
    begin
      match loc with
      | Ast.Srcloc.Builtin(_) -> error [is_bad]
      | Ast.Srcloc.Srcloc(_, _, _, _, _, _, _) ->
        error [is_bad; para [Text("One possible fix is to change the declaration of");
                             Code(Text(id)); Text("to use");
                             Code(Text("var")); Text("at");
                             draw_and_highlight prev_loc]]
    end
  | MixedIdVar(id, var_loc, id_loc) ->
    (* TODO *)
    error [para [Code(Text(id)); Text("is declared as bot a variable (at");
                 draw_and_highlight var_loc;
                 Text(") and an identifier (at");
                 LocDisplay(id_loc, "error-highlight",
                            Text(Ast.Srcloc.format
                                   (not (Ast.Srcloc.same_file var_loc id_loc)) id_loc));
                 Text(")")]]
  | ShadowId(id, new_loc, old_loc)
  | DuplicateId(id, new_loc, old_loc) ->
    begin
      match old_loc with
      | Ast.Srcloc.Builtin(_) ->
        error [para [Text("The name"); Code(Text(id)); Text("is already defined.");
                     Text("You need to pick a different name for"); Code(Text(id));
                     Text("at"); draw_and_highlight new_loc]]
      | Ast.Srcloc.Srcloc(_, _, _, _, _, _, _) ->
        error [para [Text("It looks like you've defined the name"); Code(Text(id));
                     Text("twice, at")];
               para [draw_and_highlight old_loc; Text("and")];
               para_nospace [draw_and_highlight new_loc; Text(".")];
               para [Text("You need to pick a different name for one of them.")]]
    end
  | DuplicateField(id, new_loc, old_loc) ->
    begin
      match old_loc with
      | Ast.Srcloc.Builtin(_) ->
        error [para [Text("The field name"); Code(Text(id)); Text("is already defined.");
                     Text("You need to pick a different name for"); Code(Text(id));
                     Text("at"); draw_and_highlight new_loc]]
      | Ast.Srcloc.Srcloc(_, _, _, _, _, _, _) ->
        error [para [Text("It looks like you've defined the field name"); Code(Text(id));
                     Text("twice, at")];
               para [draw_and_highlight old_loc; Text("and")];
               para_nospace [draw_and_highlight new_loc; Text(".")];
               para [Text("You need to pick a different name for one of them.")]]
    end
  | IncorrectType(bad_name, bad_loc, expected_name, expected_loc) ->
    error [para_nospace
             [Text("Expected to find "); Code(Text(expected_name));
              Text(" at"); draw_and_highlight bad_loc;
              Text(", required by"); draw_and_highlight expected_loc;
              Text(", but instead found"); Code(Text(bad_name)); Text(".")]]
  | BadTypeInstantiation(wanted, given, loc) ->
    error [para [Text("Expected to recieve"); Text(string_of_int wanted);
                 Text("arguments for type instantiation at"); draw_and_highlight loc;
                 Text(", but instead recieved"); Text(string_of_int given)]]
  | IncorrectNumberOfArgs(loc) ->
    error [para [Text("Incorrect number of arguments given to function at ");
                 draw_and_highlight loc]]
  | ApplyNonFunction(loc, typ) ->
    error [para [Text("Tried to apply a non-function type");
                 Code(Text(PPrint.pretty_string (Ast.ann_tosource typ) 80));
                 Text("at");
                 draw_and_highlight loc]]
  | ObjectMissingField(field_name, obj, obj_loc, access_loc) ->
    error [para [Text("The object of type"); Code(Text(obj)); Text(" (at");
                 draw_and_highlight obj_loc;
                 Text(") does not have the field \"" ^ field_name ^ "\", accessed at");
                 draw_and_highlight access_loc]]
  | UnneccesaryBranch(branch_name, branch_loc, type_name, type_loc) ->
    error [para [Text("The branch"); Code(Text(branch_name));
                 Text("is not a variant of"); Code(Text(type_name));
                 Text("at"); draw_and_highlight type_loc]]
  | UnneccesaryElseBranch(type_name, loc) ->
    error [para [Text("The else branch for the cases expression at");
                 draw_and_highlight loc;
                 Text("is not needed since all variants of");
                 Code(Text(type_name));
                 Text("have been exhausted.")]]
  | NonExhaustivePattern(missing, type_name, loc) ->
    error [para_nospace
             [Text("The cases expression at "); draw_and_highlight loc;
              Text(" does not exhaust all variants of ");
              Code(Text(type_name));
              Text("It is missing: " ^ (join_str missing ", ") ^ ".");]]
  | CantMatchOn(type_name, loc) ->
    error [para [Text("The type specified");
                 Code(Text(type_name));
                 Text("at");
                 draw_and_highlight loc;
                 Text("cannot be used in a cases expression.")]]
  | IncorrectNumberOfBindings(variant_name, loc, given, expected) ->
    error [para_nospace [Text("Incorrect number of bindings given to ");
                         Text("the variant " ^ variant_name);
                         Text(" at ");
                         draw_and_highlight loc;
                         Text(". ");
                         Text("Given " ^ (string_of_int given) ^ " , ");
                         Text("but expected " ^ (string_of_int expected) ^ ".")]]
  | CasesSingletonMismatch(name, branch_loc, should_be_singleton) ->
    if should_be_singleton then
      error [para [Text("The cases branch named"); Code(Text(name));
                   Text("at"); draw_and_highlight branch_loc;
                   Text("has an argument list, but the variant is a singleton.")]]
    else
      error [para [Text("The cases branch named"); Code(Text(name));
                   Text("at"); draw_and_highlight branch_loc;
                   Text("doesn't have an argument list, but the variant is not a singleton.")]]
  | GivenParameters(data_type, loc) ->
    error [para [Text("The data type"); Code(Text(data_type));
                 Text("does not take any parameters, but is given some at");
                 draw_and_highlight loc]]
  | UnableToInstantiate(loc) ->
    error [para [Text("In the type at"); draw_and_highlight loc;
                 Text("there was not enough information to instantiate the type, "
                      ^ "or the given arguments are incompatible.")]]
  | UnableToInfer(loc) ->
    error [para_nospace [Text("Unable to infer the type of ");
                         draw_and_highlight loc;
                         Text(". Please add an annotation.")]]
  | CantTypecheck(reason, loc) ->
    error [para [Text("This program cannot be type-checked. Please send it to the developers. "
                      ^ "The reason it cannot be type-checked is: " ^ reason ^" at");
                 draw_and_highlight loc]]
  | Unsupported(message, blame_loc) ->
    error [para_nospace [Text(message ^ " (found at ");
                         draw_and_highlight blame_loc;
                         Text(")")]]
  | NoModule(loc, mod_name) ->
    error [para_nospace
             [Text("There is no module imported with the name " ^ mod_name ^ " ");
              Text("(used at "); draw_and_highlight loc; Text(")")]]

let to_vs =
  let open ValueSkeleton in
  let loc_to_vs l = VSStr(Ast.str_of_loc l) in
  function
  | WFErr(msg,loc) -> VSConstr("wf-err", [VSStr(msg); loc_to_vs loc])
  | _ -> failwith "NYI (to_vs) CompileError.t"
