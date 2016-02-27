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
