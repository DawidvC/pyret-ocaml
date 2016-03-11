module A = Ast
module LA = ListAux
module U = PyretUtils
module SD = U.StringDict
module MSD = U.MutableStringDict
module VS = ValueSkeleton

let all2_strict = LA.all2_strict
and map2_strict = LA.map2_strict
and fold2_strict = LA.fold2_strict

type name = A.name

let dict_to_string : 'a. ('a -> VS.t) -> 'a SD.t -> VS.t = fun to_vs dict ->
  let items = SD.fold (fun key value -> function
      | [] -> [VS.VSStr(key); VS.VSStr(" => "); to_vs value]
      | (_ as acc) ->
        [VS.VSStr(key); VS.VSStr(" => "); to_vs value; VS.VSStr(", ")] @ acc) dict [] in
  VS.VSSeq([VS.VSStr("{")] @ items @ [VS.VSStr("}")])

let mut_dict_to_string : 'a. ('a -> VS.t) -> 'a MSD.t -> VS.t = fun to_vs dict ->
  let items = MSD.fold (fun key value -> function
      | [] -> [VS.VSStr(key); VS.VSStr(" => "); to_vs value]
      | (_ as acc) ->
        [VS.VSStr(key); VS.VSStr(" => "); to_vs value; VS.VSStr(", ")] @ acc) dict [] in
  VS.VSSeq([VS.VSStr("{")] @ items @ [VS.VSStr("}")])

let rec interleave lst item =
  match lst with
  | []
  | _ :: [] -> lst
  | hd :: tl -> hd :: (item :: (interleave tl item))

module Pair = U.Pair

type ('a, 'b) pair = ('a, 'b) Pair.t

let on_left = Pair.on_left

let on_right = Pair.on_right

module Comparison = struct
  type t =
    | LessThan
    | Equal
    | GreaterThan

  let compare : t -> t -> t = fun l r ->
    match (l,r) with
    | (_,Equal) -> l
    | (LessThan, LessThan) -> Equal
    | (_, LessThan) -> GreaterThan
    | (GreaterThan, GreaterThan) -> Equal
    | (_, GreaterThan) -> LessThan

  let std_compare : 'a. 'a -> 'a -> t  = fun a b ->
    if a < b then LessThan
    else if a > b then GreaterThan
    else Equal

  let rec list_compare : 'a 'b. ('a -> 'b -> t) -> 'a list -> 'b list -> t = fun comp a b ->
    match (a,b) with
    | ([],[]) -> Equal
    | ([],_ ) -> LessThan
    | ( _,[]) -> GreaterThan
    | (ahd :: atl, bhd :: btl) ->
      match (comp ahd bhd) with
      | Equal -> list_compare comp atl btl
      | (_ as res) -> res

  let rec fold_comparisons : t list -> t = function
    | [] -> Equal
    | hd :: tl ->
      match hd with
      | Equal -> fold_comparisons tl
      | (_ as res) -> res

  let equal = (=)
  let hash = Hashtbl.hash
end

module Variance = struct
  type t =
      Constant
    | Bivariant
    | Covariant
    | Contravariant
    | Invariant

  let join (a : t) (b : t) : t =
    match (a,b) with
    | (Constant,_) -> b
    | (Bivariant,Constant) -> Bivariant
    | (Bivariant,_) -> b
    | (Covariant,Constant)
    | (Covariant,Bivariant)
    | (Covariant,Covariant) -> Covariant
    | (Covariant,Contravariant)
    | (Covariant,Invariant) -> Invariant
    | (Contravariant,Constant)
    | (Contravariant,Bivariant)
    | (Contravariant,Contravariant) -> Contravariant
    | (Contravariant,Covariant)
    | (Contravariant,Invariant) -> Invariant
    | (Invariant,_) -> Invariant

  let flip (v : t) : t =
    match v with
    | Constant
    | Bivariant
    | Invariant -> v
    | Covariant -> Contravariant
    | Contravariant -> Covariant

  let equal = (=)
  let hash = Hashtbl.hash
end

module rec TypeMember : sig
  type t = TMember of string * Type.t * A.loc

  val to_vs : t -> VS.t
  val key : t -> string
  val substitute : Type.t -> Type.t -> t -> t
  val set_loc : A.loc -> t -> t
  val compare : t -> t -> Comparison.t

  (** Equality for hash table usage *)
  val equal : t -> t -> bool
  (** Hashing function *)
  val hash : t -> int
end = struct
  type t = TMember of string * Type.t * A.loc

  let to_vs = function
    | TMember(field_name,typ,_) ->
      VS.VSSeq([VS.VSStr(field_name); VS.VSStr(" : "); Type.to_vs typ])

  let key = function
    | TMember(field_name,typ,_) ->
      field_name ^ " : " ^ (Type.key typ)

  let substitute (x : Type.t) (r : Type.t) = function
    | TMember(field_name,typ,l) ->
      TMember(field_name,(Type.substitute typ x r),l)

  let set_loc (loc : A.loc) = function
    | TMember(field_name,typ,_) ->
      TMember(field_name,(Type.set_loc loc typ),loc)

  let compare (a : t) (b : t) : Comparison.t =
    match (a,b) with
    | (TMember(a_field_name,a_typ,_),TMember(b_field_name,b_typ,_)) ->
      Comparison.fold_comparisons [
        Comparison.std_compare a_field_name b_field_name;
        Type.compare a_typ b_typ
      ]

  let equal = (=)
  let hash = Hashtbl.hash
end

and TypeVariant : sig
  type t = TVariant of string * TypeMember.t list * TypeMember.t list * A.loc
         | TSingletonVariant of string * TypeMember.t list * A.loc
  val substitute : Type.t -> Type.t -> t -> t
  val set_loc : A.loc -> t -> t
  val name : t -> string
  val fields : t -> TypeMember.t list
  val to_vs : t -> VS.t

  (** Equality for hash table usage *)
  val equal : t -> t -> bool
  (** Hashing function *)
  val hash : t -> int
end = struct
  type t = TVariant of string * TypeMember.t list * TypeMember.t list * A.loc
         | TSingletonVariant of string * TypeMember.t list * A.loc

  let substitute (x : Type.t) (r : Type.t) = function
    | TVariant(name,fields,with_fields,l) ->
      let substitute = TypeMember.substitute x r in
      TVariant(name, List.map substitute fields, List.map substitute with_fields, l)
    | TSingletonVariant(name,with_fields,l) ->
      let substitute = TypeMember.substitute x r in
      TSingletonVariant(name,List.map substitute with_fields, l)

  let set_loc (loc : A.loc) =
    let set_loc = TypeMember.set_loc loc in
    function
    | TVariant(name,fields,with_fields,_) ->
      TVariant(name,List.map set_loc fields, List.map set_loc with_fields, loc)
    | TSingletonVariant(name,with_fields,_) ->
      TSingletonVariant(name,List.map set_loc with_fields, loc)

  let name = function
    | TVariant(n,_,_,_)
    | TSingletonVariant(n,_,_) -> n

  let fields = function
    | TVariant(_,variant_fields, with_fields, _) -> with_fields @ variant_fields
    | TSingletonVariant(_,with_fields,_) -> with_fields

  let to_vs tv =
    match tv with
    | TVariant(name,_,_,_)
    | TSingletonVariant(name,_,_) ->
      VS.VSConstr(name, List.map TypeMember.to_vs (fields tv))

  let equal = (=)
  let hash = Hashtbl.hash

end

and ModuleType : sig
  type t = TModule of string * Type.t * Type.t SD.t * Type.t SD.t

  val to_vs : t -> VS.t
  (** Equality for hash table usage *)
  val equal : t -> t -> bool
  (** Hashing function *)
  val hash : t -> int
end = struct
  type t = TModule of string * Type.t * Type.t SD.t * Type.t SD.t

  let to_vs = function
    | TModule(name,provides,types,aliases) ->
      VS.VSConstr("t-module", [
          VS.VSStr(name);
          Type.to_vs provides;
          dict_to_string Type.to_vs types;
          dict_to_string Type.to_vs aliases
        ])

  let equal = (=)
  let hash = Hashtbl.hash
end

and Type : sig
  type t =
      TName of string option * name * A.loc
    | TVar of name * A.loc
    | TArrow of t list * t * A.loc
    | TApp of t * t list * A.loc (** type%(is-t-name) * List<Type>%(is-link) * A.loc *)
    | TTop of A.loc
    | TBot of A.loc
    | TRecord of TypeMember.t list * A.loc
    | TForall of t list * t * A.loc
    | TRef of t * A.loc
    | TExistential of name * A.loc
    | TData of t list * TypeVariant.t list * TypeMember.t list * A.loc


  val lookup_variant : string -> t -> TypeVariant.t option
  val introduce : t list -> t -> t
  val key : t -> string
  val substitute : t -> t -> t -> t
  val set_loc : A.loc -> t -> t
  val compare : t -> t -> Comparison.t
  val to_vs : t -> VS.t

  (** Equality for hash table usage *)
  val equal : t -> t -> bool
  (** Hashing function *)
  val hash : t -> int
end = struct
  type t =
      TName of string option * name * A.loc
    | TVar of name * A.loc
    | TArrow of t list * t * A.loc
    | TApp of t * t list * A.loc (** type%(is-t-name) * List<Type>%(is-link) * A.loc *)
    | TTop of A.loc
    | TBot of A.loc
    | TRecord of TypeMember.t list * A.loc
    | TForall of t list * t * A.loc
    | TRef of t * A.loc
    | TExistential of name * A.loc
    | TData of t list * TypeVariant.t list * TypeMember.t list * A.loc

  let lookup_variant variant_name = function
    | TData(_,variants,_,_) ->
      let same_name tv = (TypeVariant.name tv) = variant_name in
      U.list_find same_name variants
    | _ -> failwith "lookup_variant called on non-data"

  let rec key = function
    | TName(module_name,id,_) ->
      (match module_name with
       | None -> ""
       | Some(module_name) -> module_name ^ ".") ^ (A.name_key id)
    | TVar(id,_) -> A.name_key id
    | TArrow(args,ret,_) ->
      "("^
      (U.join_str (List.map key args) ", ")^
      " -> " ^ (key ret)
      ^")"
    | TApp(onto,args,_) ->
      (key onto) ^ "<" ^ (U.join_str (List.map key args) ", ") ^ ">"
    | TTop(_) -> "Top"
    | TBot(_) -> "Bot"
    | TRecord(fields,_) ->
      "{"^
      (U.join_str (List.map TypeMember.key fields) ", ")
      ^"}"
    | TForall(introduces,onto,_) ->
      "<"^
      (U.join_str (List.map key introduces) ",")
      ^">"^(key onto)
    | TRef(typ,_) ->
      "ref "^(key typ)
    | TExistential(id,_) -> A.name_key id
    | TData(params,variants,fields,_) ->
      "data<"^(U.join_str (List.map key params) ",")^">"
      ^(U.join_str (List.map TypeVariant.name variants) "+")

  let rec substitute (orig_typ : t) (new_typ : t) (typ : t) : t =
    if typ = orig_typ then new_typ
    else
      let substitute = substitute orig_typ new_typ in
      match typ with
      | TArrow(args,ret,l) ->
        let new_args = List.map substitute args
        and new_ret = substitute ret in
        TArrow(new_args, new_ret, l)
      | TApp(onto,args,l) ->
        let new_onto = substitute onto
        and new_args = List.map substitute args in
        TApp(new_onto, new_args, l)
      | TForall(introduces,onto,l) ->
        let new_onto = substitute onto in
        TForall(introduces, new_onto, l)
      | TRef(arg_typ, l) ->
        let new_arg_typ = substitute arg_typ in
        TRef(new_arg_typ, l)
      | TData(params,variants,fields,l) ->
        TData(params,
              List.map (TypeVariant.substitute orig_typ new_typ) variants,
              List.map (TypeMember.substitute orig_typ new_typ) fields,
              l)
      | _ -> typ

  let set_loc (loc : A.loc) = function
    | TName(module_name, id, _) -> TName(module_name, id, loc)
    | TVar(id,_) -> TVar(id, loc)
    | TArrow(args,ret,_) -> TArrow(args,ret,loc)
    | TApp(onto,args,_) -> TApp(onto,args,loc)
    | TTop(_) -> TTop(loc)
    | TBot(_) -> TBot(loc)
    | TRecord(fields,_) -> TRecord(fields,loc)
    | TForall(introduces,onto,_) -> TForall(introduces,onto,loc)
    | TRef(typ,_) -> TRef(typ,loc)
    | TExistential(id,_) -> TExistential(id,loc)
    | TData(params,variants,fields,_) -> TData(params,variants,fields,loc)

  let introduce type_args = function
    | (TData(params,_,_,_) as typ) ->
      List.fold_left2 (fun new_typ param_typ arg_typ ->
          substitute param_typ arg_typ new_typ) typ params type_args
    | _ -> failwith "introduce called on non-data"

  let rec to_vs = function
    | TName(module_name,id,_) -> VS.VSStr(A.name_toname id)
    | TVar(id,_) -> VS.VSStr(A.name_toname id)
    | TArrow(args,ret,_) ->
      VS.VSSeq([
          VS.VSStr("(")]
          @ interleave (List.map to_vs args) (VS.VSStr(", "))
          @ [VS.VSStr(" -> ");
             to_vs ret;
             VS.VSStr(")")
            ])
    | TApp(onto,args,_) ->
      VS.VSSeq([
          to_vs onto;
          VS.VSStr("<")]
          @ interleave (List.map to_vs args) (VS.VSStr(", "))
          @ [VS.VSStr(">")])
    | TTop(_) -> VS.VSStr("Any")
    | TBot(_) -> VS.VSStr("Bot")
    | TRecord(fields,_) ->
      VS.VSSeq(((VS.VSStr("{"))::(interleave (List.map TypeMember.to_vs fields) (VS.VSStr(", "))))
               @ [VS.VSStr("}")])
    | TForall(introduces,onto,_) ->
      VS.VSSeq(((VS.VSStr("forall "))::(interleave (List.map to_vs introduces) (VS.VSStr(", "))))
               @ [VS.VSStr("."); to_vs onto])
    | TRef(typ,_) -> VS.VSSeq([VS.VSStr("ref "); to_vs typ])
    | TExistential(id,_) -> VS.VSStr(A.name_key id)
    | TData(params,variants,fields,_) ->
      VS.VSSeq(((VS.VSStr("("))::(interleave (List.map TypeVariant.to_vs variants) (VS.VSStr(" + "))))
               @ [VS.VSStr(")")])

  let rec compare (t1 : t) (t2 : t) : Comparison.t =
    match (t1,t2) with
    | (TTop(_),TTop(_))
    | (TBot(_),TBot(_)) -> Comparison.Equal
    | (TBot(_),_) -> Comparison.LessThan
    | (_,TBot(_)) -> Comparison.GreaterThan
    | (TTop(_),_) -> Comparison.GreaterThan
    | (_,TTop(_)) -> Comparison.LessThan
    | (TName(a_module_name,a_id,_), TName(b_module_name,b_id,_)) ->
      let or_empty_str o = match o with
        | None -> ""
        | Some(n) -> n in
      Comparison.fold_comparisons [
        Comparison.std_compare (or_empty_str a_module_name) (or_empty_str b_module_name);
        Comparison.std_compare a_id b_id
      ]
    | (TName(_,_,_),_) -> Comparison.LessThan
    | (TVar(_,_),TName(_,_,_))
    | (TVar(_,_),TExistential(_,_)) -> Comparison.GreaterThan
    | (TVar(a_id,_),TVar(b_id,_)) -> Comparison.std_compare a_id b_id
    | (TVar(_,_),_) -> Comparison.LessThan
    | (TArrow(_,_,_),TName(_,_,_))
    | (TArrow(_,_,_),TVar(_,_))
    | (TArrow(_,_,_),TExistential(_,_)) -> Comparison.GreaterThan
    | (TArrow(a_args,a_ret,_),TArrow(b_args,b_ret,_)) ->
      Comparison.fold_comparisons [
        Comparison.list_compare compare a_args b_args;
        compare a_ret b_ret
      ]
    | (TArrow(_,_,_),_) -> Comparison.LessThan
    | (TApp(_,_,_),TName(_,_,_))
    | (TApp(_,_,_),TVar(_,_))
    | (TApp(_,_,_),TExistential(_,_))
    | (TApp(_,_,_),TArrow(_,_,_)) -> Comparison.GreaterThan
    | (TApp(a_onto,a_args,_),TApp(b_onto,b_args,_)) ->
      Comparison.fold_comparisons [
        Comparison.list_compare compare a_args b_args;
        compare a_onto b_onto
      ]
    | (TApp(_,_,_),_) -> Comparison.LessThan
    | (TRecord(_,_),TRef(_,_))
    | (TRecord(_,_),TForall(_,_,_))
    | (TRecord(_,_),TData(_,_,_,_)) -> Comparison.LessThan
    | (TRecord(a_fields,_),TRecord(b_fields,_)) ->
      Comparison.list_compare TypeMember.compare a_fields b_fields
    | (TRecord(_,_),_) -> Comparison.GreaterThan
    | (TData(_,_,_,_),TRef(_,_))
    | (TData(_,_,_,_),TForall(_,_,_)) -> Comparison.LessThan
    | (TData(_,_,a_fields,_),TData(_,_,b_fields,_)) ->
      Comparison.list_compare TypeMember.compare a_fields b_fields
    | (TData(_,_,_,_),_) -> Comparison.GreaterThan
    | (TForall(_,_,_),TRef(_,_)) -> Comparison.LessThan
    | (TForall(a_introduces,a_onto,_),TForall(b_introduces,b_onto,_)) ->
      Comparison.fold_comparisons [
        Comparison.list_compare compare a_introduces b_introduces;
        compare a_onto b_onto
      ]
    | (TForall(_,_,_),_) -> Comparison.GreaterThan
    | (TRef(a_typ,_),TRef(b_typ,_)) -> compare a_typ b_typ
    | (TRef(_,_),_) -> Comparison.GreaterThan
    | (TExistential(_,_),TName(_,_,_)) -> Comparison.GreaterThan
    | (TExistential(a_id,_),TExistential(b_id,_)) -> Comparison.std_compare a_id b_id
    | (TExistential(_,_),_) -> Comparison.LessThan

  let equal = (=)
  let hash = Hashtbl.hash
end

let type_members_lookup type_members field_name =
  let same_field = function
    | TypeMember.TMember(fn,_,_) -> fn = field_name in
  U.list_find same_field type_members

let builtin_uri = Some("builtin")

let t_array_name = Type.TName(None, A.STypeGlobal("RawArray"), A.dummy_loc)

let t_number l = Type.TName(builtin_uri, A.STypeGlobal("Number"), l)
let t_string l = Type.TName(builtin_uri, A.STypeGlobal("String"), l)
let t_boolean l = Type.TName(builtin_uri, A.STypeGlobal("Boolean"), l)
let t_nothing l = Type.TName(builtin_uri, A.STypeGlobal("Nothing"), l)
let t_srcloc l = Type.TName(builtin_uri, A.STypeGlobal("Loc"), l)
let t_array v l = Type.TApp(t_array_name, [v], l)
let t_option v l = Type.TApp(
    Type.TName(Some("pyret-builtin://option"), A.SGlobal("Option"), l), [v], l)
