open TypeStructs
open TypeCheckStructs
open PyretUtils

let all2_strict = ListAux.all2_strict
let map2_strict = ListAux.map2_strict
let fold2_strict = ListAux.fold2_strict

let mk_t_binop constr loc = Type.TArrow([constr loc; constr loc], (constr loc), loc)
let mk_t_cmp constr loc = Type.TArrow([constr loc; constr loc], (t_boolean loc), loc)
let t_num_binop = mk_t_binop t_number
let t_num_cmp = mk_t_cmp t_number
let t_str_binop = mk_t_binop t_string
let t_str_cmp = mk_t_cmp t_string
let t_method_binop (field_name : string) loc =
  Type.TForall([Type.TVar(A.SAtom("B",1),loc); Type.TVar(A.SAtom("C",1),loc)],
               (Type.TArrow([Type.TRecord(
                    [
                      TypeMember.TMember(field_name, Type.TArrow(
                          [Type.TVar(A.SAtom("B",1),loc)],
                          Type.TVar(A.SAtom("C",1),loc),loc), loc)
                    ],
                    loc)], Type.TVar(A.SAtom("C",1),loc),
                            loc)), loc)

(* Math operators *)
let t_plus_method = t_method_binop "_plus"
let t_minus_method = t_method_binop "_minus"
let t_divide_method = t_method_binop "_divide"
let t_times_method = t_method_binop "_times"

(* Comparison operators *)
let t_lt_method = t_method_binop "_lessthan"
let t_lte_method = t_method_binop "_lessequal"
let t_gt_method = t_method_binop "_greaterthan"
let t_gte_method = t_method_binop "_greaterequal"

(* Not sure why these are here. *)
let fold2 f base l1 l2 =
  if (List.length l1) <> (List.length l2) then
    failwith "Lists are not equal in length!"
  else
    List.fold_left2 f base l1 l2

let map2 f l1 l2 =
  if (List.length l1) <> (List.length l2) then
    failwith "Lists are not equal in length!"
  else
    List.map2 f l1 l2

let rec split = function
  | [] -> Pair.Pair([],[])
  | (Pair.Pair(l,r)) :: tl ->
    let Pair.Pair(tl_l,tl_r) = split tl in
    Pair.Pair(l :: tl_l, r :: tl_r)

let new_existential l = Type.TExistential(A.global_names "exists", l)

let import_to_string i = function
  | CompileStructs.CompileEnvironment.CompileEnvironment(_,mods) ->
    match (StringDict.find (Ast.name_key (AstUtils.import_to_dep i)) mods) with
    | CompileStructs.Provides.Provides(uri,_,_,_) -> uri
