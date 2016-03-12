module A = Ast
module VS = ValueSkeleton
module SL = Ast.Srcloc
module C = CompileStructs

open ListAux
open PyretUtils
open TypeStructs
open TypeCheckStructs

module SD = StringDict
module MSD = MutableStringDict

module TSet = Set.Make(struct
    type t = Type.t
    let compose f g x y = f (g x y)
    let compare = compose (function
        | Comparison.LessThan -> -1
        | Comparison.Equal -> 0
        | Comparison.GreaterThan -> 1) Type.compare
  end)

type name = A.name

module Pair = U.Pair
module Either = U.Either

type type_members = TypeMember.t list
let empty_type_members = []

type key_eliminator = Type.t -> Type.t SD.t -> TSet.t -> Type.t
type direction_info = (Type.t, key_eliminator) Pair.t
type substitutions = (Type.t, Type.t) Pair.t list

let create_substitutions do_substitute do_map blame_loc constraints unknowns r info =
  map_result (fun unknown ->
      (do_map (fun x -> Pair.Pair(unknown, x))
         (do_substitute blame_loc unknown r info constraints))) unknowns

let apply_substitutions maybe_subs typ =
  FoldResult.bind (fun substitutions ->
      FoldResult.FoldResult(List.fold_left (fun curr substitution ->
          Pair.apply Type.substitute substitution) typ substitutions)) maybe_subs

let rec determine_variance typ var_id info =
  match typ with
  | Type.TName(_,_,_) -> Variance.Constant
  | Type.TVar(id,_) -> if ((Ast.name_key id) = (Ast.name_key var_id)) then
      Variance.Covariant
    else
      Variance.Constant
  | Type.TArrow(args,ret,_) ->
    Variance.join (Variance.flip (List.fold_left (fun base arg ->
        (Variance.join base (determine_variance arg var_id info))) Variance.Constant args))
      (determine_variance ret var_id info)
  | Type.TTop(_)
  | Type.TBot(_) -> Variance.Constant
  | Type.TRecord(fields,_) ->
    let field_var = function
      | TypeMember.TMember(_,t,_) -> determine_variance t var_id info in
    List.fold_left (fun base tm -> Variance.join base (field_var tm)) Variance.Constant fields
  | Type.TForall(_,onto,_) ->
    (* TODO: Rename all introduces in onto to avoid conflict *)
    determine_variance onto var_id info
  | Type.TRef(_) -> Variance.Invariant
  | Type.TExistential(_,_) -> failwith "NYI (determine_variance) TExistential"
  | Type.TData(_,_,_,_) -> failwith "NYI (determine_variance) TData"
  | Type.TApp(onto,args,_) ->
    match (Context.get_data_type onto info) with
    | None -> failwith ("internal type-checking error. This shouldn't ever happen. "
                        ^ (ValueSkeleton.render (Type.to_vs typ)) ^ " isn't actually a "
                        ^ "data type! Avaliable data types are: "
                        ^ (mut_dict_to_string info.TCInfo.data_exprs))
    | Some(data_type) ->
      failwith "TODO"

let rec is_bottom_variable (x : Type.t) (binds : bindings) =
  let key = Type.key x in
  (SD.mem key binds) &&
  let bound = SD.find key binds in
  match bound with
  | Type.TBot(_) -> true
  | _ -> is_bottom_variable bound binds

let is_rigit = function
  | Variance.Invariant -> true
  | _ -> false

let is_rigid_under r binds =
  let fold_res () =
    List.fold_left (fun acc key -> acc && (match SD.find key binds with
        | Type.TBot(_) -> false
        | _ -> true)) true (List.map fst (SD.bindings binds)) in
  match r with
  | Type.TTop(_) -> true
  | Type.TBot(_) when fold_res() -> true
  | _ ->
    not (is_bottom_variable r binds) ||
    match r with
    | Type.TArrow(_,_,_) -> failwith "t-arrow not yet handled in is-rigid-under"
    | _ -> failwith "This shouldn't happen!"

module TypeConstraint = struct
  type t =
      Equality of Type.t
    | Bounds of Type.t * Type.t

  let max = function
    | Equality(t)
    | Bounds(_,t) -> t

  let min = function
    | Equality(s)
    | Bounds(s,_) -> s

  let is_rigid info = function
    | Equality(t) -> is_rigid_under t info.TCInfo.binds
    | Bounds(s,t) -> (s = t) && (is_rigid_under s info.TCInfo.binds)

  (* TODO: is_tight/meet *)

end
