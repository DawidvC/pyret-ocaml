(** Utility functions *)

(** Joins the given list of strings with the given separator. *)
let join_str (lst : string list) (sep : string) =
  match lst with
  | [] -> ""
  | hd :: tl ->
    let rec join_str_acc lst acc =
      match lst with
      | [] -> acc
      | hd :: tl -> join_str_acc tl (acc ^ sep ^ hd) in
    join_str_acc tl hd

(** Returns the last item of the given list *)
let last : 'a. 'a list -> 'a = function
  | [] -> failwith "Cannot take the last of an empty list"
  | (hd :: tl) -> (List.fold_left (fun _ x -> x) hd tl)

(** Returns an option type matching the first item in the list satisfying `pred', if found. *)
let list_find : 'a. ('a -> bool) -> 'a list -> 'a option = fun pred list ->
  try
    Some(List.find pred list)
  with
  | Not_found -> None

(** Mutable String Dictionary Module *)
module MutableStringDict = struct
  include Hashtbl.Make(struct
      type t = string
      let equal = (=)
      let hash = Hashtbl.hash
    end)
  (** Adds each item in the given list to the given dictionary *)
  let add_each : 'a. 'a t -> (string * 'a) list -> unit = fun dict list ->
    let add (key,value) = add dict key value in
    List.iter add list

  (** Creates a new Mutable String Dictionary from the given list*)
  let of_list : 'a. (string * 'a) list -> 'a t = fun list ->
    let dict = create (List.length list) in
    add_each dict list;
    dict

  let lookup : 'a. 'a t -> string -> 'a option = fun dict key ->
    if mem dict key then
      Some(find dict key)
    else
      None
end

(** Immutable String Dictionary Module *)
module StringDict = struct
  include Map.Make(String)

  let add_each : 'a. 'a t -> (string * 'a) list -> 'a t = fun dict list ->
    let add acc (key,value) = add key value acc in
    List.fold_left add dict list

  let of_list : 'a. (string * 'a) list -> 'a t = fun list ->
    add_each empty list

  let lookup : 'a. string -> 'a t -> 'a option = fun key dict ->
    if mem key dict then
      Some(find key dict)
    else
      None
end

(** Pair of data *)
module Pair = struct
  type ('a,'b) t = Pair of 'a * 'b

  let left = function
    | Pair(l,_) -> l

  let right = function
    | Pair(_,r) -> r

  let on_left : 'a 'b. ('a -> 'a) -> ('a,'b) t -> ('a,'b) t = fun f ->
    function
    | Pair(l,r) -> Pair(f l, r)

  let on_right : 'a 'b. ('b -> 'b) -> ('a,'b) t -> ('a,'b) t = fun f ->
    function
    | Pair(l,r) -> Pair(l, f r)
end
