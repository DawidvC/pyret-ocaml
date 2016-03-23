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

(** Returns the first n items of the given list, throwing `Invalid_argument' if
n is greater than the length of the list. *)
let take : 'a. int -> 'a list -> 'a list = fun length ->
  assert (length >= 0);
  let rec take_help acc length = function
    | _ when length = 0 -> List.rev acc
    | [] ->
        raise (Invalid_argument("List is " ^ (string_of_int length) ^ " items too short."))
    | hd :: tl ->
      take_help (hd :: acc) (length - 1) tl in
  take_help [] length

(** Drops the last n items of the given list, throwing `Invalid_argument' if
n is greater than the length of the list. *)
let drop : 'a. int -> 'a list -> 'a list = fun length lst ->
  assert (length >= 0);
  take ((List.length lst) - length) lst

(** Returns an option type matching the first item in the list satisfying `pred', if found. *)
let list_find : 'a. ('a -> bool) -> 'a list -> 'a option = fun pred list ->
  try
    Some(List.find pred list)
  with
  | Not_found -> None

module PyretStringExt = struct
  include String
  let equal = (=)
  let hash = Hashtbl.hash
end
(** Mutable String Dictionary Module *)
module rec MutableStringDict : sig
  include Hashtbl.S with type key = string
  (** Adds each item in the given list to the given dictionary *)
  val add_each : 'a t -> (string * 'a) list -> unit

  (** Creates a new Mutable String Dictionary from the given list*)
  val of_list : (string * 'a) list -> 'a t

  val lookup : 'a t -> string -> 'a option

  val bindings : 'a t -> (string * 'a) list

  val freeze : 'a t -> 'a StringDict.t

  val merge : 'a t -> 'a t -> unit
end = struct
  include Hashtbl.Make(PyretStringExt)
  (** Adds each item in the given list to the given dictionary *)
  let add_each dict list =
    let add (key,value) = add dict key value in
    List.iter add list

  (** Creates a new Mutable String Dictionary from the given list*)
  let of_list list =
    let dict = create (List.length list) in
    add_each dict list;
    dict

  let lookup dict key =
    if mem dict key then
      Some(find dict key)
    else
      None

  let bindings dict =
    fold (fun key value acc -> (key,value) :: acc) dict []

  let freeze dict =
    StringDict.add_each StringDict.empty (bindings dict)

  let merge dict other =
    add_each dict @@ bindings other
end

(** Immutable String Dictionary Module *)
and StringDict : sig
  include Map.S with type key = string
  val add_each : 'a t -> (string * 'a) list -> 'a t
  val of_list : (string * 'a) list -> 'a t
  val lookup : string -> 'a t -> 'a option
  val unfreeze : 'a t -> 'a MutableStringDict.t
end = struct
  include Map.Make(String)

  let add_each dict list =
    let add acc (key,value) = add key value acc in
    List.fold_left add dict list

  let of_list list =
    add_each empty list

  let lookup key dict =
    if mem key dict then
      Some(find key dict)
    else
      None

  let unfreeze dict =
    let bindings = bindings dict in
    let ret = MutableStringDict.create (List.length bindings) in
    MutableStringDict.add_each ret bindings;
    ret
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

  let apply : 'a 'b. ('a -> 'b -> 'c) -> ('a,'b) t -> 'c = fun f ->
    function
    | Pair(l,r) -> f l r
end

module Either = struct
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b

  let map_left : 'a 'b 'c. ('a -> 'c) -> ('a,'b) t -> ('c,'b) t = fun f ->
    function
    | Left(v) -> Left(f v)
    | Right(v) -> Right(v)

  let map_right : 'a 'b 'c. ('b -> 'c) -> ('a,'b) t -> ('a,'c) t = fun f ->
    function
    | Right(v) -> Right(f v)
    | Left(v) -> Left(v)

  let rec all_left : 'a 'b. ('a,'b) t list -> 'a list = function
    | [] -> []
    | (Right(_)) :: _ -> raise (Invalid_argument("Right item found"))
    | (Left(hd)) :: tl -> hd :: (all_left tl)

  let rec all_right : 'a 'b. ('a,'b) t list -> 'b list = function
    | [] -> []
    | (Left(_)) :: _ -> raise (Invalid_argument("Left item found"))
    | (Right(hd)) :: tl -> hd :: (all_right tl)
end

(** Takes a function that takes two arguments and returns an Either, and also a base value,
and folds over the given list from the left as long as the function returns an Either.Left() value,
and returns either the final value or the Either.Right() value.*)
let rec fold_while : 'a 'b. ('a -> 'b -> ('a,'a) Either.t) -> 'a -> 'b list -> 'a = fun f base lst ->
  match lst with
  | [] -> base
  | hd :: tl ->
    match (f base hd) with
    | Either.Left(v) -> fold_while f v tl
    | Either.Right(v) -> v

(* Function composition operator *)
let (||>) : 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c) =
  fun f g x -> f @@ g x
