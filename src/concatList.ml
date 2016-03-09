type 'a t =
    ConcatEmpty
  | ConcatSingleton of 'a
  | ConcatAppend of 'a t * 'a t
  | ConcatCons of 'a * 'a t
  | ConcatSnoc of 'a t * 'a

let rec to_list_acc (cl : 'a t) (rest : 'a list) : 'a list = match cl with
  | ConcatEmpty -> rest
  | ConcatSingleton(elt) -> elt :: rest
  | ConcatAppend(left,right) -> to_list_acc left (to_list_acc right rest)
  | ConcatCons(first,r) -> first :: (to_list_acc r rest)
  | ConcatSnoc(hd,tl) -> to_list_acc hd (tl :: rest)

let rec map_to_list_acc (cl : 'a t) (f : ('a -> 'b)) (rest : 'b list) : 'b list = match cl with
  | ConcatEmpty -> rest
  | ConcatSingleton(elt) -> (f elt) :: rest
  | ConcatAppend(left,right) -> map_to_list_acc left f (map_to_list_acc right f rest)
  | ConcatCons(first,r) -> (f first) :: (map_to_list_acc r f rest)
  | ConcatSnoc(hd,tl) -> map_to_list_acc hd f ((f tl) :: rest)

let rec map (f : ('a -> 'b)) (cl : 'a t) : 'b t = match cl with
  | ConcatEmpty -> ConcatEmpty
  | ConcatSingleton(elt) -> ConcatSingleton(f elt)
  | ConcatAppend(left,right) -> ConcatAppend(map f left, map f right)
  | ConcatCons(first,rest) -> ConcatCons(f first, map f rest)
  | ConcatSnoc(hd,tl) -> ConcatSnoc(map f hd, f tl)

let rec iter (f : ('a -> unit)) (cl : 'a t) : unit = match cl with
  | ConcatEmpty -> ()
  | ConcatSingleton(elt) -> f elt
  | ConcatAppend(left,right) ->
    iter f left;
    iter f right
  | ConcatCons(first,r) -> f first; iter f r
  | ConcatSnoc(hd,tl) -> iter f hd; f tl

let rec fold_left (f : ('b -> 'a -> 'b)) (base : 'b) (cl : 'a t) : 'b = match cl with
  | ConcatEmpty -> base
  | ConcatSingleton(elt) -> f base elt
  | ConcatAppend(left,right) -> fold_left f (fold_left f base left) right
  | ConcatCons(first,rest) -> fold_left f (f base first) rest
  | ConcatSnoc(hd,tl) -> f (fold_left f base hd) tl

let rec fold_right (f : ('a -> 'b -> 'b)) (cl : 'a t) (base : 'b) : 'b = match cl with
  | ConcatEmpty -> base
  | ConcatSingleton(elt) -> f elt base
  | ConcatAppend(left,right) -> fold_right f left (fold_right f right base)
  | ConcatCons(first,rest) -> f first (fold_right f rest base)
  | ConcatSnoc(hd,tl) -> fold_right f hd (f tl base)

let rec is_empty : 'a t -> bool = function
  | ConcatEmpty -> true
  | ConcatAppend(left,right) -> is_empty left && is_empty right
  | _ -> false

let rec get_first : 'a t -> 'a = function
  | ConcatEmpty -> failwith "Cannot get first of empty concat-list"
  | ConcatSingleton(e) -> e
  | ConcatAppend(left,right) -> if is_empty left then get_first right else get_first left
  | ConcatCons(first,_) -> first
  | ConcatSnoc(hd,tl) -> if is_empty hd then tl else get_first hd

let rec get_last = function
  | ConcatEmpty -> failwith "Cannot get last of empty concat-list"
  | ConcatSingleton(e) -> e
  | ConcatAppend(left,right) -> if is_empty right then get_last left else get_last right
  | ConcatCons(first,rest) -> if is_empty rest then first else get_last rest
  | ConcatSnoc(hd,tl) -> tl

let rec length = function
  | ConcatEmpty -> 0
  | ConcatSingleton(_) -> 1
  | ConcatAppend(left,right) -> (length left) + (length right)
  | ConcatCons(_,rest) -> 1 + (length rest)
  | ConcatSnoc(hd,_) -> 1 + (length hd)

let rec join_str (cl : 'a t) (tostring : ('a -> string)) (sep : string) =
  match cl with
  | ConcatEmpty -> ""
  | ConcatSingleton(e) -> tostring e
  | ConcatAppend(left,right) ->
    let left = (join_str left tostring sep)
    and right = (join_str right tostring sep) in
    if left = "" then right
    else if right = "" then left
    else left ^ sep ^ right
  | ConcatCons(first,rest) ->
    let first = (tostring first)
    and rest = (join_str rest tostring sep) in
    if rest = "" then first
    else first ^ sep ^ rest
  | ConcatSnoc(hd,tl) ->
    let hd = (join_str hd tostring sep)
    and tl = tostring tl in
    if hd = "" then tl
    else hd ^ sep ^ tl

let rec rev = function
  | ConcatEmpty -> ConcatEmpty
  | ConcatSingleton(e) -> ConcatSingleton(e)
  | ConcatAppend(left,right) -> ConcatAppend(rev right, rev left)
  | ConcatCons(first,rest) -> ConcatSnoc(rev rest, first)
  | ConcatSnoc(hd,tl) -> ConcatCons(tl, rev hd)

let rec revmap_to_list_acc (cl : 'a t) (f : ('a -> 'b)) (revhead : 'b list) =
  match cl with
  | ConcatEmpty -> revhead
  | ConcatSingleton(e) -> (f e) :: revhead
  | ConcatAppend(left,right) ->
    revmap_to_list_acc right f (revmap_to_list_acc left f revhead)
  | ConcatCons(first,rest) -> revmap_to_list_acc rest f ((f first) :: revhead)
  | ConcatSnoc(hd,tl) ->
    let newhead = revmap_to_list_acc hd f revhead in
    (f tl) :: newhead

let to_list (cl : 'a t) : 'a list = to_list_acc cl []
let map_to_list_left (cl : 'a t) (f : ('a -> 'b)) : 'b list = (List.rev (revmap_to_list_acc cl f []))
let map_to_list (cl : 'a t) (f : ('a -> 'b)) : 'b list = map_to_list_acc cl f []

(** Returns a catenable list made up of f(n, e1), f(n+1, e2) .. for e1, e2 ... in lst *)
let rec map_list_n (f : int -> 'a -> 'b) (n : int) (lst : 'a list) : 'b t =
  match lst with
  | [] -> ConcatEmpty
  | hd :: tl -> ConcatCons(f n hd, map_list_n f (n + 1) tl)

let rec map_list (f : 'a -> 'b) (lst : 'a list) : 'b t =
  match lst with
  | [] -> ConcatEmpty
  | hd :: tl -> ConcatCons(f hd, map_list f tl)

(** Returns a catenable list made up of f(elem1, elem2) for each elem1 in l1, elem2 ... in l2 *)
let rec map_list2 (f : 'a -> 'b -> 'c) (l1 : 'a list) (l2 : 'b list) : 'c t =
  match (l1,l2) with
  | ([],_)
  | (_,[]) -> ConcatEmpty
  | (hd1 :: tl1, hd2 :: tl2) -> ConcatCons(f hd1 hd2, map_list2 f tl1 tl2)
