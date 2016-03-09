let identity : 'a. 'a -> 'a = fun t -> t

(**
all2 returns false if any application of f returns false, or if the lengths differ.
his behavior is choosen to maintain the short-circuiting semantics. If one wants to
distinguish between lists of different lengths, and f returning false, use

match (map2-strict f l1 l2) with | Some(value) -> (List.for_all(identity value))
*)
let rec all2_strict : 'a 'b. ('a -> 'b -> bool) -> 'a list -> 'b list -> bool = fun f l1 l2 ->
  match (l1,l2) with
  | ([],[]) -> true
  | (a::ar,b::br) -> (f a b) && (all2_strict f ar br)
  | _ -> false

let rec map2_strict : 'a 'b 'r. ('a -> 'b -> 'r) -> 'a list -> 'b list -> ('r list) option =
  fun f l1 l2 ->
    match (l1,l2) with
    | ([],[]) -> Some([])
    | (a::ar,b::br) ->
      Option.map (fun r -> (f a b)::r) (map2_strict f ar br)
    | _ -> None

let rec fold2_strict : 'a 'b 'r. ('r -> 'a -> 'b -> 'r) -> 'r -> 'a list -> 'b list -> 'r option =
  fun f base l1 l2 ->
    match (l1,l2) with
    | ([],[]) -> Some(base)
    | (a::ar,b::br) -> fold2_strict f (f base a b) ar br
    | _ -> None
