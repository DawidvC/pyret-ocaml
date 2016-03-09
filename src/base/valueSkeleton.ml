type t =
    VSStr of string
  | VSCollection of string * t list
  | VSConstr of string * t list
  | VSSeq of t list

let rec render_list : string -> t list -> string = fun sep -> function
  | [] -> ""
  | hd :: tl ->
    List.fold_left (fun acc hd -> acc ^ sep ^ (render hd)) (render hd) tl

and render : t -> string = function
  | VSStr(s) -> s
  | VSCollection(name,items) ->
    "[" ^ name ^ ": " ^ (render_list ", " items) ^ "]"
  | VSConstr(name,args) ->
    name ^ "(" ^ (render_list ", " args) ^ ")"
  | VSSeq(items) -> render_list "" items

let of_value : 'a. ('a -> string) -> 'a -> t = fun conv v -> VSStr(conv v)

let list_of_values : 'a. ('a -> string) -> 'a list -> t list = fun conv vs ->
  let of_value = of_value conv in
  List.map of_value vs

let of_values : 'a. ('a -> string) -> 'a list -> t = fun conv vs ->
  VSSeq(list_of_values conv vs)
