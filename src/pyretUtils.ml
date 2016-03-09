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
