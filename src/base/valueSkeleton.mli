(**
   ValueSeleton type. Like the Pyret version, except
   there is no equivalent to `vs-value'. *)
type t =
    VSStr of string
  | VSCollection of string * t list
  | VSConstr of string * t list
  | VSSeq of t list

(** Returns the string representation of the given ValueSkeleton. *)
val render : t -> string

(** Creates a converter from values to ValueSkeleton types. *)
val of_value : ('a -> string) -> 'a -> t

(** Creates a converter from value lists to a list of ValueSkeleton types. *)
val list_of_values : ('a -> string) -> 'a list -> t list

(** Creates a converter from value lists to ValueSkeleton types. *)
val of_values : ('a -> string) -> 'a list -> t

(** Creates a ValueSkeleton list of values using the given items and converter.
If ~label is true (the default), then the output will be formatted as a Pyret-style
    list (i.e. `[list: elts ...]') *)
val of_list : ('a -> t) -> 'a list -> ?label:bool -> unit -> t
