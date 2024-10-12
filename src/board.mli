open! Core

type t [@@deriving sexp_of]

val empty : width:int -> height:int -> k:int -> t Or_error.t
val place_piece : t -> column_idx:int -> t Or_error.t
val to_string_pretty : t -> string
