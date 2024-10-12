open! Core

type t =
  | Blue
  | Red
[@@deriving equal, sexp_of]

val starting : t
val other : t -> t
val to_token_repr : t -> string
