open! Core

type t =
  | Blue
  | Red
[@@deriving equal, sexp_of]

let starting = Red

let other = function
  | Blue -> Red
  | Red -> Blue
;;

let to_token_repr = function
  | Blue -> "B"
  | Red -> "R"
;;
