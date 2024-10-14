open! Core

type t =
  { k : int
  ; width : int
  ; height : int
  }
[@@deriving fields ~getters, sexp]
