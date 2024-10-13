open! Core

type t =
  { k : int
  ; width : int
  ; height : int
  }
[@@deriving sexp]
