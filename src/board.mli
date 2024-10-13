open! Core

type t [@@deriving sexp_of]

val empty : k:int -> width:int -> height:int -> t Or_error.t
val place_piece : t -> column_idx:int -> t Or_error.t
val undo : t -> t Or_error.t
val redo : t -> t Or_error.t
val game_state : t -> Game_state.t
val to_string_pretty : t -> string
