open! Core

type t [@@deriving sexp]

val empty : t
val place_piece : t -> column_idx:int -> t

module Update_result : sig
  type nonrec t =
    { updated_history : t
    ; column_idx : int
    }
end

val undo : t -> Update_result.t Or_error.t
val next_move : t -> int Or_error.t
val undo_until_start : t -> t
