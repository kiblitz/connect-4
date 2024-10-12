open! Core

module Result : sig
  type t =
    | Win of Player.t
    | Tie
  [@@deriving sexp_of]
end

type t =
  | To_move of Player.t
  | Game_over of Result.t
[@@deriving sexp_of]

val player_to_move_or_error : t -> Player.t Or_error.t
