open! Core

module Result = struct
  type t =
    | Win of Player.t
    | Tie
  [@@deriving sexp_of]
end

type t =
  | To_move of Player.t
  | Game_over of Result.t
[@@deriving sexp_of]

let player_to_move_or_error = function
  | To_move player -> Ok player
  | Game_over result -> error_s [%message "Game is over with result" (result : Result.t)]
;;
