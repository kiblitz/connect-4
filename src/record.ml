open! Core

type t =
  { game_params : Game_params.t
  ; history : History.t
  }
[@@deriving fields ~getters, sexp]

let create ~game_params ~history =
  { game_params; history = History.undo_until_start history }
;;

let load_exn filename = Sexp.load_sexp filename |> t_of_sexp
