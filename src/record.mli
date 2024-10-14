open! Core

type t [@@deriving sexp]

val create : game_params:Game_params.t -> history:History.t -> t
val load_exn : string -> t
val game_params : t -> Game_params.t
val history : t -> History.t
