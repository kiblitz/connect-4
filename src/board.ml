open! Core

module Column : sig
  type t [@@deriving sexp_of]

  val empty : t
  val filled_slots : t -> int
  val add_token : t -> player:Player.t -> t
  val nth : t -> n:int -> Player.t option
end = struct
  type t = Player.t Int.Map.t [@@deriving sexp_of]

  let empty = Int.Map.empty
  let filled_slots t = Map.length t
  let add_token t ~player = Map.add_exn t ~key:(filled_slots t) ~data:player
  let nth t ~n = Map.find t n
end

type t =
  { board : Column.t Int.Map.t
  ; game_state : Game_state.t
  ; played_tokens : int
  ; width : int
  ; height : int
  ; k : int
  }
[@@deriving fields ~getters, sexp_of]

let empty ~k ~width ~height =
  let%bind.Or_error () =
    let validate_width = Int.validate_positive width in
    let validate_height = Int.validate_positive height in
    let validate_k =
      let positive = Int.validate_positive k in
      let at_most_width = Int.validate_ubound k ~max:(Incl width) in
      let at_most_heignt = Int.validate_ubound k ~max:(Incl height) in
      Validate.of_list [ positive; at_most_width; at_most_heignt ]
    in
    Validate.of_list [ validate_width; validate_height; validate_k ] |> Validate.result
  in
  let%map.Or_error board =
    width
    |> List.init ~f:(fun column_idx -> column_idx, Column.empty)
    |> Int.Map.of_alist_or_error
  in
  { board; game_state = To_move Player.starting; played_tokens = 0; width; height; k }
;;

let get_token t ~column_idx ~row_idx =
  let%bind.Option column = Map.find (board t) column_idx in
  Column.nth column ~n:row_idx
;;

let update_game_state t ~latest_x ~latest_y =
  match t.game_state with
  | Game_over (_ : Game_state.Result.t) -> t
  | To_move latest_player ->
    let player_win =
      let winner_along_axis ~dx ~dy =
        let coords =
          let neg_k_to_k = List.range ~start:`exclusive ~stop:`exclusive (-t.k) t.k in
          List.map neg_k_to_k ~f:(fun i -> (dx * i) + latest_x, (dy * i) + latest_y)
        in
        let tokens_along_axis =
          List.map coords ~f:(fun (x, y) -> get_token t ~column_idx:x ~row_idx:y)
        in
        tokens_along_axis
        |> List.group ~break:(fun t1 t2 -> not ([%equal: Player.t option] t1 t2))
        |> List.filter_map ~f:(function
          | Some player :: _ as l when [%equal: Player.t] player latest_player ->
            Some (List.length l)
          | _ -> None)
        |> List.exists ~f:(fun tokens_in_a_row -> tokens_in_a_row >= t.k)
      in
      let axis_wins =
        let%map.List dx, dy = [ 1, 0; 0, 1; 1, -1; 1, 1 ] in
        winner_along_axis ~dx ~dy
      in
      List.exists axis_wins ~f:Fn.id
    in
    let no_more_moves = t.played_tokens = t.width * t.height in
    let game_state =
      if player_win
      then Game_state.Game_over (Win latest_player)
      else if no_more_moves
      then Game_state.Game_over Tie
      else To_move (Player.other latest_player)
    in
    { t with game_state }
;;

let place_piece t ~column_idx =
  let%bind.Or_error player_to_move = Game_state.player_to_move_or_error t.game_state in
  let%bind.Or_error () =
    let validate_column =
      Int.validate_bound column_idx ~min:(Incl 0) ~max:(Excl t.width)
    in
    Validate.result validate_column
  in
  let%bind.Or_error old_column = Map.find_or_error t.board column_idx in
  let%map.Or_error () =
    let validate_row =
      Int.validate_ubound (Column.filled_slots old_column) ~max:(Excl t.height)
    in
    Validate.result validate_row
  in
  let row_idx = Column.filled_slots old_column in
  let new_column = Column.add_token old_column ~player:player_to_move in
  { t with
    board = Map.set t.board ~key:column_idx ~data:new_column
  ; played_tokens = t.played_tokens + 1
  }
  |> update_game_state ~latest_x:column_idx ~latest_y:row_idx
;;

let to_string_pretty t =
  let add_column_separator_to_front s = List.map s ~f:(List.cons "|") in
  let add_column s ~column_idx =
    List.mapi s ~f:(fun row_idx ->
      List.cons
        (t
         |> get_token ~column_idx ~row_idx
         |> Option.value_map ~f:Player.to_token_repr ~default:" "))
  in
  let rec process_cols s ~column_idx =
    let s = add_column_separator_to_front s in
    if column_idx < 0
    then s
    else add_column s ~column_idx |> process_cols ~column_idx:(column_idx - 1)
  in
  let guard_rail =
    let length = (t.width * 2) + 1 in
    String.init length ~f:(fun idx -> if idx mod 2 = 0 then '+' else '-')
  in
  List.init t.height ~f:(Fn.const [])
  |> process_cols ~column_idx:(t.width - 1)
  |> List.map ~f:(String.concat ~sep:"")
  |> (fun board ->
       [ guard_rail ]
       @ List.rev board
       @ [ guard_rail; ""; Sexp.to_string_hum [%sexp (t.game_state : Game_state.t)] ])
  |> String.concat_lines
;;

let%expect_test "empty repr" =
  let t = empty ~width:7 ~height:6 ~k:4 |> ok_exn in
  print_string (to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+-+-+-+
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    +-+-+-+-+-+-+-+

    (To_move Red) |}]
;;
