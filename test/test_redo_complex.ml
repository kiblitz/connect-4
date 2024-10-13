open! Core
open! Import

let%expect_test "no history clear when playing moves that align with future" =
  let t = Board.empty { Game_params.k = 3; width = 4; height = 4 } |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+
    | | | | |
    | | | | |
    | | | | |
    | | | | |
    +-+-+-+-+

    (To_move Red)
    |}];
  let t = Board.place_piece t ~column_idx:1 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+
    | | | | |
    | | | | |
    | | | | |
    | |R| | |
    +-+-+-+-+

    (To_move Blue) |}];
  let t = Board.place_piece t ~column_idx:2 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+
    | | | | |
    | | | | |
    | | | | |
    | |R|B| |
    +-+-+-+-+

    (To_move Red) |}];
  let t = Board.place_piece t ~column_idx:2 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+
    | | | | |
    | | | | |
    | | |R| |
    | |R|B| |
    +-+-+-+-+

    (To_move Blue) |}];
  let t = Board.undo t |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+
    | | | | |
    | | | | |
    | | | | |
    | |R|B| |
    +-+-+-+-+

    (To_move Red) |}];
  let t = Board.undo t |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+
    | | | | |
    | | | | |
    | | | | |
    | |R| | |
    +-+-+-+-+

    (To_move Blue) |}];
  let t = Board.undo t |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+
    | | | | |
    | | | | |
    | | | | |
    | | | | |
    +-+-+-+-+

    (To_move Red) |}];
  let t = Board.place_piece t ~column_idx:1 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+
    | | | | |
    | | | | |
    | | | | |
    | |R| | |
    +-+-+-+-+

    (To_move Blue) |}];
  let t = Board.redo t in
  print_s [%message (t : Board.t Or_error.t)];
  [%expect
    {|
    (t
     (Ok
      ((board ((0 ()) (1 ((0 Red))) (2 ((0 Blue))) (3 ())))
       (game_state (To_move Red)) (played_tokens 2)
       (history ((past (2 1)) (future (2)))) (width 4) (height 4) (k 3)))) |}];
  ()
;;
