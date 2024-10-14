open! Core
open! Import

let%expect_test "simple replay" =
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
  let record = Board.record t in
  let t = Board.from_history record |> ok_exn in
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
  let t = Board.redo t |> ok_exn in
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
  let t = Board.redo t |> ok_exn in
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
  let t = Board.redo t |> ok_exn in
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
  let t = Board.redo t in
  print_s [%message (t : Board.t Or_error.t)];
  [%expect {| (t (Error "No next move in history")) |}];
  ()
;;
