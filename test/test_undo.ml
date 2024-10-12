open! Core
open! Import

let%expect_test "simple undo" =
  let t = Board.empty ~width:4 ~height:4 ~k:3 |> ok_exn in
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
  let t = Board.place_piece t ~column_idx:0 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+
    | | | | |
    | | | | |
    | | | | |
    |R| | | |
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
  let t = Board.undo t in
  print_s [%message (t : Board.t Or_error.t)];
  [%expect {| (t (Error "Cannot undo past the start of the game")) |}];
  ()
;;

let%expect_test "undo after game end" =
  let t = Board.empty ~width:3 ~height:3 ~k:2 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    | | | |
    | | | |
    | | | |
    +-+-+-+

    (To_move Red)
    |}];
  let t = Board.place_piece t ~column_idx:0 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    | | | |
    | | | |
    |R| | |
    +-+-+-+

    (To_move Blue) |}];
  let t = Board.place_piece t ~column_idx:1 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    | | | |
    | | | |
    |R|B| |
    +-+-+-+

    (To_move Red) |}];
  let t = Board.place_piece t ~column_idx:1 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    | | | |
    | |R| |
    |R|B| |
    +-+-+-+

    (Game_over (Win Red)) |}];
  let t = Board.undo t |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    | | | |
    | | | |
    |R|B| |
    +-+-+-+

    (To_move Red) |}];
  let t = Board.undo t |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    | | | |
    | | | |
    |R| | |
    +-+-+-+

    (To_move Blue) |}];
  let t = Board.undo t |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect{|
    +-+-+-+
    | | | |
    | | | |
    | | | |
    +-+-+-+

    (To_move Red) |}];
  let t = Board.undo t in
  print_s [%message (t : Board.t Or_error.t)];
  [%expect{| (t (Error "Cannot undo past the start of the game")) |}];
  ()
;;
