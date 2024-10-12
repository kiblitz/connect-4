open! Core
open! Import

let%expect_test "tiny tie" =
  let t = Board.empty ~width:3 ~height:3 ~k:3 |> ok_exn in
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
  let t = Board.place_piece t ~column_idx:1 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    | | | |
    | | | |
    | |R| |
    +-+-+-+

    (To_move Blue) |}];
  let t = Board.place_piece t ~column_idx:0 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    | | | |
    | | | |
    |B|R| |
    +-+-+-+

    (To_move Red) |}];
  let t = Board.place_piece t ~column_idx:0 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    | | | |
    |R| | |
    |B|R| |
    +-+-+-+

    (To_move Blue) |}];
  let t = Board.place_piece t ~column_idx:2 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    | | | |
    |R| | |
    |B|R|B|
    +-+-+-+

    (To_move Red) |}];
  let t = Board.place_piece t ~column_idx:2 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    | | | |
    |R| |R|
    |B|R|B|
    +-+-+-+

    (To_move Blue) |}];
  let t = Board.place_piece t ~column_idx:1 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    | | | |
    |R|B|R|
    |B|R|B|
    +-+-+-+

    (To_move Red) |}];
  let t = Board.place_piece t ~column_idx:0 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    |R| | |
    |R|B|R|
    |B|R|B|
    +-+-+-+

    (To_move Blue) |}];
  let t = Board.place_piece t ~column_idx:1 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    |R|B| |
    |R|B|R|
    |B|R|B|
    +-+-+-+

    (To_move Red) |}];
  let t = Board.place_piece t ~column_idx:2 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+
    |R|B|R|
    |R|B|R|
    |B|R|B|
    +-+-+-+

    (Game_over Tie) |}];
  let t = Board.place_piece t ~column_idx:3 in
  print_s [%message (t : Board.t Or_error.t)];
  [%expect {| (t (Error ("Game is over with result" (result Tie)))) |}];
  ()
;;
