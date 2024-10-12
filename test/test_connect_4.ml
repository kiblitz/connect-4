open! Core
open! Import

let%expect_test "simple win" =
  let t = Board.empty ~width:7 ~height:6 ~k:4 |> ok_exn in
  print_string (Board.to_string_pretty t);
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

    (To_move Red)
    |}];
  let t = Board.place_piece t ~column_idx:3 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+-+-+-+
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | |R| | | |
    +-+-+-+-+-+-+-+

    (To_move Blue) |}];
  let t = Board.place_piece t ~column_idx:4 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect{|
    +-+-+-+-+-+-+-+
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | |R|B| | |
    +-+-+-+-+-+-+-+

    (To_move Red) |}];
  let t = Board.place_piece t ~column_idx:3 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect{|
    +-+-+-+-+-+-+-+
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | |R| | | |
    | | | |R|B| | |
    +-+-+-+-+-+-+-+

    (To_move Blue) |}];
  let t = Board.place_piece t ~column_idx:4 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect{|
    +-+-+-+-+-+-+-+
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | |R|B| | |
    | | | |R|B| | |
    +-+-+-+-+-+-+-+

    (To_move Red) |}];
  let t = Board.place_piece t ~column_idx:3 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect{|
    +-+-+-+-+-+-+-+
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | |R| | | |
    | | | |R|B| | |
    | | | |R|B| | |
    +-+-+-+-+-+-+-+

    (To_move Blue) |}];
  let t = Board.place_piece t ~column_idx:4 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect{|
    +-+-+-+-+-+-+-+
    | | | | | | | |
    | | | | | | | |
    | | | | | | | |
    | | | |R|B| | |
    | | | |R|B| | |
    | | | |R|B| | |
    +-+-+-+-+-+-+-+

    (To_move Red) |}];
  let t = Board.place_piece t ~column_idx:3 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect{|
    +-+-+-+-+-+-+-+
    | | | | | | | |
    | | | | | | | |
    | | | |R| | | |
    | | | |R|B| | |
    | | | |R|B| | |
    | | | |R|B| | |
    +-+-+-+-+-+-+-+

    (Game_over (Win Red)) |}];
  ()
;;
