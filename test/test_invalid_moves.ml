open! Core
open! Import

let%expect_test "row out of bounds" =
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
  let t_err = Board.place_piece t ~column_idx:(-1) in
  print_s [%message (t_err : Board.t Or_error.t)];
  [%expect {|
    (t_err (Error ("validation errors" (("" "value -1 < bound 0"))))) |}];
  let t_err = Board.place_piece t ~column_idx:4 in
  print_s [%message (t_err : Board.t Or_error.t)];
  [%expect {|
    (t_err (Error ("validation errors" (("" "value 4 >= bound 4"))))) |}];
  ()
;;

let%expect_test "column overflow" =
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
  let t = Board.place_piece t ~column_idx:0 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+
    | | | | |
    | | | | |
    |B| | | |
    |R| | | |
    +-+-+-+-+

    (To_move Red) |}];
  let t = Board.place_piece t ~column_idx:0 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+
    | | | | |
    |R| | | |
    |B| | | |
    |R| | | |
    +-+-+-+-+

    (To_move Blue) |}];
  let t = Board.place_piece t ~column_idx:0 |> ok_exn in
  print_string (Board.to_string_pretty t);
  [%expect
    {|
    +-+-+-+-+
    |B| | | |
    |R| | | |
    |B| | | |
    |R| | | |
    +-+-+-+-+

    (To_move Red) |}];
  let t = Board.place_piece t ~column_idx:0 in
  print_s [%message (t : Board.t Or_error.t)];
  [%expect {|
    (t (Error ("validation errors" (("" "value 4 >= bound 4"))))) |}];
  ()
;;
