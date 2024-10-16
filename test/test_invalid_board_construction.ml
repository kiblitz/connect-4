open! Core
open! Import

let%expect_test "invalid board construction parameters" =
  let t = Board.empty { Game_params.k = 3; width = 0; height = 4 } in
  print_s [%message (t : Board.t Or_error.t)];
  [%expect
    {|
    (t
     (Error
      ("validation errors" (("" "value 0 <= bound 0") ("" "value 3 > bound 0"))))) |}];
  let t = Board.empty { Game_params.k = 3; width = 4; height = 0 } in
  print_s [%message (t : Board.t Or_error.t)];
  [%expect
    {|
    (t
     (Error
      ("validation errors" (("" "value 0 <= bound 0") ("" "value 3 > bound 0"))))) |}];
  let t = Board.empty { Game_params.k = 0; width = 4; height = 4 } in
  print_s [%message (t : Board.t Or_error.t)];
  [%expect {|
    (t (Error ("validation errors" (("" "value 0 <= bound 0"))))) |}];
  let t = Board.empty { Game_params.k = 5; width = 4; height = 4 } in
  print_s [%message (t : Board.t Or_error.t)];
  [%expect
    {|
    (t
     (Error
      ("validation errors" (("" "value 5 > bound 4") ("" "value 5 > bound 4"))))) |}];
  ()
;;
