open! Core
open! Async

module Args = struct
  open Command.Param

  let width =
    let default = 7 in
    flag
      "-width"
      ~doc:[%string "INT width of board (default: %{default#Int})"]
      (optional_with_default default int)
  ;;

  let height =
    let default = 6 in
    flag
      "-height"
      ~doc:[%string "INT height of board (default: %{default#Int})"]
      (optional_with_default default int)
  ;;

  let k =
    let default = 4 in
    flag
      "-k"
      ~doc:[%string "INT tokens-in-a-row to win (default: %{default#Int})"]
      (optional_with_default default int)
  ;;
end

let name = "play"

module Custom_input = struct
  let quit = "quit"
  let undo = "undo"
end

let response_as_int response =
  match response with
  | `Ok ok_response -> Int.of_string_opt ok_response
  | `Eof -> None
;;

let try_place_piece board ~column_idx =
  match Board.place_piece board ~column_idx with
  | Ok board ->
    print_string (Board.to_string_pretty board);
    board
  | Error err ->
    print_s [%message "Invalid move" (err : Error.t)];
    board
;;

let handle_game_over board =
  match (Board.game_state board : Game_state.t) with
  | To_move (_ : Player.t) -> Some board
  | Game_over (_ : Game_state.Result.t) -> None
;;

let main game_params () =
  let rec move board =
    print_s [%message "Please enter a column to place the next piece"];
    match%bind Reader.read_line (force Reader.stdin) with
    | `Ok response when [%equal: String.t] response Custom_input.quit -> return ()
    | `Ok response when [%equal: String.t] response Custom_input.undo ->
      let new_board =
        match Board.undo board with
        | Ok old_board ->
          print_string (Board.to_string_pretty old_board);
          old_board
        | Error err ->
          print_s [%message "Error trying to undo" (err : Error.t)];
          board
      in
      move new_board
    | response ->
      let new_board =
        match response_as_int response with
        | Some number -> try_place_piece board ~column_idx:number
        | None ->
          print_s
            [%message
              [%string
                "Expected one of (Number | '%{Custom_input.undo}' | \
                 '%{Custom_input.quit}')"]
                (response : string Reader.Read_result.t)];
          board
      in
      (match handle_game_over new_board with
       | Some new_board_after_handled_for_game_over ->
         move new_board_after_handled_for_game_over
       | None -> return ())
  in
  let new_game_board = Board.empty game_params |> ok_exn in
  print_string (Board.to_string_pretty new_game_board);
  move new_game_board
;;

let command =
  Command.async
    ~summary:"Play a game"
    [%map_open.Command
      let width = Args.width
      and height = Args.height
      and k = Args.k in
      main { Game_params.k; width; height }]
;;
