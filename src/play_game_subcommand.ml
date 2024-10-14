open! Core
open! Async

module Args = struct
  open Command.Param

  module Defaults = struct
    let k = 4
    let width = 7
    let height = 6
  end

  let k =
    flag
      "-k"
      ~doc:[%string "INT tokens-in-a-row to win (default: %{Defaults.k#Int})"]
      (optional int)
  ;;

  let width =
    flag
      "-width"
      ~doc:[%string "INT width of board (default: %{Defaults.width#Int})"]
      (optional int)
  ;;

  let height =
    flag
      "-height"
      ~doc:[%string "INT height of board (default: %{Defaults.height#Int})"]
      (optional int)
  ;;

  let from_cli_params =
    let%map.Command k = k
    and width = width
    and height = height in
    let%map.Option () =
      Option.some_if (List.exists [ k; width; height ] ~f:Option.is_some) ()
    in
    let game_params =
      { Game_params.k = Option.value k ~default:Defaults.k
      ; width = Option.value width ~default:Defaults.width
      ; height = Option.value height ~default:Defaults.height
      }
    in
    Board.empty game_params
  ;;

  let from_game_file =
    let%map.Command game_file =
      flag
        "-from-game"
        ~doc:[%string "FILE game to play from"]
        (optional Filename_unix.arg_type)
    in
    let%map.Option record = Option.map game_file ~f:Record.load_exn in
    Board.from_history record
  ;;

  let board =
    choose_one
      [ from_game_file; from_cli_params ]
      ~if_nothing_chosen:
        (Default_to
           (Board.empty
              { Game_params.k = Defaults.k
              ; width = Defaults.width
              ; height = Defaults.height
              }))
  ;;
end

let name = "play"

module Custom_input = struct
  let help = "help"
  let undo = "undo"
  let redo = "redo"
  let save = "save"
  let quit = "quit"
end

let input_variant_text =
  [%string
    "(Number | '%{Custom_input.undo}' | '%{Custom_input.redo}' | '%{Custom_input.quit}' \
     | '%{Custom_input.save}' | '%{Custom_input.help}')"]
;;

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

let try_undo board =
  match Board.undo board with
  | Ok old_board ->
    print_string (Board.to_string_pretty old_board);
    old_board
  | Error err ->
    print_s [%message "Error trying to undo" (err : Error.t)];
    board
;;

let try_redo board =
  match Board.redo board with
  | Ok new_board ->
    print_string (Board.to_string_pretty new_board);
    new_board
  | Error err ->
    print_s [%message "Error trying to redo" (err : Error.t)];
    board
;;

let try_extract_save_filename response =
  match String.split response ~on:' ' with
  | [ save; filename ]
    when [%equal: String.t] save Custom_input.save && not (String.is_empty filename) ->
    Ok filename
  | (_ : string list) ->
    error_s [%message "Expected save format as 'save <filename>'" (response : string)]
;;

let main ~board () =
  let rec move board =
    print_s
      [%message
        "Please enter a column to place the next piece (see 'help' for more commands)"];
    match%bind Reader.read_line (force Reader.stdin) with
    | `Ok response when [%equal: String.t] response Custom_input.help ->
      print_s [%message [%string "Expecting one of %{input_variant_text}"]];
      move board
    | `Ok response when [%equal: String.t] response Custom_input.quit -> return ()
    | `Ok response when String.is_prefix response ~prefix:Custom_input.save ->
      let%bind () =
        match try_extract_save_filename response with
        | Ok filename ->
          let record = Board.record board in
          let%map () = Writer.save_sexp filename ([%sexp_of: Record.t] record) in
          print_s [%message "Successfully saved" (filename : string)]
        | Error err ->
          print_s [%message "Failed to save" (err : Error.t)];
          return ()
      in
      move board
    | `Ok response when [%equal: String.t] response Custom_input.undo ->
      let new_board = try_undo board in
      move new_board
    | `Ok response when [%equal: String.t] response Custom_input.redo ->
      let new_board = try_redo board in
      move new_board
    | response ->
      let new_board =
        match response_as_int response with
        | Some number -> try_place_piece board ~column_idx:number
        | None ->
          print_s
            [%message
              [%string "Expected one of %{input_variant_text}"]
                (response : string Reader.Read_result.t)];
          board
      in
      move new_board
  in
  let new_game_board = board in
  print_string (Board.to_string_pretty new_game_board);
  move new_game_board
;;

let command =
  Command.async
    ~summary:"Play a game"
    [%map_open.Command
      let board = Args.board in
      main ~board:(ok_exn board)]
;;
