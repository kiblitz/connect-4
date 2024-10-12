open! Core

let name = "test"
let main () = print_s [%message "this is a test" (3 : int)]

let command =
  Command.basic ~summary:"test subcommand" (Command.Spec.return main)
