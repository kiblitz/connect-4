open! Core

let commands : (module Subcommand_intf.S) list = [ (module Test_subcommand) ]

let command =
  Command.group ~summary:"Connect 4 CLI game"
    ~preserve_subcommand_order:()
    (List.map commands ~f:(fun (module S) -> (S.name, S.command)))