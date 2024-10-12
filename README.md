# Connect 4

A toy OCaml project implementing connect-4 with a CLI frontend

## Use
### CLI interface
```
Connect 4 CLI game

  main.exe SUBCOMMAND

=== subcommands ===

  play                       . Play a game
  version                    . print version information
  help                       . explain a given subcommand (perhaps recursively)
```

```
Play a game

  main.exe play 

=== flags ===

  [-height INT]              . height of board (default: 6)
  [-k INT]                   . tokens-in-a-row to win (default: 4)
  [-width INT]               . width of board (default: 7)
  [-help], -?                . print this help text and exit
```
### Demo
![v0.2.1-alpha demo](https://github.com/kiblitz/connect-4/releases/download/v.0.2.1-demo/connect-4-demo.gif)
## Release

This is a linux binary
