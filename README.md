#Pyret OCaml Port
This repository is a port of the [Pyret][1] compiler to OCaml, primarily for my own personal learning's sake. Be warned: this entire repository is a construction zone!

## Roadmap
The gameplan for this project is as follows:

1. Do a direct port from the Pyret self-hosted compiler to OCaml (in progress; mostly done)
2. Refactor the port to better coincide with OCaml idioms
3. Add various abstractions, hopefully culminating in the ability to have multiple compiler targets

## Installation
To build the compiler, you must have OCaml (tested with v4.01.0) and OPAM installed.

If it is your first time building the compiler, you should run `make deps` first.

The `Makefile` has the following targets:

- `make` : Creates `main`, which takes a Pyret file and prints its Javascript compiled output to STDOUT
- `make run-tests` : Builds `run-tests`, which runs the compiler's test suite
- `make show-parse` : Build `show-parse`, a utility which takes a Pyret file and displays its parse tree
- `make clean` : Cleans up any object files made during the build process

## License
This repository is licensed under the GNU Public License, version 3.

[1]: https://github.com/brownplt/pyret-lang
