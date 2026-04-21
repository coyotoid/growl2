# Growl

An experimental, statically typed, stack-based programming language.

## Building

Growl is written in OCaml.  As such you'll need to have a working OCaml toolchain, the `opam` package manager, and the `dune` build system.

You'll need to install the dependencies manually, like so:

```shell-session
$ opam install containers preface fmt hashcons menhir
```

After than, you can build by running `dune build` at the root of the project.

## Usage

At the moment the only implemented part of the pipeline is the type inference algorithm.
Running the Growl driver (`dune exec -- growl`) will read the contents of `test.grr` in the current directory and show the inferred types of the words defined in the file.

```shell-session
$ dune exec -- growl
The type of ifte is ('r, bool, ('r -> 's), ('r -> 's) -> 's)
The type of factorial is ('r, int -> 'r, int)
The type of even is ('r, int -> 'r, bool)
The type of odd is ('r, int -> 'r, bool)
```

