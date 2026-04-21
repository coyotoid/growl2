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

```shell-session
$ dune exec -- growl test.grr
The type of `ifte` is `(..r, bool, (..r -> ..s), (..r -> ..s) -> ..s)`
The type of `fact` is `(..r, int -> ..r, int)`
The type of `even` is `(..r, int -> ..r, bool)`
The type of `odd` is `(..r, int -> ..r, bool)`
The type of `main` is `(..r -> ..r, bool, bool)`
Resulting stack: [true false]
```

