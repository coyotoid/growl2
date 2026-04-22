# Growl

An experimental, statically typed, stack-based programming language.

## Building

Growl is written in OCaml.  As such you'll need to have a working OCaml toolchain, the `opam` package manager, and the `dune` build system.

You'll need to install the dependencies manually, like so:

```shell-session
$ opam install containers preface fmt hashcons menhir ppx_deriving
```

After than, you can build by running `dune build` at the root of the project.

## Usage

```shell-session
$ dune exec -- growl check test.grr
ifte :: `(..r, bool, (..r -> ..s), (..r -> ..s) -> ..s)`
fact :: `(..r, int -> ..r, int)`
even :: `(..r, int -> ..r, bool)`
odd :: `(..r, int -> ..r, bool)`
main :: `(..r -> ..r, bool, bool, int)`
$ dune exec -- growl exec test.grr
Resulting stack: [true false 3628800]
```

