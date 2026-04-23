# Growl

An experimental, statically typed, stack-based programming language.

Growl is

## Building

Growl is written in OCaml.  As such you'll need to have a working OCaml 
toolchain, the `opam` package manager, and the `dune` build system.

You'll need to install the dependencies manually, like so:

```shell-session
$ opam install containers preface fmt hashcons menhir ppx_deriving
```

After than, you can build by running `dune build` at the root of the project.

## Usage

You can run a program using `dune exec -- growl exec file.grr` where `file.grr`
is the filename of the source file to execute.

There's also a convenience subcommand `check` that type-checks the whole program
and prints the types of all definitions.
