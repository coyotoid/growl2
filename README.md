# Growl

An experimental, statically typed, stack-based programming language.

## Building

Growl is written in OCaml.  As such you'll need to have a working OCaml 
toolchain, the `opam` package manager, and the `dune` build system.

In the root of the project, run the following command to install the
dependencies required:

```shell-session
$ opam install --deps-only .
```

After than, you can build by running `dune build` at the root of the project.

## Usage

You can run a program using `dune exec -- growl exec file.grr` where `file.grr`
is the filename of the source file to execute.

There's also a convenience subcommand `check` that type-checks the whole program
and prints the types of all definitions.
