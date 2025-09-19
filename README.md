# IntPQueue

`intPQueue` is an OCaml library that offers a fast and compact priority queue
whose keys are nonnegative integers. The user must use only *low* priorities,
because the space occupied by the priority queue is linear in the magnitude of
the priorities.

## Installation and Usage

Type `opam install intPQueue`.

In your `dune` file, add `(libraries intPQueue)` to the description of
your `library` or `executable`.

## Documentation

For more information,
please see the [documentation of the latest released
version](http://cambium.inria.fr/~fpottier/intPQueue/doc/intPQueue/).
