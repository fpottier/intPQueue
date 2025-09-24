# IntPQueue

This OCaml library offers a fast and compact priority queue
whose keys are nonnegative integers.

The priorities must be *low* integers,
because the space occupied by the priority queue is *O(n+p)*,
where *n* is the number of elements in the queue
and *p* is the greatest priority that is ever used.

Furthermore, this priority queue is most efficient
under the assumption
that the priorities that are passed to `add` and `update`
are at least as high
as the priority of the last element
that was returned by `extract`.
This is the case, for example,
in Dijkstra's single-source shortest paths algorithm.
In this scenario,
the time complexity of inserting and extracting *n* elements
is *O(n+p)*.
In Dijkstra's algorithm,
for example,
if the cost of every edge in the graph is 1
then *p* is *O(n)*
so the time complexity of inserting and extracting *n* elements
is *O(n)*.
In other words, every priority queue operation
has amortized time complexity *O(1)*.

The library offers two variants of the priority queue:
`IntPQueue.Plain` is simpler, faster, and more compact;
`IntPQueue.Boxed` is slower (by a constant factor)
but supports more operations, namely `remove` and `update`.

## Installation and Usage

Type `opam install intPQueue`.

In your `dune` file, add `(libraries intPQueue)` to the description of
your `library` or `executable`.

## Documentation

For more information,
please see the [documentation of the latest released
version](http://cambium.inria.fr/~fpottier/intPQueue/doc/intPQueue/).
