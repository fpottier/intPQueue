# Changes

## 2025/09/24

* Initial implementation and release.

  The module `IntPQueue.Plain` has been extracted out of
  Menhir and has been slightly modified to rely on
  [Hector](https://github.com/fpottier/hector)
  instead of home-made vectors.

  The module `IntPQueue.Boxed` is new.
  It is a slightly more complex data structure
  which supports `remove` and `update`.

  Both modules have been tested using Monolith.
