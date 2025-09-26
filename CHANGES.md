# Changes

## 2025/MM/DD

* In `Plain` and `Boxed`, new function `iter`.

## 2025/09/25

* Incompatible API changes:

  + The module `IntPQueue` is renamed to `IntPQueue.Plain`.

  + The module `IntPQueue.Boxed` is new.
    It is a slightly more complex data structure
    which supports `remove`, `update`, and `add_or_update`.

  Both modules have been tested using Monolith.

## 2025/09/19

* Initial implementation and release.

  The code has been extracted out of
  Menhir and has been slightly modified to rely on
  [Hector](https://github.com/fpottier/hector)
  instead of home-made vectors.
