# To Do

## Boxed

* One might wish to make `create` a functor, so that each queue has its own
  abstract type `box`. Then it would be impossible to use a box with the wrong
  queue. `mem q box` would become synonymous with `busy box`. Some runtime
  checks might disappear (not many), and the embarrassing comments in `update`
  and `add_or_update` (about a mistake that is not detected at runtime) would
  disappear. However, performance might suffer, because the OCaml compiler
  might not inline queue operations into client code any more, due to the
  functor. It might be worth trying and measuring the performance impact.
