# holes

Homogeneous vectors with statically known size.

Their implementation minimized indirection at the value level, because their types have a tree-like structure.
This structure also makes their `traversable`, `foldable`, and `functor` instances really easy. This package also (almost) generalizes the concept of the `largeword` package oh hackage, by providing useful instances (e.g. `Num` and `Bits`) for certain types of `Holes`'. The caveat is that the base unit must be the same for all 'holes' in a `Holes n`, but that can be 8, so that's probably alright.

*NOTE*: I've found much better ways to solve the sorts of problems that this package addresses. See [bigword](http://github.com/nickspinale/bigword).
