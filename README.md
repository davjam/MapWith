# MapWith [![Stackage version](https://www.stackage.org/package/MapWith/badge/nightly?label=Stackage)](https://www.stackage.org/package/MapWith) [![Hackage version](https://img.shields.io/hackage/v/MapWith.svg?label=Hackage)](https://hackage.haskell.org/package/MapWith) [![build status](https://img.shields.io/travis/davjam/MapWith/master.svg?label=Travis%20build)](https://travis-ci.com/github/davjam/MapWith)

`mapWith`: like `fmap`, but can "inject" additional parameters such as whether first (or last) element, etc.

# Background

I often want to map over a list, but do something slightly different with the first or last element.

For a long time I used [markbounds](https://stackoverflow.com/questions/14114011/haskell-map-operation-with-different-first-and-last-functions#answer-53282575),
but also wanted something that:

- works on structures other than lists (`mapWith` works on all `Traversable` types);
- can provide additional types of parameter (not just first/last), such as:
  - index from start/end;
  - the previous/next element; and
- makes it easy to create new types of parameter to provide; and
- can provide any number of separate parameters to a function (not just a 3-tuple).

So, after only 2 years, I built a small library to do all of these.

# Examples

Passing a "standard combination" of isFirst and isLast parameters:

```
let g x f l = [star f, x, star l]; star b = if b then '*' else ' '
in withFirstLast g "fred"
["*f ", " r ", " e ", " d*"]
```

Passing a custom combination of different types of parameter 
(the index from the start, whether it's the last element, and elements from another list applied from the right):

```
let g x n l e = concat [[x], show n, if l then "*" else "-", e]
in mapWith (g ^-> eltIx & isLast <-^ eltFrom ["x","yy","z","zzzz","y"]) "fred"
["f0-zzzz","r1-z","e2-yy","d3*x"]
```

More examples are [here](doc/examples.hs).

# Questions/Doubts

Note that this is my first library and my first use of cabal, so I've probably done some dumb things.

Some things I wonder:

- Doesn't this already exist? (It feels like it should!)
- Should I name it `Data.Traversable.MapWith`? Or are such names "reserved" for "official" libraries, or something? Would this name impact my own file/directory structures?

# Future Work

Areas for potential improvement in later releases:

- Performance investigations and hopefully improvements, in particular:
  - fusion for eltFrom Injectors (unlikely, given the reasons it's not possible for zipWith, but we'll see).
  - enhancements for "stateful" "from the right" Injectors (unlikely, given [this](https://stackoverflow.com/questions/63504127/haskell-pinned-or-stack-memory-for-performance)).

- CurryTF: avoid tuples? (The tuple `(7, ())` is interpreted by `CurryTF` as an application of a single value `7`, but by `Data.Tuple.Curry` as two values: `7` and `()`,
  which I think is slightly more confusing than it needs to be.)

