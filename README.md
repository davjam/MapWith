# MapWith [![Stackage version](https://www.stackage.org/package/MapWith/badge/nightly?label=Stackage)](https://www.stackage.org/package/MapWith) [![Hackage version](https://img.shields.io/hackage/v/MapWith.svg?label=Hackage)](https://hackage.haskell.org/package/MapWith) [![build status](https://img.shields.io/travis/davjam/MapWith/master.svg?label=Travis%20build)](https://travis-ci.com/github/davjam/MapWith)

`mapWith`: like `fmap`, but can "inject" additional parameters such as whether first (or last) element, etc.

# Background

I often want to map over a list, but do something slightly different with the first or last element.

For a long time I used [markbounds](https://stackoverflow.com/questions/14114011/haskell-map-operation-with-different-first-and-last-functions#answer-53282575),
but also wanted something that:

- works on structures other than lists (`mapWith` works on all `Traversable` types)
- can provide additional types of attributes (not just first/last), such as:
  - index from start/end
  - the previous/next element
- makes it easy to create new types of attribute to provide.
- can provide any number of attributes as separate parameters to a function (not just a 3-tuple).

so, after only 2 years, I built a small library to do all of these.

# Examples

A trivial example:
```
mapWith ((,) <-^ isLim) "abc"
[('a',False),('b',False),('c',True)]
```

More examples are [here](doc/examples.hs).

# Questions/Doubts

Note that this is my first library and my first use of cabal, so I've probably done some dumb things.

Some things I wonder:

- Doesn't this already exist? (It feels like it should!)
- Is this useful enough to be a separate library?
- Should I name it `Data.Traversable.MapWith`? Or are such names "reserved" for "official" libraries, or something? Would this name impact my own file/directory structures?
- Should I make up loads more Injectors, or leave that for anyone who uses the library?

# Future Work

Areas for potential improvement in later releases:

- Performance investigations and hopefully improvements, in particular:
  - fusion for eltFrom Injectors (unlikely, given the reasons it's not possible for zipWith, but we'll see).
  - enhancements for "stateful" "from the right" Injectors (unlikely, given [this](https://stackoverflow.com/questions/63504127/haskell-pinned-or-stack-memory-for-performance)).

- CurryTF: avoid tuples? (The tuple `(7, ())` is interpreted by `CurryTF` as an application of a single value `7`, but by `Data.Tuple.Curry` as two values: `7` and `()`,
  which I think is slightly more confusing than it needs to be.)

