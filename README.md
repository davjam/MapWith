# MapWith
mapWith: like fmap, but can "inject" additional arguments such as whether first (or last) element, etc.

# Background

I often want to map over a list, but do something slightly different with the first or last element.

For a long time I used [markbounds](https://stackoverflow.com/questions/14114011/haskell-map-operation-with-different-first-and-last-functions#answer-53282575),
but also wanted something that:

- works on structures other than lists (mapWith works on all Traversables)
- can provide additional types of attributes (not just first/last), such as:
  - index from start/end
  - the previous/next element
- makes it easy to create new types of attribute to provide.
- can provide any number of attributes as separate arguments to a function (not just a 3-tuple).

so, after only 2 years, I built a small library to do all of these.

# Examples

A trivial example:
```
mapWith ((,) <-^ isLim) "abc"
[('a',False),('b',False),('c',True)]
```

More examples are [here](doc/examples.hs)

# Questions/Doubts

Note that this is my first library and my first use of cabal, so I've probably done some dumb things.

Some things I wonder:

- Doesn't this already exist? (It feels like it should!)
- Is this useful enough to be a separate library?
- Should I name it Data.Traversable.MapWith? Or are such names "reserved" for "official" libraries, or something? Would this name impact my own file/directory structures?
- Is the code/documentation clear enough?
- Should I export the Injectable class and InjectedFn constructor?
  (I don't think it would be possible to create new instances anyway.)
  Not exporting them leaves an annoying warning when I build the documentation, and doesn't create a hyperlink for Injectable.
- Should I make up loads more Injectors, or leave that for anyone who uses the library?

# Performance

I've tried to make it perform well, including ensuring it "traverses" in each direction at most once, and only goes in both directions if it needs to.

Compared to the "benchmarks" I set myself, I think it's "OK". I'd like it to be better, but think I'd need help.
I've read about / tried things like fusing & unboxed tuples, but haven't yet found a way to employ them that improves performance.
There are some things I (naïvely) would have expected the optimising compiler to do, but it doesn't seem to.

I myself only use it on relatively small structures, and it's more than good enough for these. I'd be grateful for any suggestions.

