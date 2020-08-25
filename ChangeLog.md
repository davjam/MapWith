# Revision history for MapWith

## 0.1.0.0 -- 2020-06-24
* First release

## 0.2.0.0 -- 2020-08-25
* Significant performance improvements (including fusion)
* New Features:
   * An Injector can inject multiple values (for example adj2Elts)
   * New Injectors:
      * evenElt
      * foldlElts and foldl1Elts
      * adj2Elts
   * New utility functions:
      * withFirst
      * withLast
* Breaking Changes:
   * eltFrom (& similar) now consume a List, not a Foldable. (They never used any features of Foldables, other than converting them to a list).
   * Injector functions have two changes. To convert Injectors, change `(\a s -> ... (i, s'))` to `(\a s -> ... (s', app1 i))`:
      * the order of the output pair is reversed for consistancy with state transformers, `mapAccumL`, etc. It's now `(new-state, injection-values)`.
      * the injector types and values now need to be instances of CurryTF.
* Improved documentation including examples and benchmark stats.
* Also tested in GHC 8.10.1
     
