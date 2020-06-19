{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}

{-|
Module      : MapWith  
Description : blah
Copyright   : (c) David James, 2020
License     : BSD3
Stability   : Experimental

Provides fmap-like functionality, but can also "inject" additional parameters to the mapping function, such as:

* whether the first / last element
* the previous / next element
* the index from the start / end

-}

module MapWith
  (
  -- * Type Names
  -- $TypeNames
  
  -- * Pre-Packaged Maps
  -- $PrePackagedMaps
    withFirstLast
  , andFirstLast
  , withPrevNext
  , andPrevNext


  -- * Custom Maps
  -- $CustomMaps
  , mapWith
  , ($->)
  , (<-$)
  , (*->)
  , (<-*)
  
  -- * Predefined Injectors
  , limIt
  , adjElt
  , ixIt
  , zipIt

  -- * Custom Injectors
  , Injector(..)
  )
where

import Data.Foldable (toList)
import Data.Traversable (mapAccumL, mapAccumR)

{-$TypeNames
These 'names' are used for types and variables throughout:

[@t@]: the Traversable we're mapping over
[@a@]: the value in the input Traversable
[@b@]: the result in the output Traversable
[@i@]: an output from an Injector, injected into the mapper
[@l@]: an i that's determined "from the left"
[@r@]: an i that's determined "from the right"
[@s@]: the internal state in an Injector
-}

--XXXX I'd like to add separate comments for each argument, but that's not supported to GHC 5.6 https://github.com/haskell/haddock/issues/836#issuecomment-391402361
data Injector a i = forall s. Injector (a -> s -> (i, s)) s -- ^the first argument is a generate function, the second argument is the initial state.

{-^
Injectors have an initial state and a generate function.

For each element, the generate function can use both:

- the value from the Transformer, and
- the current state

to determine both:

- the injection value, and
- the new state.

The first value to inject is determined by a first call to the generate function.
The first call to the generate function is with the first (or last) element from the Transformer and the initial state.

For example:

>>> funnyNext a s = (a + s, a + 1)
>>> funnyInjector = Injector funnyNext 17
>>> mapWith ((\_ i -> i) $-> funnyInjector) [4,8,3]
[21,13,12]

+-------+---------------+------+---------------+-----------------+
+ Call  + Initial State + Item + Injection     + New State       +
+=======+===============+======+===============+=================+
+ 1     + 17            + 4    + 17+4=__21__   + 4+1=5           +
+-------+---------------+------+---------------+-----------------+
+ 2     + 5             + 8    + 5+8=__13__    + 8+1=9           +
+-------+---------------+------+---------------+-----------------+
+ 3     + 9             + 3    + 9+3=__12__    + 3+1=4 (ignored) |
+-------+---------------+------+---------------+-----------------+

>>> mapWith ((\_ i -> i) <-$ funnyInjector) [4,8,3]
[13,12,20]

+-------+---------------+------+---------------+-----------------+
+ Call  + Initial State + Item + Injection     + New State       +
+=======+===============+======+===============+=================+
+ 1     + 17            + 3    + 17+3=__20__   + 3+1=4           +
+-------+---------------+------+---------------+-----------------+
+ 2     + 4             + 8    + 4+8=__12__    + 8+1=9           +
+-------+---------------+------+---------------+-----------------+
+ 3     + 9             + 4    + 9+4=__13__    + 4+1=5 (ignored) |
+-------+---------------+------+---------------+-----------------+

More usefully, this would allow for e.g. the prior two elements:

> prev2Inj = Injector (\x i@(prev1May, prev2May) -> (i, (Just x, prev1May))) (Nothing, Nothing)
-}

{-$InjCons
hello
-}

pairIt :: Injector a i1 -> Injector a i2 -> Injector a (i1, i2)
pairIt (Injector n1 z1) (Injector n2 z2) = Injector nxt (z1, z2)
  where
  nxt a ~(s1, s2) = let (i1, s1') = n1 a s1       -- !! NOTE THE ~ !! It allows "constant" iterators (e.g. limIt), and hence e.g. andFirstLast to work on infinite lists.
                        (i2, s2') = n2 a s2
                    in ((i1, i2), (s1', s2'))

nullIt :: Injector a ()
nullIt = Injector (\_ _ -> ((), ())) ()

limIt :: Injector a Bool
limIt = Injector (\_ i -> (i, False)) True
{-^
from the left: inject 'True' for the first element, else inject 'False'.

from the right: inject 'True' for the last element, else inject 'False'.
-}

ixIt :: Integral i => Injector a i
ixIt = Injector (\_ i -> (i, i+1)) 0
{-^
from the left: inject the index of the element from the start (the first element is 0).

from the right: inject the index of the element from the left (the last element is 0).
-}

zipIt :: Foldable f
      => f i          -- ^ The elements to inject. There must be enough elements.
      -> Injector a i --unsafe if we run off the end
zipIt f = Injector (\_ x -> (head x, tail x)) (toList f)
{-^
Inject each given element in turn.

from the left: the first element will be injected for the first item in the Traversable.

from the right: the first element will be injected for the last item in the Traversable.

As a result of lazyness, it is not always an error if there are not enough elements, for example:

>>> drop 1 $ mapWith ((\_ i -> i) <-$ zipIt [8,2]) "abc"
[2,8]
-}

adjElt :: Injector a (Maybe a)
adjElt = Injector (\a prevMay -> (prevMay, Just a)) Nothing
{-^
from the left: inject 'Just' the previous element, or 'Nothing' if there isn't one (i.e. for the first item).

from the right: inject 'Just' the next element, or 'Nothing' if there isn't one (i.e. for the last item).
(The "previous from the right" is the "next").
-}


itMapL :: Traversable t => Injector a i -> t (a, i -> b) -> t (a, b)
itMapL (Injector n z) = snd . mapAccumL (acc n) z

itMapR :: Traversable t => Injector a i -> t (a, i -> b) -> t (a, b)
itMapR (Injector n z) = snd . mapAccumR (acc n) z

acc :: (a -> s -> (i, s)) -> s -> (a, i -> b) -> (s, (a, b))
acc n s (a, f) = let (i, s') = n a s in (s', (a, f i))

{-$CustomMaps

If you want a map function with different values injected, you can create them. The general code is:

@
mapWith /MapPlan/ /myTraversable/
@
-}

mapWith :: Traversable t
        => MapPlan a l r b  -- ^ Created by combining a map function with injectors, using the operators listed below.
        -> t a
        -> t b
mapWith (MapPlan f itL itR) l = fmap snd $ itMapR itR $ itMapL itL $ fmap (\a -> (a, f a)) l

{-^
The map function will take one parameter from the Traversable, then one from each injector. For example:

> mapWith ((\w x y z -> (w,x,y,z)) <-$ limIt *-> ixIt <-* zipIt [8,2,7,1]) "abc"

Will call myFn 3 times, once for each character in "abc":

+------+--------+---------+---------+---------+
| Call | w      | x       | y       | z       |
+======+========+=========+=========+=========+
|    1 | \'a\'  | 'False' | 0       | 7       |
+------+--------+---------+---------+---------+
|    2 | \'b\'  | 'False' | 1       | 2       |
+------+--------+---------+---------+---------+
|    3 | \'c\'  | 'True'  | 2       | 8       |
+------+--------+---------+---------+---------+

The meaning of the operators is:

+-------------------------------+-----------------+------------------+
+                               + "from the left" + "from the right" +
+-------------------------------+-----------------+------------------+
+ First operator after function +  @$->@          +  @<-$@           +
+-------------------------------+-----------------+------------------+
+ Subsequent operators          +  @*->@          +  @<-*@           +
+-------------------------------+-----------------+------------------+

Hence:

- @<-$ limIt@: inject True if this is the limit from the right (i.e. the last element).
- @*-> ixIt@: inject the position from the left
- @<-* zipIt [8,9,10,11]@: inject elements from this list, from the right.

-}

data MapPlan a l r b = MapPlan (a -> l -> r -> b) (Injector a l) (Injector a r)

($->) :: (a -> l -> b) -> Injector a l -> MapPlan a l () b
f $-> it = MapPlan (\a l _ -> f a l) it nullIt

(<-$) :: (a -> r -> b) -> Injector a r -> MapPlan a () r b
f <-$ it = MapPlan (\a _ r -> f a r) nullIt it

(*->) :: MapPlan a l r (l' -> b) -> Injector a l' -> MapPlan a (l, l') r b
MapPlan f itL itR *-> itL' = MapPlan (\a (l, l') r -> f a l r l') (pairIt itL itL') itR

(<-*) :: MapPlan a l r (r' -> b) -> Injector a r' -> MapPlan a l (r, r') b
MapPlan f itL itR <-* itR' = MapPlan (\a l (r, r') -> f a l r r') itL (pairIt itR itR')

{-$PrePackagedMaps
Some pre-defined maps with commonly used injectors.
-}

withFirstLast :: Traversable t => (a -> Bool -> Bool -> b) -> t a -> t b
withFirstLast f = mapWith $ f $-> limIt <-* limIt
{-^
Maps over a Traversable, with additional parameters indicating whether an element is the first or last (or both) in the list.

>>> let f x isFirst isLast = star isFirst ++ x ++ star isLast; star b = if b then "*" else "" in withFirstLast f ["foo", "bar", "baz"]
["*foo", "bar", "baz*"]
-}

andFirstLast :: Traversable t => t a -> t (a, Bool, Bool)
andFirstLast = withFirstLast (,,)
-- ^ > andFirstLast = withFirstLast (,,)

withPrevNext :: Traversable t => (a -> Maybe a -> Maybe a -> b) -> t a -> t b
withPrevNext f = mapWith $ f $-> adjElt <-* adjElt
{-^
Maps over a Traversable, with additional parameters indicating the previous and next elements.

The second (or third) param to the map function is 'Nothing' when called for the first (or last) item, otherwise it's 'Just' the previous (or next) item.

>>> let f x prvMay nxtMay = maybe "*" (cmp x) prvMay ++ x ++ maybe "*" (cmp x) nxtMay; cmp x y = show $ compare x y in withPrevNext f ["foo", "bar", "baz"]
["*fooGT","LTbarLT","GTbaz*"]
-}

andPrevNext :: Traversable t => t a -> t (a, Maybe a, Maybe a)
andPrevNext = withPrevNext (,,)
-- ^ > andPrevNext = withPrevNext (,,)
