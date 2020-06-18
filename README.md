# MapWith
mapWith: like fmap, but can "inject" additional arguments such as whether first (or last) element, etc.

# Examples

Use of a simple APIs:

    let f x isStart isEnd = star isStart ++ x ++ star isEnd where
        star b = case b of True -> "*"; False -> ""
    in withFirstLast f ["foo", "bar", "baz"]
    == ["*foo", "bar", "baz*"]

Or a "custom built" injection of various parameters:

    mapWith ((,,,,) $-> limIt <-* ixIt *-> adjElt <-* zipIt "hello") "abc"
    == [('a',True,2,Nothing,'l'),('b',False,1,Just 'a','e'),('c',False,0,Just 'b','h')]

# Usage

## Simple APIs

There are four simple APIs, which should be easy to use.

The two beginning "with" take a function as first parameter. The function takes 3 parameters: the Traversable element then two injected parameters.
The two beginning "and" don't take a function, they return a triple of (element, injected1, injected2)

The two ending FirstLast inject two Bools: am I the first in the Traversable, am I the last?
The two ending PrevNext inject two Maybes. These are Nothing for the first or last elements.

## Custom maps

    mapWith <plan> <traversable>

A plan is built from a function combined with "injectors":

    f <op> injector <op> injector ...

There has to be at least one injector. f must take 1 parameter (from the traversable) plus one from each injector.

The first op has to be $-> or <-$. Subsequent ones must be \*-> or <-\*

$-> and \*-> mean "from the left", <-$ and <-\* mean "from the right"

    f $-> limIt
  
injects a True for the first element, then False for the rest.

    f <-$ limIt
  
injects a True for the last element.

Other injectors:
* ixIt: the 0-based index from the start or end
* zipIt l: the nth element from l.
* adjElt: the adjacent element. With $-> essentially gives the previous element, <-$ gives the next.

## Custom Injectors

You can make your own!

# To Do

## Optimisations

There are two optimisations I'd like to make:
1. in the case that we only traverse in one direction, we don't call both itMapL and itMapR
2. in the case that we don't use the Traversable's elements (e.g. in adjElt), we don't construct the (init value, f...) pairs

But maybe after the optimising compilers been at it, neither of these are a problem? 
And I've not found good ways to do them (yet) either.

## Fewer Operators

Something a bit like this, so I don't need different operators at the start:

    mapWith (f *-> limIr *-> ixIt) l

    class Mapper m where
      (*->) :: (m a l r (l' -> b)) -> Injector a l' -> MapPlan a (l, l') r b
      
    instance Mapper MapPlan where
      MapPlan f itL itR *-> itL' = MapPlan (\a (l, l') r -> f a l r l') (pairIt itL itL') itR
      
    instance Mapper (-> a b) where  --ERROR!!
      f *-> itL' = MapPlan (\a l _ -> f a l) itL' nullIt

Is there a way around the type mismatch?

