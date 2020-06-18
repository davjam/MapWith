{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module MapWith
  (
    Injector(..)
  , limIt, ixIt, zipIt
  , adjElt
  
  , mapWith
  , ($->), (<-$), (*->), (<-*)
  
  , withFirstLast, andFirstLast
  , withPrevNext, andPrevNext
  )
where

import Data.Foldable (toList)
import Data.Traversable (mapAccumL, mapAccumR)

{-
Common type/value names:
a = the value in the Traversable we're mapping over
b = the result in the final Traversable

i = an output from an Injector, injected into the mapper
l = an i that's determined from the left
r = an i that's determined from the right

s = internal state in an Injector
-}

data Injector a i = forall s. Injector
  (a -> s -> (i, s)) --like a transformer, but also takes a value from the set we're mapping over
  s                  --initial state

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

ixIt :: Integral i => Injector a i
ixIt = Injector (\_ i -> (i, i+1)) 0

zipIt :: Foldable f => f i -> Injector a i --unsafe if we run off the end
zipIt f = Injector (\_ x -> (head x, tail x)) (toList f)

adjElt :: Injector a (Maybe a)
adjElt = Injector (\a prevMay -> (prevMay, Just a)) Nothing

itMapL :: Traversable t => Injector a i -> t (a, i -> b) -> t (a, b)
itMapL (Injector n z) = snd . mapAccumL (acc n) z

itMapR :: Traversable t => Injector a i -> t (a, i -> b) -> t (a, b)
itMapR (Injector n z) = snd . mapAccumR (acc n) z

acc :: (a -> s -> (i, s)) -> s -> (a, i -> b) -> (s, (a, b))
acc n s (a, f) = let (i, s') = n a s in (s', (a, f i))

data MapPlan a l r b = MapPlan (a -> l -> r -> b) (Injector a l) (Injector a r)

($->) :: (a -> l -> b) -> Injector a l -> MapPlan a l () b
f $-> it = MapPlan (\a l _ -> f a l) it nullIt

(<-$) :: (a -> r -> b) -> Injector a r -> MapPlan a () r b
f <-$ it = MapPlan (\a _ r -> f a r) nullIt it

(*->) :: MapPlan a l r (l' -> b) -> Injector a l' -> MapPlan a (l, l') r b
MapPlan f itL itR *-> itL' = MapPlan (\a (l, l') r -> f a l r l') (pairIt itL itL') itR

(<-*) :: MapPlan a l r (r' -> b) -> Injector a r' -> MapPlan a l (r, r') b
MapPlan f itL itR <-* itR' = MapPlan (\a l (r, r') -> f a l r r') itL (pairIt itR itR')

mapWith :: Traversable t => MapPlan a l r b -> t a -> t b
mapWith (MapPlan f itL itR) l = fmap snd $ itMapR itR $ itMapL itL $ fmap (\a -> (a, f a)) l

withFirstLast :: Traversable t => (a -> Bool -> Bool -> b) -> t a -> t b
withFirstLast f = mapWith $ f $-> limIt <-* limIt

andFirstLast :: Traversable t => t a -> t (a, Bool, Bool)
andFirstLast = withFirstLast (,,)

withPrevNext :: Traversable t => (a -> Maybe a -> Maybe a -> b) -> t a -> t b
withPrevNext f = mapWith $ f $-> adjElt <-* adjElt

andPrevNext :: Traversable t => t a -> t (a, Maybe a, Maybe a)
andPrevNext = withPrevNext (,,)

{-
There are two optimisations I'd like to make:
1. in the case that we only traverse in one direction, we don't call both itMapL and itMapR
2. in the case that we don't use the Traversable's elements (e.g. in adjElt), we don't construct the (init value, f...) pairs

BUT: maybe after the optimising compilers been at it, neither of these are a problem?
AND: I've not found good ways to do either.
-}
