{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module MapWith
  (
    Iterator(..)
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

data Iterator a i = forall s. Iterator
  (i -> s -> (a, s)) --like a transformer, but also takes a value from the set we're mapping over
  s                  --initial state

pairIt :: Iterator a i -> Iterator b i -> Iterator (a, b) i
pairIt (Iterator n1 z1) (Iterator n2 z2) = Iterator nxt (z1, z2)
  where
  nxt i (s1, s2) = let (x1, s1') = n1 i s1
                       (x2, s2') = n2 i s2
                   in ((x1, x2), (s1', s2'))

nullIt :: Iterator () i
nullIt = Iterator (\_ _ -> ((), ())) ()

limIt :: Iterator Bool i
limIt = Iterator (\_ x -> (x, False)) True

ixIt :: Integral a => Iterator a i
ixIt = Iterator (\_ n -> (n, n+1)) 0

zipIt :: Foldable f => f a -> Iterator a i --unsafe if we run off the end
zipIt f = Iterator (\_ x -> (head x, tail x)) (toList f)

adjElt :: Iterator (Maybe i) i
adjElt = Iterator (\x prevMay -> (prevMay, Just x)) Nothing

itMapL :: Traversable t => forall b a. Iterator b i -> t (i, (b -> a)) -> t (i, a)
itMapL (Iterator nxt sInit) = snd . mapAccumL applyBound sInit
  where
  applyBound s (i, f) = let (x, s') = nxt i s in (s', (i, f x))

itMapR :: Traversable t => forall b a. Iterator b i -> t (i, (b -> a)) -> t (i, a)
itMapR (Iterator nxt sInit) = snd . mapAccumR applyBound sInit
  where
  applyBound s (i, f) = let (x, s') = nxt i s in (s', (i, f x))

data ItPlan a l r b = ItPlan (a -> l -> r -> b) (Iterator l a) (Iterator r a)

($->) :: (a -> l -> b) -> Iterator l a -> ItPlan a l () b
f $-> it = ItPlan (\a l _ -> f a l) it nullIt

(<-$) :: (a -> r -> b) -> Iterator r a -> ItPlan a () r b
f <-$ it = ItPlan (\a _ r -> f a r) nullIt it

(*->) :: ItPlan a l r (l' -> b) -> Iterator l' a -> ItPlan a (l, l') r b
ItPlan f itL itR *-> itL' = ItPlan (\a (l, l') r -> f a l r l') (pairIt itL itL') itR

(<-*) :: ItPlan a l r (r' -> b) -> Iterator r' a -> ItPlan a l (r, r') b
ItPlan f itL itR <-* itR' = ItPlan (\a l (r, r') -> f a l r r') itL (pairIt itR itR')

mapWith :: Traversable t => ItPlan a l r b -> t a -> t b
mapWith (ItPlan f itL itR) l = fmap snd $ itMapR itR $ itMapL itL $ fmap (\x -> (x, f x)) l

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
-}


