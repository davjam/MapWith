{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module MapWith
  (
    Iterator(..)
  , limIt, ixIt, zipIt
  
  , mapWith
  , ($->), (<-$), (*->), (<-*)
  
  , withFirstLast, tagFirstLast
  )
where

import Data.Foldable (toList)
import Data.Traversable (mapAccumL, mapAccumR)

data Iterator a = forall s. Iterator
  (s -> (a, s)) --like a transformer
  s             --initial state

pairIt :: Iterator a -> Iterator b -> Iterator (a, b)
pairIt (Iterator n1 z1) (Iterator n2 z2) = Iterator nxt (z1, z2)
  where
  nxt (s1, s2) = let (x1, s1') = n1 s1
                     (x2, s2') = n2 s2
                 in ((x1, x2), (s1', s2'))

nullIt :: Iterator ()
nullIt = Iterator (\_ -> ((), ())) ()

limIt :: Iterator Bool
limIt = Iterator (\x -> (x, False)) True

ixIt :: Integral a => Iterator a
ixIt = Iterator (\n -> (n, n+1)) 0

zipIt :: Foldable f => f a -> Iterator a --unsafe if we run off the end
zipIt f = Iterator (\x -> (head x, tail x)) (toList f)

itMapL :: Traversable t => forall b a. Iterator b -> t (b -> a) -> t a
itMapL (Iterator nxt sInit) = snd . mapAccumL applyBound sInit
  where
  applyBound s f = let (x, s') = nxt s in (s', f x)

{-
itMapL' :: Traversable t => forall b a. Iterator b -> t (init, b -> a) -> t (init, a)
itMapL' (Iterator nxt sInit) = snd . mapAccumL applyBound sInit
  where
  applyBound s (i, f) = let (x, s') = nxt s in (s', (i, f x))
-}

itMapR :: Traversable t => forall b a. Iterator b -> t (b -> a) -> t a
itMapR (Iterator nxt sInit) = snd . mapAccumR applyBound sInit
  where
  applyBound s f = let (x, s') = nxt s in (s', f x)

{-
This is an optimisation, though I'm not sure it's really worth it.
Ideally, if NeedsL, we'd only itL, but I can't find a way to make that happen.
At least this just applies only an fmap instead of a mapAccumR, but does this save much?
-}

zMap :: Traversable t => forall b a. Iterator b -> t (b -> a) -> t a
zMap (Iterator nxt sInit) t = let (x, _) = nxt sInit in fmap ($ x) t

data ItPlan a l r b = ItPlan ItNeeds (a -> l -> r -> b) (Iterator l) (Iterator r)

data ItNeeds = NeedsL | NeedsR | NeedsLR
  deriving Show

instance Semigroup ItNeeds where
  NeedsL <> NeedsL = NeedsL
  NeedsR <> NeedsR = NeedsR
  _      <> _      = NeedsLR

($->) :: (a -> l -> b) -> Iterator l -> ItPlan a l () b
f $-> it = ItPlan NeedsL (\a l _ -> f a l) it nullIt

(<-$) :: (a -> r -> b) -> Iterator r -> ItPlan a () r b
f <-$ it = ItPlan NeedsR (\a _ r -> f a r) nullIt it

(*->) :: ItPlan a l r (l' -> b) -> Iterator l' -> ItPlan a (l, l') r b
ItPlan n f itL itR *-> itL' = ItPlan (n <> NeedsL) (\a (l, l') r -> f a l r l') (pairIt itL itL') itR

(<-*) :: ItPlan a l r (r' -> b) -> Iterator r' -> ItPlan a l (r, r') b
ItPlan n f itL itR <-* itR' = ItPlan (n <> NeedsR) (\a l (r, r') -> f a l r r') itL (pairIt itR itR')

mapWith :: Traversable t => ItPlan a l r b -> t a -> t b
mapWith (ItPlan NeedsL  f itL itR) l = zMap   itR $ itMapL itL $ fmap f l
mapWith (ItPlan NeedsR  f itL itR) l = itMapR itR $ zMap   itL $ fmap f l
mapWith (ItPlan NeedsLR f itL itR) l = itMapR itR $ itMapL itL $ fmap f l

withFirstLast :: Traversable t => (a -> Bool -> Bool -> b) -> t a -> t b
withFirstLast f = mapWith $ f $-> limIt <-* limIt

tagFirstLast :: Traversable t => t a -> t (a, Bool, Bool)
tagFirstLast = withFirstLast (,,)
