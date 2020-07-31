{-# LANGUAGE ExistentialQuantification #-}

import Data.Traversable (mapAccumL)
import MapWith
import CurryN

main = mainB'

-- This demostrates that the 61 is not "inlined" (at bce9a33), just like in the MultiInjectors branch. But could it be?

mainB = print $ sum $ mapWith (fn2 ^-> constInjB) [101, 102]

fn2 :: Int -> Int -> Int
fn2 w x | w > 10 = fn2 (w - 6) (x - 15)
        | otherwise = w + x

constInjB :: Injector a (App1 Int)
constInjB = Injector (\_ _ -> (app1 61, ())) ()

{-
Depends on INLINABLE in MapWith:

^->         mapWith    core
no          no         main11 = I# 61#
yes         yes        case $wfn 102# 61# of ww_s93b
yes         no         main11 = I# 61#
no          yes        case $wfn 102# 61# of ww_s93b
-}

mainB' = print $ sum $ mapWith (fn3 ^-> constInjB ^-> constInjB') $ take 100 primes

constInjB' :: Injector a (App1 Int)
constInjB' = Injector (\_ _ -> (app1 65, ())) ()

fn3 :: Int -> Int -> Int -> Int
fn3 w x y | w > 10 = fn3 (w - 6) (x - 15) (y + 2)
          | otherwise = w + x + y

-- now not inlined (either 61 or 65) (even with INLINABLE on mapWith, ^-> (both insts), injPair, and -fspecialise-aggressively -fexpose-all-unfoldings, SPECIALISE on ^->

-- BUT does with INLINE (on all)
-- Also without INLINE on ^-> (I guess GHC makes these "INLINABLE" anyway?)
-- Only if injPair INLINE, not INLINABLE
-- Buy MapWith INLINABLE is OK. (even w/o -fspecialise-aggressively -fexpose-all-unfoldings, SPECIALISE)

--HENCE INLINE on injPair and INLINABLE on mapWith seems to give best results.

{-
But - what does it do to performance of perf-ind-end?
with INLINE/ABLEs: (2nd run)
	total time  =        0.26 secs   (260 ticks @ 1000 us, 1 processor)
	total alloc = 344,046,128 bytes  (excludes profiling overheads)
without:
	total time  =        0.50 secs   (497 ticks @ 1000 us, 1 processor)
	total alloc = 720,046,296 bytes  (excludes profiling overheads)

So pretty good! Vs "baseline":
	total time  =        0.21 secs   (206 ticks @ 1000 us, 1 processor)
	total alloc = 352,045,968 bytes  (excludes profiling overheads)

Hoorah!

Checking perf-prev-next:
with:
	total time  =        0.17 secs   (169 ticks @ 1000 us, 1 processor)
	total alloc = 488,045,968 bytes  (excludes profiling overheads)

without:
	total time  =        0.74 secs   (738 ticks @ 1000 us, 1 processor)
	total alloc = 1,320,046,304 bytes  (excludes profiling overheads)

baseline:
	total time  =        0.18 secs   (180 ticks @ 1000 us, 1 processor)
	total alloc = 488,045,968 bytes  (excludes profiling overheads)

Blimey.
-}

{- The above is all without the CurryN stuff. With it (amazingly) we still inline. Checking performance:
perf ind-end:
	total time  =        0.31 secs   (311 ticks @ 1000 us, 1 processor)
	total alloc = 392,046,128 bytes  (excludes profiling overheads)

	total time  =        0.20 secs   (199 ticks @ 1000 us, 1 processor)
	total alloc = 512,045,968 bytes  (excludes profiling overheads)

so a slight degradation.
-}


--But:
mainC = print $ sum $ injFwd constInjC fn2 [101, 102]

constInjC :: Injector a Int
constInjC = Injector (\_ _ -> (62, ())) ()

--core has: main5 = case $wfn 101# 61# of ww_s6fD { __DEFAULT -> I# ww_s6fD }

injFwd :: Traversable t => Injector a i -> (a -> i -> b) -> t a -> t b
injFwd (Injector nxt z) f = snd . mapAccumL acc z
  where
  acc s a = let (i, s') = nxt a s in (s', f a i)

--And with a non-const list:

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

mainD = print $ sum $ injFwd constInjD fn2 $ take 100 primes

constInjD :: Injector a Int
constInjD = Injector (\_ _ -> (63, ())) ()

--Still yes: case $wfn_r736 ww2_s6VS 63# of ww3_s6W0 (A bit weird: the 63 is in there four times).

mainE = print $ sum $ myMapWith (fn2 ^*> constInjE) $ take 100 primes

constInjE :: Injector a Int
constInjE = Injector (\_ _ -> (64, ())) ()

data InjectedFn a b
  = forall l r. InjectedFnLR (a -> l -> r -> b) (Injector a l) (Injector a r)
  | forall l  . InjectedFnL  (a -> l      -> b) (Injector a l)
  | forall   r. InjectedFnR  (a      -> r -> b)                (Injector a r)

myMapWith (InjectedFnL  f (Injector gen z)) = snd . mapAccumL acc z
  where acc s a = let (i, s') = gen a s in (s', f a i)

f ^*> itL' = InjectedFnL (\a l   -> f a l) itL'
  
-- still inlined! case $wfn_r7ho ww1_s79P 64# of ww2_s79X { __DEFAULT ->

mainF = print $ sum $ myMapWith (fn3 ^*> constInjF ^**> constInjF') $ take 100 primes

constInjF :: Injector a Int
constInjF = Injector (\_ _ -> (66, ())) ()

constInjF' :: Injector a Int
constInjF' = Injector (\_ _ -> (67, ())) ()


InjectedFnL  f itL     ^**> itL' = InjectedFnL  (\a (l, l')   -> f a l   l') (injPair itL itL')

injPair :: Injector a i1 -> Injector a i2 -> Injector a (i1, i2)
injPair (Injector n1 z1) (Injector n2 z2) = Injector nxt (z1, z2)
  where
  nxt a ~(s1, s2) = let (i1, s1') = n1 a s1       -- !! NOTE THE ~ !! It allows "constant" injectors (e.g. isLim), and hence e.g. andFirstLast to work on infinite lists.
                        (i2, s2') = n2 a s2
                    in ((i1, i2), (s1', s2'))

--inlined! case $wfn3 ww1_s7iF 66# 67# of ww2_s7iR { __DEFAULT ->
-- even without -fspecialise-aggressively -fexpose-all-unfoldings



