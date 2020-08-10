{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Traversable (mapAccumL)
import Data.Function ((&))
import MapWith
import CurryN

main = mainP

mainA = print $ sum $ mapWith (fn2 <-^ eltIx) $ take 100 primes

--This file is for various experiments in tuning.
--These have so far shown where addition of INLINABLE pragmas is very beneficial.
--As a result, I now have comperable performance to the "baselines".
--I don't (yet) have comperable performance to markbounds, which remains a challenge.


-- This demostrates that the 61 is not "inlined" (at bce9a33), just like in the MultiInjectors branch. But could it be?
mainB = print $ sum $ mapWith (fn2 ^-> constInjB) $ take 100 primes

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

perf-prev-next:
	total time  =        0.20 secs   (199 ticks @ 1000 us, 1 processor)
	total alloc = 512,045,968 bytes  (excludes profiling overheads)

so a slight degradation.  Why?

Comparing mainB: curry & uncurry (with INLINE/ABLEs) are identical.

Comparing mainA, curry has case i_a5xN of { (arg1_au0, moreArgs1_au1), so is not inlining the recursive uncurryN calls.
(Although we can see from CurryNPerf that (surpisingly?) it is capable of doing so).

Now with INLINABLE in eltIx etc:
-- perf-ind-end:
	total time  =        0.16 secs   (164 ticks @ 1000 us, 1 processor)
	total alloc = 216,045,936 bytes  (excludes profiling overheads)
(Hmmm better that the baseline???)

-- perf-prev-next:
	total time  =        0.18 secs   (182 ticks @ 1000 us, 1 processor)
	total alloc = 512,045,968 bytes  (excludes profiling overheads)
-}

--But:
mainC = print $ sum $ injFwd constInjC fn2 $ take 100 primes

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

data MyInjectedFn a b
  = forall l r. MyInjectedFnLR (a -> l -> r -> b) (Injector a l) (Injector a r)
  | forall l  . MyInjectedFnL  (a -> l      -> b) (Injector a l)
  | forall   r. MyInjectedFnR  (a      -> r -> b)                (Injector a r)

myMapWith (MyInjectedFnL  f (Injector gen z)) = snd . mapAccumL acc z
  where acc s a = let (i, s') = gen a s in (s', f a i)

(^*>) :: (a -> i -> b) -> Injector a i -> MyInjectedFn a b
f ^*> itL' = MyInjectedFnL (\a l   -> f a l) itL'


-- still inlined! case $wfn_r7ho ww1_s79P 64# of ww2_s79X { __DEFAULT ->

mainF = print $ sum $ myMapWith (fn3 ^*> constInjF ^**> constInjF') $ take 100 primes

constInjF :: Injector a Int
constInjF = Injector (\_ _ -> (66, ())) ()

constInjF' :: Injector a Int
constInjF' = Injector (\_ _ -> (67, ())) ()


MyInjectedFnL  f itL     ^**> itL' = MyInjectedFnL  (\a (l, l')   -> f a l   l') (injPair itL itL')

injPair :: Injector a i1 -> Injector a i2 -> Injector a (i1, i2)
injPair (Injector n1 z1) (Injector n2 z2) = Injector nxt (z1, z2)
  where
  nxt a ~(s1, s2) = let (i1, s1') = n1 a s1       -- !! NOTE THE ~ !! It allows "constant" injectors (e.g. isLim), and hence e.g. andFirstLast to work on infinite lists.
                        (i2, s2') = n2 a s2
                    in ((i1, i2), (s1', s2'))

--inlined! case $wfn3 ww1_s7iF 66# 67# of ww2_s7iR { __DEFAULT ->
-- even without -fspecialise-aggressively -fexpose-all-unfoldings

--This is uses a local copy of Curry, and is inlined (so it is possible!)
mainG = print $ sum $ myMapWith (fn2 ^+> myEltIx) $ take 100 primes

myEltIx :: Integral i => Injector a (i, ())
myEltIx = Injector (\_ i -> ((i, ()), i+1)) 0

(^+>) :: MyCurryN i b => (a -> MyFnType i b) -> Injector a i -> MyInjectedFn a b
f ^+> itL' = MyInjectedFnL (\a l   -> f a $## l) itL'

($##) :: MyCurryN args r => MyFnType args r -> args -> r
f $## args = (myUncurryN f) args

class MyCurryN args r where
  type MyFnType args r :: *
  myUncurryN :: MyFnType args r -> args -> r

instance MyCurryN () r where
  type MyFnType () r = r
  myUncurryN f () = f

instance MyCurryN moreArgs r => MyCurryN (arg, moreArgs) r where
  type MyFnType (arg, moreArgs) r = arg -> (MyFnType moreArgs r)
  myUncurryN f (arg, moreArgs) = myUncurryN (f arg) moreArgs

--mainH also uses local Curry, but eltIx from MapWith, and isn't inlined!
mainH = print $ sum $ myMapWith (fn2 ^+> eltIx) $ take 100 primes

--But is if we set INLINABLE on eltIx!


--ABOVE here: gets perf equiv to "baselines". But they use mapAccumL/R, and don't seem to fuse.
--Ideally I'd like performance similar to markbounds, so there's more work to do...

mainJ = print $ sum $ map fn2Tup $ markbounds [1..1000000]

fn2Tup (x, True, _   ) = x + 10
fn2Tup (x, _,    True) = x * 2
fn2Tup (x, _,    _   ) = x

markbounds :: [a] -> [(a, Bool, Bool)]
markbounds [] = []
markbounds [x] = [(x, True, True)]
markbounds (x:xs) = (x, True, False) : tailbound xs
  where
    tailbound [y] = [(y, False, True)]
    tailbound (y:ys) = (y, False, False): tailbound ys
    
    
{- mainJ:    
	total time  =        0.10 secs   (99 ticks @ 1000 us, 1 processor)
	total alloc = 176,045,824 bytes  (excludes profiling overheads)
-}

mainK = print $ sum $ withFirstLast fn2Args [1..1000000]

fn2Args x True  _    = x + 10
fn2Args x _     True = x * 2
fn2Args x _     _    = x

{- mainK:
	total time  =        0.29 secs   (290 ticks @ 1000 us, 1 processor)
	total alloc = 488,045,920 bytes  (excludes profiling overheads)
-}

mainL = print $ sum ([1..1000000] :: [Int])

{-  Very good: doesn't create a list.

	total time  =        0.00 secs   (0 ticks @ 1000 us, 1 processor)
	total alloc =      45,912 bytes  (excludes profiling overheads)

$wgo
  = \ counter sumSoFar ->
      case counter of counter' {
        __DEFAULT -> $wgo (+# counter' 1#) (+# sumSoFar counter');
        1000000# -> +# sumSoFar 1000000#
      }

main2
  = case $wgo 1# 0# of theSum { __DEFAULT ->
    case $wshowSignedInt 0# theSum [] of { (# showRslt1, showRslt2 #) ->
    : showRslt1 showRslt12
    }
    }
-}

mainM = print $ sum $ mapWith (fn1Arg & isFirst) [1..1000000]
-- perfect!
fn1Arg :: Int -> Bool -> Int
fn1Arg n True  = n * 78
fn1Arg n False = n
--{-# NOINLINE fn1Arg #-}

mainN = print $ sum $ mapWith (fn4 & prevElt) [1..1000000]
--also perfect!
fn4 :: Int -> Maybe Int -> Int
fn4 x (Just y) = x + y
fn4 x Nothing = x * 2

mainP = print $ sum $ mapWith (fn1Arg & isEven) [1..1000000]
--perfect with Injector-based isEven.

{- Wow! It does two numbers with each loop!
main_$s$wgo
  = \ sumSoFar n ->
      case n of n' {
        __DEFAULT ->
          let { nPlus1 = +# n' 1# } in
          main_$s$wgo (+# (+# sumSoFar (*# n' 78#)) nPlus1) (+# nPlus1 1#);
        999999# -> +# (+# sumSoFar 77999922#) 1000000#;
        1000000# -> +# sumSoFar 78000000#
      }

main2
  = case main_$s$wgo 0# 1# of and ...
-}

