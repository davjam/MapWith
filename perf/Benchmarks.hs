module Benchmarks (testFn, myCycle)
where

import Data.Function ((&))
import MapWith
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Base (build)

{-
BEWARE: this is very sensitive to changes.
E.g. adding SCC tags, or sharing the list between test fns
can give incorrect benchmarks.
-}

{-# INLINE testFn #-}
testFn :: Int -> Int -> [Int]
--left-based maps:
testFn  1 n = withFirstRec     fnBool                     [1..n]
testFn  2 n = withFirstScan    fnBool                     [1..n]
testFn  3 n = withFirstMap     fnBool                     [1..n]
testFn  4 n = mapWith         (fnBool & isFirst)          [1..n]

testFn  5 n = withPrevRec      fnAdj                      [1..n]
testFn  6 n = withPrevScan     fnAdj                      [1..n]
testFn  7 n = withPrevZip      fnAdj                      [1..n]
testFn  8 n = mapWith         (fnAdj ^-> adjElt)          [1..n]

--Right-based maps:
testFn  9 n = withLastRec      fnBool                     [1..n]
testFn 10 n = withLastScan     fnBool                     [1..n]
testFn 11 n = withLastMap      fnBool                     [1..n]
testFn 12 n = mapWith         (fnBool & isLast)           [1..n]

testFn 13 n = withNextRec      fnAdj                      [1..n]
testFn 14 n = withNextScan     fnAdj                      [1..n]
testFn 15 n = withNextZip      fnAdj                      [1..n]
testFn 16 n = mapWith         (fnAdj <-^ adjElt)          [1..n]

--Left and Right:
testFn 17 n = withFirstLastRec fnBoolBool                 [1..n]
testFn 18 n = withFirstLastMap fnBoolBool                 [1..n]
testFn 19 n = withFirstLast    fnBoolBool                 [1..n]
testFn 20 n = map              fnBoolBoolTup $ markbounds [1..n]

testFn 21 n = withPrevNextRec  fnAdjAdj                   [1..n]
testFn 22 n = withPrevNextZip  fnAdjAdj                   [1..n]
testFn 23 n = withPrevNext     fnAdjAdj                   [1..n]

--Foldl injector:
testFn 30 n = mapWith ((+) ^-> foldlElts (+) 0)           [1..n]
testFn 31 n = mapWith ((+) <-^ foldlElts (+) 0)           [1..n]    --performance not great. data not PINNED.
testFn 32 n = let xs = [1..n] in zipWith (+) xs (scanr (+) 0 xs)    --but neither is this.

--eltIx injector:
testFn 33 n = mapWith ((+) ^-> eltIx)                     [1..n]
testFn 34 n = mapWith ((+) <-^ eltIx)                     [1..n]

--adj2Elts injector:
testFn 35 n = mapWith (fnAdjAdj ^-> adj2Elts)             [1..n]
testFn 36 n = mapWith (fnAdjAdj <-^ adj2Elts)             [1..n]

--eltFrom/etc
testFn 37 n = mapWith ((+) ^-> eltFrom [3,5..(n*3)])      [1..n]
testFn 38 n = mapWith ((+) <-^ eltFrom [3,5..(n*3)])      [1..n]
testFn 39 n = mapWith ((+) ^-> eltFromDef 7 [3,12,2,9])   [1..n]
testFn 40 n = mapWith ((+) <-^ eltFromDef 7 [3,12,2,9])   [1..n]
testFn 43 n = mapWith (fnAdj ^-> eltFromMay [3,12,2,9])   [1..n]
testFn 44 n = mapWith (fnAdj <-^ eltFromMay [3,12,2,9])   [1..n]
testFn 45 n = mapWith ((+) ^-> eltFrom (cycle [3,12,2,9]))[1..n]
testFn 46 n = mapWith ((+) <-^ eltFrom (cycle [3,12,2,9]))[1..n]
testFn 47 n = mapWith ((+) ^-> eltFrom (myCycle [3,12,2,9]))[1..n]
testFn 48 n = mapWith ((+) <-^ eltFrom (myCycle [3,12,2,9]))[1..n]

--Some more fusion tests:
testFn 100 n = take n $ withFirst     fnBool           $ repeat  (100 :: Int)
testFn 107 n = take n $ withLast      fnBool           $ repeat  (12 :: Int)
testFn 101 n = take n $ withFirstLast fnBoolBool       $ repeat  (100 :: Int)
testFn 108 n = take n $ withFirst     fnBool           $ cycle   ([10,15,19,2] :: [Int])
testFn 109 n = take n $ withLast      fnBool           $ cycle   ([10,15,19,2] :: [Int])
testFn 102 n = take n $ withFirstLast fnBoolBool       $ cycle   ([10,15,19,2] :: [Int])
testFn 110 n = take n $ withFirst     fnBool           $ myCycle ([10,15,19,2] :: [Int])
testFn 111 n = take n $ withLast      fnBool           $ myCycle ([10,15,19,2] :: [Int])
testFn 105 n = take n $ withFirstLast fnBoolBool       $ myCycle ([10,15,19,2] :: [Int])

testFn 103 n = take n $ map fnBoolBoolTup $ markbounds $ repeat  (100 :: Int)
testFn 104 n = take n $ map fnBoolBoolTup $ markbounds $ cycle   ([10,15,19,2] :: [Int])
testFn 106 n = take n $ map fnBoolBoolTup $ markbounds $ myCycle ([10,15,19,2] :: [Int])

testFn 112 n = take n $ withFirst     fnBool           $ iterate (+1) 1
testFn 113 n = take n $ withLast      fnBool           $ iterate (+1) 1
testFn 114 n = take n $ withFirstLast fnBoolBool       $ iterate (+1) 1
testFn 115 n = take n $ map fnBoolBoolTup $ markbounds $ iterate (+1) 1

myCycle :: [a] -> [a]
myCycle xs = xs' where xs' = xs ++ xs'
{-# NOINLINE [1] myCycle #-}

{-# RULES "myCycle/build" [~1] forall (f::forall b.(a->b->b) -> b -> b). myCycle (build f) = build (\c _n -> let z = f c z in z)
    #-}

--Hand crafted alternatives to mapWith

--recursive
withFirstRec :: (a -> Bool -> b) -> [a] -> [b]
withFirstRec f = go True
  where
    go _       []     = []
    go isFirst (x:xs) = f x isFirst : go False xs

withPrevRec :: (a -> Maybe a -> b) -> [a] -> [b]
withPrevRec f = go Nothing
  where
    go _          []     = []
    go prevEltMay (x:xs) = f x prevEltMay : go (Just x) xs

withLastRec :: (a -> Bool -> b) -> [a] -> [b]
withLastRec f = go
  where
    go []     = []
    go [x]    = f x True  : []
    go (x:xs) = f x False : go xs

withNextRec :: (a -> Maybe a -> b) -> [a] -> [b]
withNextRec f = go
  where
    go []           = []
    go (x:[])       = f x Nothing  : []
    go (x:xs@(n:_)) = f x (Just n) : go xs

withFirstLastRec :: (a -> Bool -> Bool -> b) -> [a] -> [b]
withFirstLastRec f = go
  where
    go []         = []
    go [x]        = f x True  True  : []
    go (x:xs)     = f x True  False : goRest xs
    goRest []     = undefined
    goRest [x]    = f x False True  : []
    goRest (x:xs) = f x False False : goRest xs

withPrevNextRec :: (a -> Maybe a -> Maybe a -> b) -> [a] -> [b]
withPrevNextRec f = go Nothing
  where
    go _          []           = []
    go prevEltMay (x:[])       = f x prevEltMay Nothing  : []
    go prevEltMay (x:xs@(n:_)) = f x prevEltMay (Just n) : go (Just x) xs

--the original from https://stackoverflow.com/questions/14114011/haskell-map-operation-with-different-first-and-last-functions
markbounds :: [a] -> [(a, Bool, Bool)]
markbounds [] = []
markbounds [x] = [(x, True, True)]
markbounds (x:xs) = (x, True, False) : tailbound xs
  where
    tailbound [y] = [(y, False, True)]
    tailbound (y:ys) = (y, False, False): tailbound ys

--scans
withFirstScan :: (a -> Bool -> b) -> [a] -> [b]
withFirstScan f [] = []
withFirstScan f (x:xs) = map fst $ scanl acc (f x True, False) xs
  where
    acc (_, isFirst) x = (f x isFirst, False)
    
withLastScan :: (a -> Bool -> b) -> [a] -> [b]
withLastScan f [] = []
withLastScan f xs = map fst $ scanr acc (f (last xs) True, False) (init xs)
  where
    acc x (_, isLast) = (f x isLast, False)
    
withPrevScan :: (a -> Maybe a -> b) -> [a] -> [b]
withPrevScan f [] = []
withPrevScan f (x:xs) = map fst $ scanl acc (f x Nothing, Just x) xs
  where
    acc (_, prevMay) x = (f x prevMay, Just x)

withNextScan :: (a -> Maybe a -> b) -> [a] -> [b]
withNextScan f [] = []
withNextScan f xs = map fst $ let x = last xs in scanr acc (f x Nothing, Just x) (init xs)
  where
    acc x (_, nextMay) = (f x nextMay, Just x)

--map/zip
withFirstMap :: (a -> Bool -> b) -> [a] -> [b]
withFirstMap _ [] = []
withFirstMap f (x:xs) = f x True : map (flip f False) xs

withLastMap :: (a -> Bool -> b) -> [a] -> [b]
withLastMap _ [] = []
withLastMap f xs = map (flip f False) (init xs) ++ [f (last xs) True]

withPrevZip :: (a -> Maybe a -> b) -> [a] -> [b]
withPrevZip f xs = zipWith f xs (Nothing : map Just xs)

withNextZip :: (a -> Maybe a -> b) -> [a] -> [b]
withNextZip f xs = zipWith f xs $ map Just (tail xs) ++ [Nothing]

withFirstLastMap :: (a -> Bool -> Bool -> b) -> [a] -> [b]
withFirstLastMap _ [] = []
withFirstLastMap f [x] = [f x True True]
withFirstLastMap f (x:xs) = f x True False : map (\x -> f x False False) (init xs) ++ [f (last xs) False True]

withPrevNextZip :: (a -> Maybe a -> Maybe a -> b) -> [a] -> [b]
withPrevNextZip f xs = zipWith3 f xs (Nothing : map Just xs) (map Just (tail xs) ++ [Nothing])

--injected functions
fnBool :: Int -> Bool -> Int
fnBool n True  = n * 9
fnBool n False = n * 8

fnAdj :: Int -> Maybe Int -> Int
fnAdj n Nothing  =  n      * 9
fnAdj n (Just m) = (n + m) * 8

fnBoolBool :: Int -> Bool -> Bool -> Int
fnBoolBool n True  True  = n * 9
fnBoolBool n True  False = n * 8
fnBoolBool n False True  = n * 7
fnBoolBool n False False = n * 6

fnBoolBoolTup :: (Int, Bool, Bool) -> Int
fnBoolBoolTup (n, True , True ) = n * 9
fnBoolBoolTup (n, True , False) = n * 8
fnBoolBoolTup (n, False, True ) = n * 7
fnBoolBoolTup (n, False, False) = n * 6

{-# INLINE fnAdjAdj #-} --INLINE makes test 35 fast. (But doesn't impact test 36).
fnAdjAdj :: Int -> Maybe Int -> Maybe Int -> Int
fnAdjAdj n Nothing  Nothing  =  n          * 9
fnAdjAdj n (Just m) Nothing  = (n + m    ) * 8
fnAdjAdj n Nothing  (Just p) = (n     + p) * 7
fnAdjAdj n (Just m) (Just p) = (n + m + p) * 6

{-
This has the same effect on test 35 as marking INLINE.
fnAdjAdj n mMay pMay =
  case pMay of Nothing  -> case mMay of Nothing  ->  n          * 9
                                        (Just m) -> (n + m    ) * 8
               (Just p) -> case mMay of Nothing  -> (n     + p) * 7
                                        (Just m) -> (n + m + p) * 6
-}
