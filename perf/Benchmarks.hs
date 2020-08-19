--NOT IN GIT, to make cross-version comparison easier.

module Main (main)
where

import Data.Function ((&))
import MapWith

main = prevNextMapWith
lots = [1..100000000] :: [Int]

--Left-based maps:
firstRec     = print $ sum $ withFirstRec  fnBool lots
firstScan    = print $ sum $ withFirstScan fnBool lots
firstMap     = print $ sum $ withFirstMap  fnBool lots
firstMapWith = print $ sum $ mapWith (fnBool & isFirst) lots

prevRec      = print $ sum $ withPrevRec   fnAdj  lots
prevScan     = print $ sum $ withPrevScan  fnAdj  lots
prevZip      = print $ sum $ withPrevZip   fnAdj  lots
prevMapWith  = print $ sum $ mapWith (fnAdj ^-> adjElt) lots

--Right-based maps:
lastRec      = print $ sum $ withLastRec   fnBool lots
lastScan     = print $ sum $ withLastScan  fnBool lots
lastMap      = print $ sum $ withLastMap   fnBool lots
lastMapWith  = print $ sum $ mapWith (fnBool & isLast) lots

nextRec      = print $ sum $ withNextRec   fnAdj  lots
nextScan     = print $ sum $ withNextScan  fnAdj  lots
nextZip      = print $ sum $ withNextZip   fnAdj  lots
nextMapWith  = print $ sum $ mapWith (fnAdj <-^ adjElt) lots

--Left and Right:
firstLastRec     = print $ sum $ withFirstLastRec fnBoolBool lots
firstLastMap     = print $ sum $ withFirstLastMap fnBoolBool lots
firstLastMapWith = print $ sum $ withFirstLast    fnBoolBool lots
firstLastMB      = print $ sum $ map fnBoolBoolTup $ markbounds lots

prevNextRec      = print $ sum $ withPrevNextRec fnAdjAdj  lots
prevNextZip      = print $ sum $ withPrevNextZip fnAdjAdj  lots
prevNextMapWith  = print $ sum $ withPrevNext    fnAdjAdj  lots

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

fnAdjAdj :: Int -> Maybe Int -> Maybe Int -> Int
fnAdjAdj n Nothing  Nothing  =  n          * 9
fnAdjAdj n (Just m) Nothing  = (n + m    ) * 8
fnAdjAdj n Nothing  (Just p) = (n     + p) * 7
fnAdjAdj n (Just m) (Just p) = (n + m + p) * 6
