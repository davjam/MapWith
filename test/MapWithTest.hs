{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Main (main)
where

import System.Exit
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..), fromList)
import MapWith

testFn0 :: a -> b -> b
testFn0 _ b = b

testFn :: String -> Bool -> Int -> String
testFn s True  index = replicate index ' ' ++ s ++ "*"
testFn s False index = replicate index ' ' ++ s

testFn2 :: String -> Bool -> Int -> Int -> String
testFn2 s True  index ind2 = replicate index ' ' ++ s ++ (show ind2) ++ "*"
testFn2 s False index ind2 = replicate index ' ' ++ s ++ (show ind2)

testFnP :: String -> (Bool, Int) -> String
testFnP s (True , index) = replicate index ' ' ++ s ++ "*"
testFnP s (False, index) = replicate index ' ' ++ s

r0 = mapWith (testFn0 ^-> isLim) fbb
r2 = mapWith (testFn  <-^ isLim <-^ eltIx) fbb
r3 = mapWith (testFn  <-^ isLim ^-> eltIx) fbb
r4 = mapWith (testFn2 ^-> isLim ^-> eltIx <-^ eltFrom [8,9,10,11]) fbb

fbb = ["foo", "bar", "baz"]

data FunnySet a = FunnySet a a a a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

sillyInj :: Injector Int (App1 Int) --testing we've exported enough to make building Injectors easy.
sillyInj = Injector (\a s -> (s+2, app1 (a+s))) 3

tests :: [Bool]
tests =
  [
    mapWith (testFn0 & isFirst) "abc"   == [True,  False, False]
  , mapWith (testFn0 & isLast) "abc"   == [False, False, True ]
  , mapWith ((\_ a b c d -> (a,b,c,d)) <-^ isLim ^-> eltIx <-^ eltIx ^-> isLim) "abc"
                                        == [(False,0,2,True),(False,1,1,False),(True,2,0,False)]
  , mapWith (testFn0 & prevElt) "abc"  == [Nothing, Just 'a', Just 'b']
  , mapWith (testFn0 & nextElt) "abc"  == [Just 'b', Just 'c', Nothing]
  , andFirstLast "abc"                  == [('a',True,False),('b',False,False),('c',False,True)]
  , andFirstLast "a"                    == [('a',True,True)]
  , andFirstLast ""                     == []
  , take 3 (andFirstLast [1..])         == [(1,True,False),(2,False,False),(3,False,False)]
  , andFirstLast (FunnySet 8 9 1 2 5)   == FunnySet (8,True,False) (9,False,False) (1,False,False) (2,False,False) (5,False,True)
  , mapWith (testFn0 <-^ eltFromMay [1,2]) [1,2,3]
                                        == [Nothing, Just 2, Just 1]
  , mapWith (testFn0 <-^ eltFromDef 7 [1,2]) [1,2,3]
                                        == [7, 2, 1]
  , mapWith (testFn0 ^-> evenElt) "abcdef" == [True,False,True,False,True,False]
  , mapWith (testFn0 <-^ evenElt) "abcdef" == [False,True,False,True,False,True]
  , mapWith (testFn0 ^-> foldl1Elts (-)   ) [9, 1, 8] == [ 9,  8, 0]
  , mapWith (testFn0 <-^ foldl1Elts (-)   ) [9, 1, 8] == [-2,  7, 8]
  , mapWith (testFn0 ^-> foldlElts  (-) 20) [9, 1, 8] == [11, 10, 2] 
  , mapWith ((,,,) ^-> adj2Elts & isLast) "fred" ==[('f',Nothing,Nothing,False),('r',Just 'f',Nothing,False),('e',Just 'r',Just 'f',False),('d',Just 'e',Just 'r',True)]
  , mapWith (testFn0 ^-> sillyInj) [4, 5, 6] == [7,10,13]
--, length (scoreWeek [1,2..168]) == 168  --hangs in GHC 8.4.3 (per https://gitlab.haskell.org/ghc/ghc/-/issues/16943)
  , length (mapWeek [1,2..168]) == 168    --I don't think we have quite the same situation, and I think I've tested lots of infinite list cases already, but trying to be safe.
  ]

main | and tests = exitSuccess
     | otherwise = exitFailure

scoreWeek :: [Int] -> [[Int]]
scoreWeek xs = take 168 $ scanr (:) [] $ cycle xs

mapWeek :: [Int] -> [Int]
mapWeek xs = take 168 $ mapWith (testFn0 ^-> eltIx) $ cycle xs
