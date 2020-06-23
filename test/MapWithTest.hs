{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import MapWith
import System.Exit

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

tests :: [Bool]
tests =
  [
    mapWith (testFn0 ^-> isLim) "abc"   == [True,  False, False]
  , mapWith (testFn0 <-^ isLim) "abc"   == [False, False, True ]
  , mapWith ((\_ a b c d -> (a,b,c,d)) <-^ isLim ^-> eltIx <-^ eltIx ^-> isLim) "abc"
                                        == [(False,0,2,True),(False,1,1,False),(True,2,0,False)]
  , mapWith (testFn0 ^-> adjElt) "abc"  == [Nothing, Just 'a', Just 'b']
  , mapWith (testFn0 <-^ adjElt) "abc"  == [Just 'b', Just 'c', Nothing]
  , andFirstLast "abc"                  == [('a',True,False),('b',False,False),('c',False,True)]
  , take 3 (andFirstLast [1..])         == [(1,True,False),(2,False,False),(3,False,False)]
  , andFirstLast (FunnySet 8 9 1 2 5)   == FunnySet (8,True,False) (9,False,False) (1,False,False) (2,False,False) (5,False,True)
  , mapWith (testFn0 <-^ eltFromMay [1,2]) [1,2,3]
                                        == [Nothing, Just 2, Just 1]
  , mapWith (testFn0 <-^ eltFromDef 7 [1,2]) [1,2,3]
                                        == [7, 2, 1]

  ]

main = do
  if and tests
  then exitSuccess
  else exitFailure

