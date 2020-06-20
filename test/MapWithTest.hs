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

r0 = mapWith (testFn0 ^-> limIt) fbb
r2 = mapWith (testFn  <-^ limIt <-^ ixIt) fbb
r3 = mapWith (testFn  <-^ limIt ^-> ixIt) fbb
r4 = mapWith (testFn2 ^-> limIt ^-> ixIt <-^ zipIt [8,9,10,11]) fbb

fbb = ["foo", "bar", "baz"]

tests :: [Bool]
tests =
  [
    mapWith (testFn0 ^-> limIt) "abc"   == [True,  False, False]
  , mapWith (testFn0 <-^ limIt) "abc"   == [False, False, True ]
  , mapWith ((\_ a b c d -> (a,b,c,d)) <-^ limIt ^-> ixIt <-^ ixIt ^-> limIt) "abc"
                                        == [(False,0,2,True),(False,1,1,False),(True,2,0,False)]
  , mapWith (testFn0 ^-> adjElt) "abc"  == [Nothing, Just 'a', Just 'b']
  , mapWith (testFn0 <-^ adjElt) "abc"  == [Just 'b', Just 'c', Nothing]
  , andFirstLast "abc"                  == [('a',True,False),('b',False,False),('c',False,True)]
  , take 3 (andFirstLast [1..])         == [(1,True,False),(2,False,False),(3,False,False)]
  ]

main = do
  if and tests
  then exitSuccess
  else exitFailure

