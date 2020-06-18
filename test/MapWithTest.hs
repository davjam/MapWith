import MapWith
import System.Exit

testFn0 :: a -> Bool -> Bool
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

r0 = mapWith (testFn0 $-> limIt) fbb
r2 = mapWith (testFn  <-$ limIt <-* ixIt) fbb
r3 = mapWith (testFn  <-$ limIt *-> ixIt) fbb
r4 = mapWith (testFn2 $-> limIt *-> ixIt <-* zipIt [8,9,10,11]) fbb

fbb = ["foo", "bar", "baz"]

main = do
  if tagFirstLast "abc" == [('a',True,False),('b',False,False),('c',False,True)]
  then exitSuccess
  else exitFailure

