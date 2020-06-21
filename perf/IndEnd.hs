

import MapWith

main = do
  print $ sum $ withEndIx xxx [1..1000000]
  where
  xxx n nEndInd = n + nEndInd

withEndIx :: Traversable t => (a -> Int -> b) -> t a -> t b
withEndIx f = mapWith $ f <-^ ixIt
