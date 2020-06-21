{-# LANGUAGE BangPatterns #-}

import Data.Traversable (mapAccumR)

main = do
  print $ sum $ withEndIx xxx [1..1000000]
  where
  xxx n nEndInd = n + nEndInd

withEndIx :: Traversable t => (a -> Int -> b) -> t a -> t b
withEndIx f !t = snd $ mapAccumR acc 0 t
  where
  acc !i a = (i+1, f a i)
