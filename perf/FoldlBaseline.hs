{-# LANGUAGE BangPatterns #-}
import Data.Monoid (Sum(..))
import Data.Traversable (mapAccumL)
import Data.Foldable (fold)

main = do
  print $ getSum $ fold $ snd $ mapAccumL (\ssf n -> (ssf + n, Sum $ ssf + n)) 0 $ take 10000000 $ repeat (1 :: Int)
  where
  xxx n nEndInd = n + nEndInd

