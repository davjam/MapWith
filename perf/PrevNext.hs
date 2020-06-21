import MapWith
import Data.Maybe (fromMaybe)

main = do
  print $ sum $ withPrevNext xxx [1..1000000]
  where
  xxx :: Int -> Maybe Int -> Maybe Int -> Int
  xxx n prevMay nextMay = n + fromMaybe 0 prevMay + fromMaybe 0 nextMay
