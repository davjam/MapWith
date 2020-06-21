import Data.Traversable (mapAccumL, mapAccumR)
import Data.Maybe (fromMaybe)

main = do
  print $ sum $ withPrevNext xxx [1..1000000]
  where
  xxx :: Int -> Maybe Int -> Maybe Int -> Int
  xxx n prevMay nextMay = n + fromMaybe 0 prevMay + fromMaybe 0 nextMay

withPrevNext :: Traversable t => (a -> Maybe a -> Maybe a -> b) -> t a -> t b
withPrevNext f = snd . mapAccumR accR Nothing . snd . mapAccumL accL Nothing
  where
  accL prevMay a = (Just a, (a, f a prevMay))
  accR nextMay (a, fap) = (Just a, fap nextMay)
  
  
