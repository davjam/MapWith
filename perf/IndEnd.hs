import MapWith
import Data.Maybe (fromMaybe)

main = do
  print $ sum $ mapWith ((+) <-^ ixIt) [1..1000000]

