import MapWith
import Data.Monoid (Sum(..))

main = do
  print $ getSum $ foldMapWith (xxx ^-> foldl1Elts (+)) $ take 10000000 $ repeat (1 :: Int)
  where
  xxx _ s = Sum s
