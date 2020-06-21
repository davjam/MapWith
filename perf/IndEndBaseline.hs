import MapWith

main = do
  print $ sum $ zipWith (+) [1..1000000] [999999,999998..0]
