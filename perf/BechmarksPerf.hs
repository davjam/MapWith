module Main (main)
where

import Benchmarks
import System.Environment

main = do
  args <- getArgs --e.g. ["1000000 7"]
  let [n, testFnId] = map read $ words $ head args
  print $ sum $ testFn testFnId n
