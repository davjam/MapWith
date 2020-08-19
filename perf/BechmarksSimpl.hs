-- ddump-simpl of BenchmarksPerf is ugly (all the IO read stuff is muddled in with the mapWith code).
-- This unmuddles it.

module Main (main)
where

import Benchmarks

main = print $ sum $ testFn 19 1000000
