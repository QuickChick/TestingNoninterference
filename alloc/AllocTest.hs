module AllocTest where

import CollectTests
import SinglecoreAlloc hiding (main)
import MulticoreAlloc  hiding (main)

main :: IO ()
main = defaultTestingMain $ singlecoreAllocProps ++ multicoreAllocProps
