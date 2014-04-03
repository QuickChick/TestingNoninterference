module Aggregate (aggregate, aggregateResults) where

import Test.QuickCheck
import Test.QuickCheck.Text
import Data.IORef
import System.IO.Unsafe

{-# NOINLINE aggrRef #-}
aggrRef :: IORef [String]
aggrRef = unsafePerformIO $ newIORef []

aggregate :: (Show a, Testable prop) => [a] -> prop -> Property
aggregate xs p = unsafePerformIO $ do
  modifyIORef aggrRef (map show xs ++)
  return $ property p

aggregateResults :: IO ()
aggregateResults = do
  xs <- readIORef aggrRef
  if null xs then return () else
    quickCheckWith stdArgs{maxSuccess = length xs} $
      ioProperty $ do
        x:xs <- readIORef aggrRef
        writeIORef aggrRef xs
        return $ collect (MkStr x) True
