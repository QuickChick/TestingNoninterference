{-# LANGUAGE NamedFieldPuns #-}

{-
** CH: is there any way to stop QC after a certain amount of time passes?
- this would be more helpful for us than only stopping after a number of successful tests;
  especially in the presence of discards
- any way to do this without changing the QuickCheck test driver?
  because for the default driver the only arguments are maxSuccess and maxDiscardRatio
- we could also try to implement this by shutting down processes,
  but I think that's less desirable + shrinking also takes time;
  seems there are some hacks to avoid shrinking:
  http://stackoverflow.com/questions/9550172/how-can-i-prevent-quickcheck-from-catching-all-exceptions
  http://hackage.haskell.org/packages/archive/base/4.2.0.0/doc/html/System-Timeout.html
  http://hackage.haskell.org/packages/archive/QuickCheck/2.5.1.1/doc/html/src/Test-QuickCheck-Property.html#within
-}
{-# LANGUAGE DeriveDataTypeable #-}

module QuickCheckTimeout where

import System.Timeout (timeout)
import Control.Concurrent (threadDelay, myThreadId, forkIO, killThread)
import Control.Concurrent.Thread.Delay ( delay )

import Test.QuickCheck (quickCheck, quickCheckResult, within,
                        Property, Result(..))
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Control.Exception (bracket, throwTo )
import qualified Control.Exception as E ( Exception, handleJust )
-- import Control.Exception (Exception)

import Data.Typeable

-- use threadDelay to simulate a slow computation
prop_slow_plus_zero_right_identity :: Int -> Property
prop_slow_plus_zero_right_identity i = monadicIO $ do
  run (threadDelay (100000 * i))
  assert (i + 0 == i)

---------------------------------------------------------------------------
-- Attempt #1: using "within" from Test.QuickCheck
---------------------------------------------------------------------------

-- Problem #1:
-- QuickCheck still tries to shrink the input at the time of the timeout
runTestsWithin :: IO ()
runTestsWithin =
  quickCheck ((within 3000000) . prop_slow_plus_zero_right_identity)

-- Problem #2:
-- QuickCheck gives no way to distinguish timeout from failure
runTestsWithinResult :: IO ()
runTestsWithinResult = do
  r <- quickCheckResult ((within 3000000) . prop_slow_plus_zero_right_identity)
  case r of
    Failure { reason } -> putStrLn ("Reason = " ++ reason)
    _ -> return ()
--  putStrLn $ show (reason r) -- r is Failure, but it should be a timeout / GaveUp

---------------------------------------------------------------------------
-- Attempt #2: using timeout from Haskell library
---------------------------------------------------------------------------

-- This doesn't work as expected because QC catches exceptions
-- Problem #1 and #2 are still there

runTestsTimeout :: IO ()
runTestsTimeout = do
  result <- timeout 3000000 (quickCheck prop_slow_plus_zero_right_identity)
  case result of
    Nothing -> putStrLn "timed out!"
    Just _  -> putStrLn "completed!"

runTestsTimeoutResult :: IO ()
runTestsTimeoutResult = do
  result <- timeout 3000000 (quickCheckResult prop_slow_plus_zero_right_identity)
  case result of
    Nothing -> putStrLn "timed out!"
    Just r  -> putStrLn $ show r -- r is Failure, but it should be a timeout / GaveUp

---------------------------------------------------------------------------
-- Attempt #3: using hand-made timeout' that throws UserInterrupt
---------------------------------------------------------------------------

-- This solves Problem #1 (shrinking is gone);
-- and for Problem #2 we just compare failure strings

data MyTimeout = MyTimeout Int
  deriving (Eq, Typeable)

instance Show MyTimeout where
  show tm = "internal timeout"

instance E.Exception MyTimeout



timeout' :: Integer -> IO b -> IO (Maybe b)
timeout' n f = do
    pid <- myThreadId
--    r <- newIORef (error :: b)
    do E.handleJust (\e -> if e == MyTimeout 666 then Just () else Nothing) -- Always propagate it! 
                    (\() -> return Nothing) -- Return nothing, we will the results from the state
            (bracket (forkIO (delay n >> throwTo pid (MyTimeout 666))) -- UserInterrupt
                     (killThread)
                     (\_ -> fmap Just f))


-- Like this QuickCheck doesn't perform the shrink steps
-- but somehow we still don't get to the "end1" output
-- runTestsTimeout' :: IO ()
-- runTestsTimeout' = do
--   putStrLn "start"  
--   E.catch (timeout' 3000000 (quickCheck prop_slow_plus_zero_right_identity))
--     (\e -> putStrLn $ "end1" ++ show (e :: AsyncException))
--            -- WFT? why doesn't this trigger???
--   putStrLn "end2"

runTestsTimeout'Result :: IO ()
runTestsTimeout'Result = do
  putStrLn "start"
  r <- timeout' 3000000 (quickCheckResult prop_slow_plus_zero_right_identity)
  --putStrLn $ show r -- r is Failure, but it should be a timeout / GaveUp
  case r of
    Nothing -> putStrLn "timeout!"
    _ -> undefined 
    -- Failure {reason} | reason == "Exception: 'internal timeout'"
    --                    -> putStrLn "timeout!"
    -- _ -> undefined
  return ()
