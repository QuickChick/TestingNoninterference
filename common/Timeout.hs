{-# LANGUAGE DeriveDataTypeable #-}

module Timeout where

import Data.Typeable
import Control.Concurrent (myThreadId, forkIO, killThread)
import Control.Concurrent.Thread.Delay ( delay )
import Control.Exception (bracket, throwTo )
import qualified Control.Exception as E ( Exception, handleJust )

-- For other attempts to do this, and further comments please see:
-- old-code/tmu/QuickCheckTimeout.hs

data MyTimeout = MyTimeout Int
  deriving (Eq, Typeable)

instance Show MyTimeout where
  show tm = "internal timeout"

instance E.Exception MyTimeout

timeout' :: Integer -> IO b -> IO (Maybe b)
timeout' n f = do
    pid <- myThreadId
    do E.handleJust (\e -> if e == MyTimeout 666 then Just () else Nothing) -- Always propagate it! 
                    (\() -> return Nothing) -- Return nothing, we will the results from the state
            (bracket (forkIO (delay n >> throwTo pid (MyTimeout 666))) -- UserInterrupt
                     (killThread)
                     (\_ -> fmap Just f))

