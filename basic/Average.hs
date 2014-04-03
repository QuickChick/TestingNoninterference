module Average where

import Data.IORef
import System.IO.Unsafe


recordedData2 :: IORef [(Int,Int)]
recordedData2 = unsafePerformIO (newIORef [])

recordedData2' :: IORef [(Int,Int)]
recordedData2' = unsafePerformIO (newIORef [])


recordedData :: IORef [Int]
recordedData = unsafePerformIO (newIORef [])

record :: Int -> a -> a
record n a = unsafePerformIO $ do
  modifyIORef recordedData (n:)
  return a

record2 :: (Int,Int) -> a -> a
record2 n a = unsafePerformIO $ do
  modifyIORef recordedData2 (n:)
  return a

record2' :: (Int,Int) -> a -> a
record2' n a = unsafePerformIO $ do
  modifyIORef recordedData2' (n:)
  return a

clear :: IO ()
clear = writeIORef recordedData []

clear2 :: IO ()
clear2 = writeIORef recordedData2 []

clear2' :: IO ()
clear2' = writeIORef recordedData2' []

average :: IO Double
average = do
  ns <- readIORef recordedData
  return (sum (map fromIntegral ns) / fromIntegral (length ns))


average2 :: IO (Double, Double)
average2 = do
  ns <- readIORef recordedData2
  let n1 = sum (map (fromIntegral . fst) ns)
      n2 = sum (map (fromIntegral . snd) ns)
  return (n1 / fromIntegral (length ns), n2 / fromIntegral (length ns)) 


average2' :: IO (Double, Double)
average2' = do
  ns <- readIORef recordedData2'
  let n1 = sum (map (fromIntegral . fst) ns)
      n2 = sum (map (fromIntegral . snd) ns)
  return (n1 / fromIntegral (length ns), n2 / fromIntegral (length ns)) 


