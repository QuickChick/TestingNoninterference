module Driver where

import Test.QuickCheck

import SingleStateArb()
import SanityChecks
import Generation()
import SSNI
import Rules
import Mutate

import Control.Monad

foundBug :: Result -> Bool
foundBug Failure{} = True
foundBug _         = False

message :: Bool -> Int -> Int -> String
message kill n1 n2 = 
    (if kill then "Killed" else "Missed") ++ 
    " mutant " ++ (if kill then "" else "[") ++ show n2
               ++ (if kill then "" else "]") 
    ++ " (" ++ show n1 ++ " frags)"

checkMutants :: IO ()
checkMutants = do 
  let mutants = mutateTable defaultTable
  putStrLn $ "Fighting " ++ show (length mutants) ++ " mutants"
  _ <- foldM (\(n1,n2) t -> do
           res <- quickCheckWithResult stdArgs{maxSuccess = 100000, chatty = False} 
                   (propSSNI t .&&. propPreservesWellFormed t)
           let n1' = n1 + (if foundBug res then 1 else 0)
               msg = message (foundBug res) n1' n2
           putStrLn msg
           return (n1', n2+1)
        ) (0,0) mutants
  return ()
               
quickCheckN :: Int -> Property -> IO ()
quickCheckN n = quickCheckWith stdArgs{maxSuccess = n}

main :: IO ()
main = do
    putStrLn "Checking defaultTable"
    quickCheckN 10000 (propSSNI defaultTable .&&. 
                        propPreservesWellFormed defaultTable)
    putStrLn "Checking Mutants"
    checkMutants

--main = quickCheckN 100000 propStampsWellFormed


