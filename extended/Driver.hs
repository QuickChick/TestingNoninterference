{-# LANGUAGE RecordWildCards #-}
module Driver where

import Test.QuickCheck

import Primitives
import SingleStateArb()
import Shrinking
import SanityChecks
import Generation
import SSNI
import LLNI
import Rules
import Mutate
import Flags

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

checkMutants :: Flags -> IO ()
checkMutants flags = do 
  let mutants = mutateTable defaultTable
  putStrLn $ "Fighting " ++ show (length mutants) ++ " mutants"
  _ <- foldM (\(n1,n2) t -> do
           res <- quickCheckWithResult stdArgs{maxSuccess = 100000, chatty = False} 
                  $ mkProperty flags t
           let n1' = n1 + (if foundBug res then 1 else 0)
               msg = message (foundBug res) n1' n2
           putStrLn msg
           return (n1', n2+1)
        ) (0,0) mutants
  return ()

mkProperty :: Flags -> RuleTable -> Property
mkProperty f@Flags{..} t = 
    case strategy of
      GenSSNI -> 
          forAll (genVariationState f) $ \v@(Var _ st _) -> 
              propSSNI f t v .&&. propPreservesWellFormed t st
      GenLLNI -> 
          forAll (genVariationState f) $ \v@(Var _ st _) ->
              propLLNI f t v .&&. propPreservesWellFormed t st

ssniConfig :: Flags 
ssniConfig = Flags GenSSNI 2
llniConfig :: Flags
llniConfig = Flags GenLLNI 42

quickCheckN :: Int -> Property -> IO ()
quickCheckN n = quickCheckWith stdArgs{maxSuccess = n}

--ssniConfig = (Flags GenSSNI 2, propSSNI, 

--main = do 
  
main :: IO ()
main = do
    putStrLn "Checking defaultTable: SSNI"
    quickCheckN 10000 $ mkProperty ssniConfig defaultTable
    putStrLn "Checking defaultTable: LLNI"
    quickCheckN 10000 $ mkProperty llniConfig defaultTable
    putStrLn "Checking Mutants with SSNI"
    checkMutants ssniConfig
    putStrLn "Checking Mutants with LLNI"
    checkMutants llniConfig
