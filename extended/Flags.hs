{-# LANGUAGE DeriveDataTypeable #-}
module Flags where

import System.Console.CmdArgs

data MemType = MemList | MemMap
             deriving (Eq, Show, Read, Typeable, Data)

data GenInstrType = Naive | DiscardUniform
            deriving (Eq, Show, Read, Typeable, Data)

data GenType = GenByExec 
             | GenSSNI   
               deriving (Eq, Show, Read, Typeable, Data)

data TestProperty = TestLLNI
                  | TestSSNI
                  | TestMSNI
                  | TestEENI
            deriving (Eq, Show, Read, Typeable, Data)

data QCMode = ModeQuickCheck
            | ModePrintTable
            deriving (Eq, Show, Read, Typeable, Data)

data CollectF = CollectInstrCode
              | CollectNothing
            deriving (Eq, Show, Read, Typeable, Data)

data Flags = Flags { mode :: QCMode
                   , strategy :: GenType 
                   , genInstrDist :: GenInstrType
                   , memType :: MemType
                   , testProp :: TestProperty
                   , noSteps  :: Int
                   , maxTests :: Int
                   , mutantNo :: Maybe Int
                   , discardRatio :: Int
                   , showCounters :: Bool -- CH: this flag has confusing semantics
                     -- is it show counterexamples? or show the quickcheck counter?
                     -- currently it seems that the answer is: both
                   , printLatex   :: Bool
                   , timeout      :: Int 
                   , doShrink     :: Bool 
                   , collectF     :: CollectF }
           deriving (Eq, Show, Read, Typeable, Data)

defaultFlags :: Flags
defaultFlags = Flags { mode = ModeQuickCheck
                     , strategy = GenByExec
                     , genInstrDist = DiscardUniform
                     , memType = MemMap
                     , testProp = TestMSNI
                     , noSteps  = 42
                     , maxTests = 10000
                     , mutantNo = Nothing
                     , discardRatio = 10
                     , showCounters = False
                     , printLatex = False
                     , timeout = 10
                     , doShrink = False 
                     , collectF = CollectNothing }

naiveSsniConfig :: Flags -> Flags 
naiveSsniConfig f = f { strategy = GenSSNI 
                      , genInstrDist = Naive
                      , testProp = TestSSNI , noSteps = 2 }
ssniConfig :: Flags -> Flags 
ssniConfig f = (naiveSsniConfig f){ genInstrDist = DiscardUniform }
llniConfig :: Flags -> Flags
llniConfig f = f { strategy = GenByExec
                 , genInstrDist =  DiscardUniform 
                 , testProp = TestLLNI , noSteps = 42 }
naiveLlniConfig :: Flags -> Flags
naiveLlniConfig f = (llniConfig f) {genInstrDist = Naive}
naiveLLNIListConfig :: Flags -> Flags
naiveLLNIListConfig f = (naiveLlniConfig f) {memType = MemList}
msniConfig :: Flags -> Flags
msniConfig f = (llniConfig f) { genInstrDist = DiscardUniform, testProp = TestMSNI }
naiveMsniConfig :: Flags -> Flags 
naiveMsniConfig f = (msniConfig f) { genInstrDist = Naive }
eeniConfig :: Flags -> Flags
eeniConfig f = (llniConfig f) { genInstrDist = DiscardUniform, testProp = TestEENI }



