{-# LANGUAGE DeriveDataTypeable #-}
module Flags where

import System.Console.CmdArgs

data GenInstrType = Naive | DiscardUniform
            deriving (Eq, Show, Read, Typeable, Data)

data GenType = GenByExec 
             | GenSSNI   
               deriving (Eq, Show, Read, Typeable, Data)

data TestProperty = TestLLNI
                  | TestSSNI
                  | TestMSNI
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
                   , testProp :: TestProperty
                   , noSteps  :: Int
                   , maxTests :: Int
                   , mutantNo :: Maybe Int
                   , discardRatio :: Int
                   , showCounters :: Bool
                   , printLatex   :: Bool 
                   , timeout      :: Int 
                   , doShrink     :: Bool 
                   , collectF     :: CollectF }
           deriving (Eq, Show, Read, Typeable, Data)

defaultFlags :: Flags
defaultFlags = Flags { mode = ModeQuickCheck
                     , strategy = GenByExec
                     , genInstrDist = DiscardUniform
                     , testProp = TestMSNI
                     , noSteps  = 42
                     , maxTests = 10000
                     , mutantNo = Nothing
                     , discardRatio = 5
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
msniConfig :: Flags -> Flags
msniConfig f = (llniConfig f) { genInstrDist = DiscardUniform, testProp = TestMSNI }
naiveMsniConfig :: Flags -> Flags 
naiveMsniConfig f = (msniConfig f) { genInstrDist = Naive }


