{-# LANGUAGE DeriveDataTypeable #-}
module Flags where

import System.Console.CmdArgs

data GenType = GenByExec
             | GenTinySSNI
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
                     , strategy = GenTinySSNI
                     , testProp = TestSSNI
                     , noSteps  = 2
                     , maxTests = 10000
                     , mutantNo = Nothing
                     , discardRatio = 5
                     , showCounters = False
                     , printLatex = False
                     , timeout = 10
                     , doShrink = False 
                     , collectF = CollectNothing }

ssniConfig :: Flags -> Flags 
ssniConfig f = f { strategy = GenTinySSNI , testProp = TestSSNI }
llniConfig :: Flags -> Flags
llniConfig f = f { strategy = GenByExec , testProp = TestLLNI , noSteps = 42 }
msniConfig :: Flags -> Flags
msniConfig f = f { strategy = GenByExec , testProp = TestMSNI , noSteps = 42 }


