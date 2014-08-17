{-# LANGUAGE DeriveDataTypeable #-}
module Flags where

import System.Console.CmdArgs

data GenType = GenLLNI
             | GenSSNI
               deriving (Eq, Show, Read, Typeable, Data)

data QCMode = ModeQuickCheck
          | ModePrintTable
            deriving (Eq, Show, Read, Typeable, Data)

data CollectF = CollectInstrCode
              | CollectNothing
            deriving (Eq, Show, Read, Typeable, Data)

data Flags = Flags { mode :: QCMode
                   , strategy :: GenType 
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
                     , strategy = GenSSNI
                     , noSteps  = 2
                     , maxTests = 10000
                     , mutantNo = Nothing
                     , discardRatio = 5
                     , showCounters = False
                     , printLatex = False
                     , timeout = 10
                     , doShrink = False 
                     , collectF = CollectNothing }
