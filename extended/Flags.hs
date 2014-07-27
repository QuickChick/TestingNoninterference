module Flags where

data GenType = GenLLNI
             | GenSSNI

data Flags = Flags { strategy :: GenType 
                   , noSteps  :: Int
                   , maxTests :: Int
                   , discardRatio :: Int
                   , showCounters :: Bool
                   , printLatex   :: Bool 
                   , timeout      :: Int 
                   , doShrink     :: Bool }

defaultFlags :: Flags
defaultFlags = Flags { strategy = GenSSNI
                     , noSteps  = 2
                     , maxTests = 10000
                     , discardRatio = 5
                     , showCounters = True
                     , printLatex = False
                     , timeout = 10
                     , doShrink = False }