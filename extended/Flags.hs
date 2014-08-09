{-# LANGUAGE DeriveDataTypeable #-}
module Flags where

import System.Console.CmdArgs

data GenType = GenLLNI
             | GenSSNI
               deriving (Eq, Show, Read, Typeable, Data)

{-
data Mode = ModeQuickCheck
          | ModePrintTable
            deriving (Eq, Show, Read, Typeable, Data)
-}

data Flags = Flags { strategy :: GenType 
                   , noSteps  :: Int
                   , maxTests :: Int
                   , discardRatio :: Int
                   , showCounters :: Bool
                   , printLatex   :: Bool 
                   , timeout      :: Int 
                   , doShrink     :: Bool }
           deriving (Eq, Show, Read, Typeable, Data)

defaultFlags :: Flags
defaultFlags = Flags { strategy = GenSSNI
                     , noSteps  = 2
                     , maxTests = 10000
                     , discardRatio = 5
                     , showCounters = False
                     , printLatex = False
                     , timeout = 10
                     , doShrink = False }