module Flags where

data GenType = GenLLNI
             | GenSSNI
data Flags = Flags { strategy :: GenType 
                   , noSteps  :: Int }
