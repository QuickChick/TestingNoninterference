module LaTeX where

import Data.List

class LaTeX a where
  toLaTeX :: a -> String

instance LaTeX Bool where
  toLaTeX True  = "\\True"
  toLaTeX False = "\\False"

instance LaTeX Int where
  toLaTeX = show

instance LaTeX a => LaTeX [a] where
  toLaTeX xs = "\\left[" ++ body ++ "\\right]"
    where body | null xs   = "\\,"
               | otherwise = intercalate "," $ map toLaTeX xs

putLaTeX :: LaTeX a => a -> IO ()
putLaTeX = putStr . toLaTeX
