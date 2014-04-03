module Pretty where

import Text.PrettyPrint

class Pretty a where
  pretty     :: a -> Doc
  prettyPrec :: Int -> a -> Doc

  pretty = prettyPrec 0
  prettyPrec _ = pretty

listlike :: Doc -> Doc -> Doc -> [Doc] -> Doc
listlike lpar _ rpar [] = lpar <> rpar
listlike lpar mid rpar xs =
  fcat (zipWith (<+>) (lpar : repeat mid) xs) <+> rpar

list, record :: [Doc] -> Doc
list   = listlike (text "[") comma (text "]")
record = listlike (text "{") comma (text "}")
