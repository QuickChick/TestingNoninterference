module Trace where

import Text.PrettyPrint
import Pretty

data Trace s = Trace [s]

instance Pretty s => Pretty (Trace s) where
  pretty (Trace (s:ss)) =  text ""
                        $$ nest 4 (pretty s)
                        $$ foldr ($$) (text "STOP") [text "-->" <+> pretty s' | s' <- ss]
                        $$ text ""
  pretty (Trace []) = text "" $$ nest 4 (text "STOPPED") $$ text ""

instance Pretty s => Show (Trace s) where
  show = show . pretty
