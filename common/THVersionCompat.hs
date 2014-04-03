-- This module is used by CollectTests, and handles a difference in API between
-- TH < 2.7 and TH > 2.7 (for looking up information about class instances).

{-# LANGUAGE TemplateHaskell #-}
module THVersionCompat where

import Language.Haskell.TH

extractInstanceType :: ExpQ
extractInstanceType = recover new old
  where old =  reify (mkName "ci_tys")
            >> [| \inst -> case $(varE $ mkName "ci_tys") inst of
                    [ty] -> Just ty
                    _    -> Nothing |]
        new =  [| \inst -> case inst of
                    InstanceD _ (AppT (ConT _) ty) _ -> Just ty
                    _                                -> Nothing |]
