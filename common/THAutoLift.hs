-- This module is used (indirectly) by CollectTests; it automatically derives
-- instances of `Lift` for simple types.

{-# LANGUAGE TemplateHaskell #-}
module THAutoLift where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Internals
import Control.Monad
import Data.List

simpleDeriveLift1 :: Name -> DecQ
simpleDeriveLift1 tyName = do
  tyInfo <- reify tyName
  case tyInfo of
    TyConI (DataD    [] _ [] cons []) -> liftInstance cons
    TyConI (NewtypeD [] _ [] con  []) -> liftInstance [con]
    _                                 -> die
  where liftInstance cons = instanceD (return [])
                                      (conT ''Lift `appT` conT tyName)
                                      [funD 'lift $ map liftCon cons] --'
        
        liftCon (NormalC conN args) = buildClause conN $ length args
        liftCon (RecC    conN args) = buildClause conN $ length args
        liftCon (InfixC  _ conN _)  = buildClause conN 2
        liftCon (ForallC _ _ _)     = die
        
        buildClause conN k = do
          vars  <- replicateM k $ newName "x"
          conNE <- case conN of
                     Name (OccName name)
                          (NameG DataName (PkgName pkg) (ModName modn)) ->
                       [| Name (OccName name)
                               (NameG DataName (PkgName pkg) (ModName modn)) |]
                     _ -> die
          return $ Clause
            [ConP conN $ map VarP vars]
            ( NormalB . foldl' (\l r -> InfixE (Just l) (VarE 'appE) (Just r)) --'
                               (AppE (VarE 'conE) conNE) --'
                      $ map (AppE (VarE 'lift) . VarE) vars) --'
            []
        
        die :: Q a
        die = fail $ "Could not create a simple `instance Lift "
                   ++ show tyName ++ "'."

simpleDeriveLifts :: [Name] -> DecsQ
simpleDeriveLifts = mapM simpleDeriveLift1

simpleDeriveLift :: Name -> DecsQ
simpleDeriveLift = simpleDeriveLifts . (:[])
