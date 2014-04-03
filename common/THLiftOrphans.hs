-- This module is used by CollectTests; it contains the necessary (orphan)
-- `Lift` instances to lift `Name`s and `Type`s from the Haskell value level to
-- TH terms.

{-# LANGUAGE TemplateHaskell, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module THLiftOrphans where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.Exts (Int(I#), Int#)
import THAutoLift

simpleDeriveLifts [ ''OccName, ''ModName, ''NameSpace, ''PkgName, ''Name
                  , ''Kind, ''TyVarBndr, ''Pred, ''Type ]

-- Since Int# has kind #, not *, it can't be lifted automatically, as it can't
-- be a type class instance.  Thus, NameFlavour needs special handling.  (Oy.
-- Why not use !Int and -funbox-strict-fields?  Ah well.)

liftInt# :: Int# -> ExpQ
liftInt# i# = litE . intPrimL . fromIntegral $ I# i#

instance Lift NameFlavour where
  lift NameS               = [|NameS|]
  lift (NameQ modn)        = [|NameQ modn|]
  lift (NameU i)           = [|NameU $(liftInt# i)|]
  lift (NameL i)           = [|NameL $(liftInt# i)|]
  lift (NameG ns pkg modn) = [|NameG ns pkg modn|]
