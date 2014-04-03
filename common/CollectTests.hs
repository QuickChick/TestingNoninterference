-- This module provides a hackish way to automatically collect and run all
-- QuickCheck properties in a module, provided they are named `prop_whatever`
-- and are defined at the start of a line.
-- 
-- To deal with polymorphic properties, we allow the user to specify, for any
-- single-variable class constraints in the context, what types they should be
-- instantiated at.  For example, one could specify
-- `(''Eq, Some [[t|Bool|], [t|Char|])`,
-- and then any `prop_foo :: Eq a => a -> Bool` would be tested at
-- `prop_foo :: Integer -> Bool` and `prop_foo :: Char -> Bool`.  One could also
-- specify `(''Eq, All)`, and then every monomorphic instance in scope would be
-- used; use this with caution, however, and probably only for type classes you
-- have written yourself.  Note that we use equality constraints to
-- monomorphize, so you must specify `-XTypeFamilies` or `-XGADTs`.  It is the
-- need for this feature which requires us to roll our own code, instead of
-- using the Test.QuickCheck.All module.
-- 
-- An example of using this module:
-- 
--     {-# LANGUAGE TemplateHaskell, TypeFamilies #-}
--     {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
--     module Symmetry where
--     
--     import Test.QuickCheck
--     import CollectTests
--     
--     class Semigroup a where
--       (<>) :: a -> a -> a
--     
--     instance Semigroup Bool   where (<>) = (&&)
--     instance Semigroup Int    where (<>) = (+)
--     instance Semigroup String where (<>) = (++)
--     
--     prop_le_antisym :: Ord a => a -> a -> Bool
--     prop_le_antisym x y = not ((x <= y) && (y <= x)) || x == y
--     
--     prop_semigroup_sym :: (Eq a, Semigroup a) => a -> a -> Bool
--     prop_semigroup_sym x y = x <> y == y <> x
--     
--     prop_plus_sym :: (Eq a, Num a) => a -> a -> Bool
--     prop_plus_sym x y = x + y == y + x
--     
--     symmetryProps :: [(Name,[(Type,Property)])]
--     symmetryProps = $(moduleProps [t|Integer|]
--                                   [ (''Ord,       Some [ [t|Double|]
--                                                        , [t|Char|] ])
--                                   , (''Semigroup, All) ])
--     
--     main :: IO ()
--     main = defaultTestingMain symmetryProps
-- 
-- Then, running (from another module or GHCi) the `quickCheckProps` or
-- `quickCheckPropsWith` function produces the following results:
-- 
--     *Symmetry> quickCheckProps symmetryProps 
--     Symmetry.prop_le_antisym :: forall a_0 . (GHC.Classes.Ord a_0,
--                                               a_0 ~ GHC.Types.Double) =>
--                                              a_0 -> a_0 -> GHC.Types.Bool
--     +++ OK, passed 100 tests.
--     
--     Symmetry.prop_le_antisym :: forall a_0 . (GHC.Classes.Ord a_0,
--                                               a_0 ~ GHC.Types.Char) =>
--                                              a_0 -> a_0 -> GHC.Types.Bool
--     +++ OK, passed 100 tests.
--     
--     Symmetry.prop_semigroup_sym :: forall a_0 . (GHC.Classes.Eq a_0,
--                                                  Symmetry.Semigroup a_0,
--                                                  a_0 ~ GHC.Types.Bool) =>
--                                                 a_0 -> a_0 -> GHC.Types.Bool
--     +++ OK, passed 100 tests.
--     
--     Symmetry.prop_semigroup_sym :: forall a_0 . (GHC.Classes.Eq a_0,
--                                                  Symmetry.Semigroup a_0,
--                                                  a_0 ~ GHC.Types.Int) =>
--                                                 a_0 -> a_0 -> GHC.Types.Bool
--     +++ OK, passed 100 tests.
--     
--     Symmetry.prop_semigroup_sym :: forall a_0 . (GHC.Classes.Eq a_0,
--                                                  Symmetry.Semigroup a_0,
--                                                  a_0 ~ GHC.Base.String) =>
--                                                 a_0 -> a_0 -> GHC.Types.Bool
--     *** Failed! Falsifiable (after 2 tests and 2 shrinks):    
--     "a"
--     "b"
--     
--     Symmetry.prop_plus_sym :: forall a_0 . (GHC.Classes.Eq a_0,
--                                             GHC.Num.Num a_0,
--                                             a_0 ~ GHC.Integer.Type.Integer) =>
--                                            a_0 -> a_0 -> GHC.Types.Bool
--     +++ OK, passed 100 tests.
--     
--     Ran 6 tests, passed 5 (83.33%).
--     Failed 1 test:
--       Symmetry.prop_semigroup_sym :: forall a_0 . (GHC.Classes.Eq a_0,
--                                                    Symmetry.Semigroup a_0,
--                                                    a_0 ~ GHC.Base.String) =>
--                                                   a_0 -> a_0 -> GHC.Types.Bool
--     False
-- 
-- Note that this output (a) instantiates `Ord` to the appropriate two types for
-- `prop_le_antisym`; (b) uses all three defined instances for `Semigroup`; and
-- (c) uses the default choice of `Integer` to instantiate `prop_plus_sym`, as
-- no monomorphization information was specified for `Num` or `Eq`.  The final
-- line is the return value: `True` if all tests passed, and `False` otherwise.
-- 
-- Similarly, if the above module is compiled into an executable named
-- `symmetry`, then `./symmetry` will run the same tests with some hopefully-
-- intelligent default settings (including a setting to run more tests than the
-- default 100; this number can be adjusted from the command line).  The output
-- format is the same.  The executable will exit with a nonzero exit status if
-- some of the tests failed (or were given up on, etc.), and a zero (successful)
-- exit status if they all succeeded. 
-- 
-- The reason that this module is hackish is that (in addition to only handling
-- simple class constraints for monomorphization and ignoring the rest), the
-- only way to get a list of in-scope names is to read in the file, look for
-- lines which begin with with `prop_`, and treat the names thus found as names
-- in the current module.  Thus, the properties to be automatically tested
-- *must* come at the start of lines and *must* be named in the right format---
-- and *nothing else* must be named that way.

{-# LANGUAGE TemplateHaskell, TupleSections, RecordWildCards #-}
module CollectTests ( tyVarBndrName
                    , monomorphizeWith, monomorphizeNameWith
                    , getPropsInFile, modulePropNames
                    , Instances(..), instancesTypes
                    , moduleProps, processPropsWith
                    , showTypedName
                    , quickCheckPropsWith, quickCheckProps, defaultTestingMain
                    , Name, Type ) where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Maybe
import Data.List
import Data.Ord
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import THVersionCompat
import THLiftOrphans () -- Instances
import System.IO
import Data.Char
import Test.QuickCheck
import Data.IORef
import Util
import Text.Printf
import System.Environment
import System.Exit

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV  name)   = name
tyVarBndrName (KindedTV name _) = name

monomorphizeWith :: (Name -> Maybe [Type]) -> Type -> Type -> [Type]
monomorphizeWith maybeConcretize defaultType = go
  where
    concretizePred (ClassP cl [VarT tv]) = (tv,) <$> maybeConcretize cl
    concretizePred _                     = Nothing
    
    intersect' [] = []
    intersect' xs = foldr1 intersect xs
    
    go (ForallT binders cxt ty) =
      let cxtEqs    = map (\pairs@((tv,_):_) -> (tv, intersect' $ map snd pairs))
                    . groupBy ((==) `on` fst)
                    . sortBy (comparing fst)
                    $ mapMaybe concretizePred cxt
          nonCxtEqs = map (,[defaultType])
                    $ map tyVarBndrName binders \\ map fst cxtEqs
          eqs       = mapM (\(tv,tys) -> map (EqualP $ VarT tv) tys)
                    $ cxtEqs ++ nonCxtEqs
      in ForallT binders . (cxt ++) <$> eqs <*> go ty
    go (AppT ty1 ty2) = AppT <$> go ty1 <*> go ty2
    go (SigT ty k)    = SigT <$> go ty <*> pure k
    go ty             = pure ty


monomorphizeNameWith :: (Name -> Maybe [Type]) -> Type -> Name -> Q [Exp]
monomorphizeNameWith mayConcr defTy var =
  do info <- reify var
     case info of
       VarI _ ty _ _ -> case monomorphizeWith mayConcr defTy ty of
                          [] -> fail $  "Could not monomorphize the name `"
                                     ++ show var ++ "'."
                          tys -> return $ map (SigE $ VarE var) tys
       _             -> fail $  "The name `" ++ show var
                             ++ "' is not a bound variable."

getPropsInFile :: FilePath -> IO [String]
getPropsInFile hs = map (takeWhile $ not . isSpace)
                  . filter ("prop_" `isPrefixOf`)
                  . lines <$> readFile hs

modulePropNames :: Q [Name]
modulePropNames = do
  Loc{..} <- location
  props   <- runIO $ getPropsInFile loc_filename
  return . nub $ map (mkNameG_v loc_package loc_module) props

data Instances = Some [TypeQ] | All

instancesTypes :: Name -> Instances -> Q [Type]
instancesTypes _   (Some tys) = sequence tys
instancesTypes cls All        = do
  clsInfo <- reify cls
  return $ case clsInfo of
    ClassI _ clis -> filter (isConcrete []) $ mapMaybe $extractInstanceType clis
    _             -> []
  where
    isConcrete bound (ForallT bndrs _ ty) = isConcrete
                                              (map tyVarBndrName bndrs ++ bound)
                                              ty
    isConcrete bound (VarT var)           = var `elem` bound
    isConcrete bound (AppT ty1 ty2)       = ((&&) `on` isConcrete bound) ty1 ty2
    isConcrete bound (SigT ty _)          = isConcrete bound ty
    isConcrete bound _                    = True


-- Returns something of type [(Name,[(Type,Property)])]
moduleProps :: TypeQ -> [(Name,Instances)] -> ExpQ
moduleProps defaultTypeQ concreteTypesQ = do
  defaultType   <- defaultTypeQ
  concreteTypes <- mapM (\(name,insts) -> (name,) <$> instancesTypes name insts)
                        concreteTypesQ
  names         <- modulePropNames
  listE . flip map names $ \name ->
    [| ( name
       , $(  listE . map withTypeProp
         =<< monomorphizeNameWith (`lookup` concreteTypes)
                                  defaultType
                                  name) ) |]
  where
    withTypeProp expr@(SigE _ ty) = [|(ty,property $(return expr))|]
    withTypeProp _ = fail $  "[Internal error] "
                          ++ "Malformed `monomorphizeNameWith' result."

processPropsWith :: Monad m
                 => (Name -> Type -> Property -> m ())
                 -> [(Name,[(Type,Property)])]
                 -> m ()
processPropsWith action = mapM_ . uncurry $ mapM_ . uncurry . action

showTypedName :: Name -> Type -> String
showTypedName name ty = pprint $ SigE (VarE name) ty

quickCheckPropsWith :: Args -> [(Name,[(Type,Property)])] -> IO Bool
quickCheckPropsWith args ntprops =
  do successesRef          <- newIORef []
     gaveUpsRef            <- newIORef []
     failuresRef           <- newIORef []
     noExpectedFailuresRef <- newIORef []
     
     flip processPropsWith ntprops $ \name ty prop -> do
       putStrLn $ showTypedName name ty
       res <- quickCheckWithResult args prop
       putStrLn ""
       let whichRef = case res of
                        Success{}           -> successesRef
                        Failure{}           -> failuresRef
                        GaveUp{}            -> gaveUpsRef
                        NoExpectedFailure{} -> noExpectedFailuresRef
       modifyIORef' whichRef ((name,ty) :)
     
     mapM_ (flip modifyIORef' reverse)
           [successesRef,gaveUpsRef,failuresRef,noExpectedFailuresRef]
     
     successes          <- readIORef successesRef
     gaveUps            <- readIORef gaveUpsRef
     failures           <- readIORef failuresRef
     noExpectedFailures <- readIORef noExpectedFailuresRef
     
     let numSuccesses          = length successes         
         numGaveUps            = length gaveUps           
         numFailures           = length failures          
         numNoExpectedFailures = length noExpectedFailures
         numTests              = numSuccesses + numGaveUps + numFailures
                               + numNoExpectedFailures
     
     let plural n word | abs n == 1 = show n ++ " " ++ word
                       | otherwise  = show n ++ " " ++ word ++ "s"
         
         reportErrors title []    = return ()
         reportErrors title tests = do
           putStrLn $ title ++ " " ++ plural (length tests) "test" ++ ":"
           mapM_ (mapM_ (putStrLn . ("  " ++)) . lines . uncurry showTypedName)
                 tests
     
     void $ printf "Ran %d tests, passed %d (%.2f%%).\n"
                   numTests
                   numSuccesses
                   ((100 :: Double) * ((/) `on` fromIntegral) numSuccesses numTests)
     reportErrors "Failed"                   failures
     reportErrors "Gave up on"               gaveUps
     reportErrors "No expected failures for" noExpectedFailures
     return $ numTests == numSuccesses

quickCheckProps :: [(Name,[(Type,Property)])] -> IO Bool
quickCheckProps = quickCheckPropsWith stdArgs

defaultTestingMain :: [(Name,[(Type,Property)])] -> IO ()
defaultTestingMain props = do
  args    <- getArgs
  n       <- fromMaybe 1000 <$> case args of
               [s] | all isDigit s -> return . Just $ read s
               [] -> return Nothing
               _  -> do hPutStrLn stderr "Warning: ignoring arguments."
                        return Nothing
  success <- quickCheckPropsWith stdArgs{maxSuccess = n, maxDiscardRatio = 2}
             props
  if success then exitSuccess else exitFailure
