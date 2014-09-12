{-# LANGUAGE RecordWildCards,DeriveDataTypeable #-}
module GenerateTablesMakefile where 

import Data.Char
import Data.List
import Control.Monad
import Flags

import Data.Typeable

import System.Console.CmdArgs hiding (name)

trim :: Show a => a -> String
trim = dropWhile isLower . drop 1 . show

data Parameters = Parameters { instrs   :: GenInstrs
                             , prop     :: PropTest
                             , equiv    :: Equiv
                             , bugs     :: [IfcSemantics]
                             , start    :: StartingAS
                             , strategy :: GenStrategy 
                             , smartints:: Bool }
                deriving (Eq, Read, Show)

data ParametersProf = ParametersProf { pstart     :: StartingAS
                                     , pstrategy  :: GenStrategy 
                                     , psmartints :: Bool }
                    deriving (Eq, Read, Show)

name :: Parameters -> String
name params = "picotable-" ++ name_short params

name_prof :: ParametersProf -> String
name_prof params = "prof-" ++ name_short_prof params

name_profvar :: ParametersProf -> String
name_profvar params = "profvar-" ++ name_short_prof params


name_short :: Parameters -> String
name_short Parameters{..} = intercalate "-" [ trim instrs
                                            , trim prop
                                            , trim equiv
                                            , trim start
                                            , trim strategy
                                            , show smartints ]

name_short_prof :: ParametersProf -> String
name_short_prof ParametersProf{..} = intercalate "-" [ trim pstart
                                                     , trim pstrategy
                                                     , show psmartints ]



commandLine :: Parameters -> String
commandLine Parameters{..} =
  unwords [ "$(MAKE)", "picotable"
          , var "INSTRS" $ trim instrs
          , var "BUGS"   . quote $ show bugs
          , var "PROP"   $ trim prop
          , var "EQUIV"  $ trim equiv
          , var "STRATEGY"  $ trim strategy
          , var "START"  $ trim start
          , var "SMART_INTS"  $ show smartints ]
  where quote = ("'" ++) . (++ "'")
        var name val = concat [name,"=",val]

commandLineProf :: ParametersProf -> String
commandLineProf ParametersProf{..} =
  unwords [ "$(MAKE)", "picoprofile"
          , var "STRATEGY"  $ trim pstrategy
          , var "START"  $ trim pstart
          , var "SMART_INTS"  $ show psmartints ]
  where quote = ("'" ++) . (++ "'")
        var name val = concat [name,"=",val]

commandLineProfVar :: ParametersProf -> String
commandLineProfVar ParametersProf{..} =
  unwords [ "$(MAKE)", "picoprofilevar"
          , var "STRATEGY"  $ trim pstrategy
          , var "START"  $ trim pstart
          , var "SMART_INTS"  $ show psmartints ]
  where quote = ("'" ++) . (++ "'")
        var name val = concat [name,"=",val]

instructions :: [GenInstrs]
instructions = [InstrsBasic, InstrsCally]
               -- Removed: InstrsFull

propsFor :: GenInstrs -> [PropTest]
propsFor InstrsBasic = [PropEENI, PropLLNI]
propsFor InstrsCally = [PropEENI, PropLLNI, PropSSNI]
propsFor _           = error "propsFor: not supported"

equivFor :: GenInstrs -> PropTest -> [Equiv]
equivFor InstrsBasic PropEENI = [EquivMem, EquivLow]
equivFor InstrsBasic PropLLNI = [EquivMem, EquivLow]
equivFor InstrsCally PropEENI = [EquivMem, EquivLow, EquivFull]
equivFor InstrsCally PropLLNI = [EquivLow, EquivFull]
equivFor InstrsCally PropSSNI = [EquivFull]
  -- EquivMem, EquivLow, and EquivWrongFull don't work for SSNI
equivFor _           _        = error "equivFor: not supported"

bugsFor :: GenInstrs -> [IfcSemantics]
bugsFor InstrsBasic = [ IfcBugArithNoTaint
                      , IfcBugPushNoTaint
                      , IfcBugLoadNoTaint
                      , IfcBugStoreNoPointerTaint
                      , IfcBugAllowWriteDownThroughHighPtr
                      , IfcBugStoreNoValueTaint ]
bugsFor InstrsCally = bugsFor InstrsBasic ++
                      [ IfcBugJumpNoRaisePc
                      , IfcBugJumpLowerPc
                      , IfcBugStoreNoPcTaint
                      , IfcBugAllowWriteDownWithHighPc
                      , IfcBugCallNoRaisePc
                      , IfcBugReturnNoTaint
                      , IfcBugValueOrVoidOnReturn 
                      , IfcBugPopPopsReturns ]

bugsFor InstrsJumpy = error "We are not supposed to call this, ever!" 


startsFor :: PropTest -> Equiv -> [StartingAS]
startsFor PropSSNI _ = [StartArbitrary]
startsFor _ equiv = [StartInitial | equiv /= EquivFull ] ++
                    [StartQuasiInitial | equiv == EquivLow] ++
                    [StartArbitrary | equiv == EquivFull]

strategiesFor :: PropTest -> GenInstrs -> StartingAS -> [GenStrategy]
strategiesFor prop instrs start
  = [ GenNaive, GenByExec ] ++
    [ GenWeighted | prop == PropEENI || prop == PropLLNI] ++
    [ GenSequence | prop == PropEENI || prop == PropLLNI] ++
    [ GenByFwdExec | instrs /= InstrsCally ] ++
    [ GenTinySSNI | prop == PropSSNI
                    && start == StartArbitrary ]
    
smartintsFor :: PropTest -> GenInstrs -> [Bool]
smartintsFor PropEENI InstrsBasic = [False, True]
smartintsFor _        _           = [True]

allParameters :: [Parameters]
allParameters = do instrs <- instructions
                   prop <- propsFor instrs
                   equiv <- equivFor instrs prop
                   let bugs =  bugsFor instrs
                   start    <- startsFor prop equiv
                   strategy <- strategiesFor prop instrs start
                   smartints <- smartintsFor prop instrs
                   return Parameters{..}

-- Only the relevant configurations that we list in the paper at the moment

relPropsFor :: GenInstrs -> [PropTest]
relPropsFor InstrsBasic = [PropEENI]
relPropsFor InstrsCally = [PropEENI, PropLLNI, PropSSNI]
relPropsFor _           = error "relPropsFor: not supported"

relEquivFor :: GenInstrs -> PropTest -> [Equiv]
relEquivFor InstrsBasic PropEENI = [EquivMem]
relEquivFor InstrsCally PropEENI = [EquivMem, EquivLow]
relEquivFor InstrsCally PropLLNI = [EquivLow]
relEquivFor InstrsCally PropSSNI = [EquivFull]
  -- EquivMem, EquivLow, and EquivWrongFull don't work for SSNI
relEquivFor _           _        = error "relEquivFor: not supported"

relStartsFor :: PropTest -> GenInstrs -> Equiv -> [StartingAS]
relStartsFor PropSSNI _           _        = [StartArbitrary]
relStartsFor PropEENI InstrsBasic _        = [StartInitial]
relStartsFor PropEENI InstrsCally EquivMem = [StartInitial]
relStartsFor PropEENI InstrsCally EquivLow = [StartInitial,StartQuasiInitial]
relStartsFor PropLLNI _           _        = [StartQuasiInitial]
relStartsFor _        _           _        =
  error "relStartsFor: not supported"

relStrategiesFor :: PropTest -> GenInstrs -> [GenStrategy]
relStrategiesFor PropEENI InstrsBasic =
  [ GenWeighted, GenSequence, GenNaive, GenByExec ]
relStrategiesFor PropEENI InstrsCally =  
  [ GenByExec2 ]
relStrategiesFor PropLLNI InstrsCally =
  [ GenByExec2 ]
relStrategiesFor PropSSNI InstrsCally =
  [ GenNaive, GenTinySSNI ]
relStrategiesFor _ _ = error "relStrategiesFor: not supported"

relSmartIntsFor :: PropTest -> GenInstrs -> GenStrategy -> [Bool]
relSmartIntsFor PropEENI InstrsBasic strategy =
  [False | strategy == GenNaive
           || strategy == GenWeighted 
           || strategy == GenSequence] ++
  [True | strategy /= GenNaive
          && strategy /= GenWeighted ]
relSmartIntsFor _        _           _     = [True]

relParameters :: [Parameters]
relParameters = do instrs <- instructions
                   prop <- relPropsFor instrs
                   equiv <- relEquivFor instrs prop
                   let bugs =  bugsFor instrs
                   start    <- relStartsFor prop instrs equiv
                   strategy <- relStrategiesFor prop instrs
                   smartints <- relSmartIntsFor prop instrs strategy
                   return Parameters{..}

allParametersProf :: [ParametersProf]
allParametersProf = do 
  pstart     <- [StartInitial, StartQuasiInitial]
  pstrategy  <- [GenNaive, GenWeighted, GenSequence, GenByExec]
                -- for cally machine add , GenByExec2, GenByExec4
  psmartints <- [False, True]
  return ParametersProf{..}

data What = GenMakefileAll | GenTablesAll |
            GenMakefileRel | GenTablesRel |
            GenMakefileProf | GenTablesProf
  deriving (Read,Show,Eq,Typeable,Data)
data Config = Config { config :: What }
  deriving (Read,Show,Eq,Typeable,Data)

dfltConfig :: Config
dfltConfig = Config GenMakefileRel

main :: IO ()
main = do { f <- cmdArgs dfltConfig
          ; case config f of
               GenMakefileAll -> produceMakefile allParameters
               GenTablesAll -> produceTableNames allParameters
               GenMakefileRel -> produceMakefile relParameters
               GenTablesRel -> produceTableNames relParameters
               GenMakefileProf -> produceMakefileProf
               GenTablesProf -> produceTableNamesProf
          }

produceMakefile :: [Parameters] -> IO ()
produceMakefile params
  = do putStrLn "# THIS FILE HAS BEEN AUTOMATICALLY GENERATED!"
       putStrLn "# EDIT IT AT YOUR OWN RISK"
       putStrLn $ "all-picotables: " ++ unwords (map name params)
       forM_ params $ \p -> do
         putStrLn $ name p ++ ":"
         putStrLn . ('\t' :) $ commandLine p

produceTableNames :: [Parameters] -> IO ()
produceTableNames params
  = forM_ grouped_params $ \g -> process_group g >> putStrLn "\\clearpage"
  where grouped_params = groupBy group_pred params
        group_pred x y = (prop x == prop y && instrs x == instrs y)
        process_group g = forM_  g $ \p ->
                            putStrLn $ "\\bugtable[h]{" ++ name_short p ++ "}{}"

produceMakefileProf :: IO ()
produceMakefileProf
  = do putStrLn $ "all-picoprofiles: " ++
         unwords (map name_prof allParametersProf) ++
            " " ++ unwords (map name_profvar allParametersProf)
         
       forM_ allParametersProf $ \p -> do
         putStrLn $ name_prof p ++ ":"
         putStrLn . ('\t' :) $ commandLineProf p
         putStrLn $ name_profvar p ++ ":"
         putStrLn . ('\t' :) $ commandLineProfVar p

produceTableNamesProf :: IO ()
produceTableNamesProf
  = forM_ allParametersProf $ \p ->
      do { putStrLn $ "\\proftable[h]{" ++ name_short_prof p ++ "}{}"
         ; putStrLn $ "\\profvartable[h]{" ++ name_short_prof p ++ "}{}" }
