{-# LANGUAGE RecordWildCards #-}
module Rules where

import Control.Monad

import Labels 
import Instructions
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

data MVec = MVec { mLab1 :: Label
                 , mLab2 :: Label
                 , mLab3 :: Label 
                 , mLab4 :: Label
                 , mLabPC :: Label}

type Var = MVec -> Label

data RuleExpr = EBot
              | EVar  (String, Var)
              | EJoin RuleExpr RuleExpr

instance Show RuleExpr where
    show EBot = "EBot"
    show (EVar (s,_)) = "EVar " ++ s
    show (EJoin e1 e2) = "JOIN ( " ++ show e1 ++ " " ++ show e2 ++ " )"

data SideCond = ATrue
              | ALe   RuleExpr RuleExpr
              | AAnd  SideCond SideCond
              | AOr   SideCond SideCond
              deriving (Show)

data Rule = Rule { allow :: SideCond
                 , rlab  :: Maybe RuleExpr
                 , rlpc  :: RuleExpr }
          deriving(Show)

type RVec = Maybe (Maybe Label, Label)
  
evalExpr :: MVec -> RuleExpr -> Label
evalExpr _ EBot = bot
evalExpr m (EVar (_,l)) = l m
evalExpr m (EJoin r1 r2) = evalExpr m r1 `lub` evalExpr m r2

evalCond :: MVec -> SideCond -> Bool
evalCond _m ATrue = True
evalCond m (AAnd c1 c2) = evalCond m c1 && evalCond m c2
evalCond m (AOr  c1 c2) = evalCond m c1 || evalCond m c2
evalCond m (ALe  e1 e2) = evalExpr m e1 `flowsTo` evalExpr m e2

applyRule :: MVec -> Rule -> RVec
applyRule m Rule{..} 
    | evalCond m allow = Just (fmap (evalExpr m) rlab, evalExpr m rlpc)
    | otherwise        = Nothing
      
type RuleTable = Map InstrKind Rule

getRule :: RuleTable -> InstrKind -> Rule
getRule t op = fromJust $ Map.lookup op t

runTMU' :: RuleTable -> InstrKind -> MVec -> RVec 
runTMU' t op m = join . fmap (applyRule m) $ Map.lookup op t

runTMU :: RuleTable -> InstrKind -> [Label] -> Label -> RVec
runTMU t op labs lpc = runTMU' t op (buildMVec (labs ++ repeat undefined) lpc) 

buildMVec :: [Label] -> Label -> MVec
buildMVec (l1:l2:l3:l4:_) lpc = MVec l1 l2 l3 l4 lpc
buildMVec _ _ = error "buildMVec"

-- Default Rule Table

parseRule :: String -> (InstrKind, Rule)
parseRule s = 
    let (op:_:_:rest) = words s
        (scond, (_:rest')) = span (/= ",") rest
        (rexp1, (_:rexp2)) = span (/= ",") rest'
    in (read op, Rule (fst $ parseSCond scond)
                      (fst $ parseExpr rexp1)
                      (fromJust . fst $ parseExpr rexp2))

parseSCond :: [String] -> (SideCond, [String])
parseSCond ("TRUE":r) = (ATrue, r)
parseSCond ("LE":r) = let (Just e1, r') = parseExpr r
                          (Just e2, r'' ) = parseExpr r'
                      in (ALe e1 e2, r'')
parseSCond a = error $ "Unexpected" ++ show a

parseExpr :: [String] -> (Maybe RuleExpr, [String])
parseExpr ("__":r)   = (Nothing, r)
parseExpr ("(":r) = parseExpr r
parseExpr (")":r) = parseExpr r
parseExpr ("BOT":r)  = (Just EBot, r)
parseExpr ("JOIN":r) = let (Just e1, r') = parseExpr r
                           (Just e2, r'' ) = parseExpr r'
                       in (Just $ EJoin e1 e2, r'')
parseExpr ("Lab1":r) = (Just $ EVar ("Lab1",mLab1), r)
parseExpr ("Lab2":r) = (Just $ EVar ("Lab2",mLab2), r)
parseExpr ("Lab3":r) = (Just $ EVar ("Lab3",mLab3), r)
parseExpr ("Lab4":r) = (Just $ EVar ("Lab4",mLab4), r)
parseExpr ("LabPC":r) = (Just $ EVar ("LabPC",mLabPC), r)
parseExpr a = error $ "Unexpected" ++ show a

defaultTable :: RuleTable
defaultTable = Map.fromList . map parseRule $ [
  "LAB     ::=  << TRUE , BOT , LabPC >>",
  "MLAB    ::=  << TRUE , Lab1 , LabPC >>",
  "PCLAB   ::=  << TRUE , BOT , LabPC >>",
  "BCALL   ::=  << TRUE , JOIN Lab2 LabPC , JOIN Lab1 LabPC >>",
  "BRET    ::=  << LE ( JOIN Lab1 LabPC ) ( JOIN Lab2 Lab3 ) , Lab2 , Lab3 >>",
  "PUTLAB  ::=  << TRUE , BOT , LabPC >>",
  "NOOP    ::=  << TRUE , __ , LabPC >>",
  "PUT     ::=  << TRUE , BOT , LabPC >>",
  "BINOP   ::=  << TRUE , JOIN Lab1 Lab2 , LabPC >>",
  "JUMP    ::=  << TRUE , __ , JOIN LabPC Lab1 >>",
  "BNZ     ::=  << TRUE , __ , JOIN Lab1 LabPC >>",
  "LOAD    ::=  << TRUE , Lab3 , JOIN LabPC ( JOIN Lab1 Lab2 ) >>",
  "STORE   ::=  << LE ( JOIN Lab1 LabPC ) ( JOIN Lab2 Lab3 ) , Lab3 , LabPC >>",
  "WRITE   ::=  << LE ( JOIN ( JOIN LabPC Lab1 ) Lab3 ) ( JOIN Lab2 Lab4 ) , Lab4 , LabPC >>",
  "ALLOC   ::=  << TRUE , JOIN Lab1 Lab2 , LabPC >>",
  "PSETOFF ::=  << TRUE , JOIN Lab1 Lab2 , LabPC >>",
  "PGETOFF ::=  << TRUE , Lab1 , LabPC >>",
  "MSIZE   ::=  << TRUE , Lab2 , JOIN LabPC Lab1 >>",
  "MOV     ::=  << TRUE , Lab1 , LabPC >>"]
