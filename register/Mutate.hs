{-# LANGUAGE TupleSections, RecordWildCards #-}
module Mutate where

import Control.Monad
import Data.Maybe
import Rules
import qualified Data.Map as Map

breakExpr :: RuleExpr -> [RuleExpr]
breakExpr EBot = []
breakExpr (EVar (s,v)) = [EVar (s,v)]
breakExpr (EJoin e1 e2) = breakExpr e1 ++ breakExpr e2

joinExprs :: [RuleExpr] -> RuleExpr
joinExprs [] = EBot
joinExprs [e] = e
joinExprs (h:t) = EJoin h $ joinExprs t

breakScond :: SideCond -> [SideCond]
breakScond ATrue = []
breakScond (ALe e1 e2) = map (flip ALe e2) (breakExpr e1)
breakScond (AAnd c1 c2) = breakScond c1 ++ breakScond c2
breakScond (AOr  c1 c2) = breakScond c1 ++ breakScond c2

andSconds :: [SideCond] -> SideCond
andSconds [] = ATrue
andSconds [c] = c
andSconds (h:t) = AAnd h (andSconds t)

dropAux :: a -> (a, [a]) -> (a, [a])
dropAux x xs = (fst xs, x : snd xs)

dropEachButNotPC :: [RuleExpr] -> [(RuleExpr,[RuleExpr])]
dropEachButNotPC [] = []
dropEachButNotPC (v@(EVar ("LabPC",_)):xs) = map (dropAux v) $ dropEachButNotPC xs
dropEachButNotPC (x:xs) = (x,xs) : (map (dropAux x) (dropEachButNotPC xs))

dropEach :: [x] -> [[x]]
dropEach [] = []
dropEach (x:xs) = xs : (map (x:) (dropEach xs))

mutatePC :: Maybe RuleExpr -> RuleExpr -> [(Maybe RuleExpr, RuleExpr)]
mutatePC ores epc =  
  case breakExpr epc of 
    [] -> []
    es -> 
     case ores of 
       Just eres -> 
           let f xs = (Just (EJoin eres (fst xs)), joinExprs (snd xs)) in
           map f (dropEachButNotPC es)
       Nothing -> 
           let f xs = (Nothing, joinExprs xs) in
           map f (dropEach es)

mutateExpr :: RuleExpr -> [RuleExpr]
mutateExpr e = case breakExpr e of 
                 [] -> []
                 es -> map joinExprs $ dropEach es

mutateScond :: SideCond -> [SideCond]
mutateScond c = case breakScond c of 
                  [] -> []
                  cs -> map andSconds $ dropEach cs

mutateRule :: Rule -> [Rule]
mutateRule Rule{..} = 
    (map (\a -> Rule a rlab rlpc) (mutateScond allow)) ++
    (join . maybeToList $ 
          fmap (map (flip (Rule allow) rlpc . Just) . mutateExpr) rlab) ++
    (map (\respc -> Rule allow (fst respc) (snd respc)) (mutatePC rlab rlpc))
--Rule allow rlab) $ mutateExpr rlpc)

{-
helper :: InstrKind -> Rule -> InstrKind -> Rule -> Rule
helper o mr o' orig
    | o == o'   = mr
    | otherwise = orig

mutateTable' :: RuleTable -> RuleTable -> [RuleTable]
mutateTable' t t' = 
    foldl' (++) 
     (map (\o -> 
      (map (\mr o' -> 
       helper o mr o' (getRule t' o')
      ) (mutateRule (getRule t o)))
     ) allInstrKind) []
-}
mutateTable :: RuleTable -> [RuleTable] 
mutateTable t = Map.foldWithKey 
                 (\k a -> (++) $ map (flip (Map.insert k) t) (mutateRule a)) 
                 [] t

-- Convert to LaTeX    
showMutantTable :: RuleTable -> String
showMutantTable t = 
    show $ head $ Map.keys $ Map.differenceWith 
           (\a b -> if show a == show b then Nothing else Just a) 
           t defaultTable

showMutantX :: Int -> String
showMutantX n = 
    show $ Map.differenceWith 
           (\a b -> if show a == show b then Nothing else Just a) 
           (mutateTable defaultTable !! n)
           defaultTable
