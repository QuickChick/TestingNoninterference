{-# LANGUAGE RecordWildCards #-}
module Zipper where

import Data.List 
import Data.Maybe

data Zipper a = Zipper { front   :: [a]
                       , current :: a
                       , back    :: [a]
                       , idx     :: Int} 

toList :: Zipper a -> [a]
toList Zipper{..} = foldl' (flip (:)) (current : back) front

fromList :: [a] -> Zipper a
fromList [] = error "Attempted to make zipper of empty list"
fromList (h:t) = Zipper [] h t 0

moveZipper :: Zipper a -> Int -> Maybe (Zipper a)
moveZipper z@Zipper{..} n
    | idx == n = Just z
    | idx > n = 
        case front of 
          [] -> Nothing
          (x:t) -> 
              let z' = Zipper t x (current : back) (idx-1)
              in z' `seq` moveZipper z' n
    | otherwise = 
        case back of 
          [] -> Nothing
          (x:t) -> 
              let z' = Zipper (current : front) x t (idx+1)
              in z' `seq` moveZipper z' n
