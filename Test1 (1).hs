-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Test1 ( evenMajority
             , get5SmoothNumbers
             , comesBefore
             , countApplications
             , f 
             ) where

import Types

{- QUESTION 1 -}

evenMajority :: [Int] -> Bool
evenMajority ns = (length (filter even ns)) > (length ns `div` 2)

{- QUESTION 2 -}

get5SmoothNumbers :: Int -> [Int]
get5SmoothNumbers k = filter (\x -> all (\f -> f <= 5) (primeFactors x)) [1..k]


{- QUESTION 3 -}

comesBefore :: TrainStop -> TrainStop -> Bool
comesBefore s1 s2 | s1 == BirminghamNewStreet = False 
                  | theStopAfter s1 == s2 = True
                  | otherwise = comesBefore (theStopAfter s1) s2

{- QUESTION 4 -}

countApplications :: (a -> a) -> (a -> Bool) -> a -> Int
countApplications f p x | p x       = 0
                        | otherwise = 1 + countApplications f p (f x)

{- QUESTION 5 -}

f :: (a -> a -> r) -> ((a -> r) -> a) -> r
f g h = g (h (\b -> g b b)) (h (\c -> g c c))





