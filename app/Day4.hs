{-# LANGUAGE TupleSections #-}

module Day4 (answer1, answer2) where

import Data.List.Split (splitOn)
import Problem (answer)

parseInput :: String -> [([Int],[Int])]
parseInput = map (pair . map (map read . words) . splitOn " | " . drop 2 . dropWhile (/= ':')) . lines
    where pair l = case l of [a,b] -> (a,b); _ -> ([],[])

-- Part 1

score :: ([Int], [Int]) -> Int
score (win, own) = case n of 0 -> 0; _ -> 2^(n-1)
    where n = length (filter (`elem` win) own)

totalScore :: [([Int],[Int])] -> Int
totalScore = sum . map score

answer1 :: Bool -> IO Int
answer1 = answer 4 $ totalScore . parseInput

-- Part 2

matches :: ([Int], [Int]) -> Int
matches (win, own) = length (filter (`elem` win) own)

matchList :: [([Int],[Int])] -> [(Int,Int)]
matchList l = zip (replicate (length l) 1) $ map matches l

reduce :: (Int, [(Int,Int)]) -> Int
reduce (t,[])                   = t
reduce (t,(copies, match):rest) = reduce (t + copies, map (\(c,m) -> (c + copies, m)) (take match rest) ++ drop match rest)

answer2 :: Bool -> IO Int
answer2 = answer 4 $ reduce . (0,) . matchList . parseInput