module Day14 (answer1, cycleLoads) where

import Data.List (groupBy, transpose)
import Problem (answer)

-- Part 1

equal :: Char -> Char -> Bool
equal '#' '#' = True
equal '#' _   = False
equal _  '#'  = False
equal _   _   = True

shiftRow :: String -> String
shiftRow = concatMap shiftBlock . groupBy equal
    where shiftBlock s | head s == '#' = s
                       | otherwise     = filter (=='O') s ++ filter (=='.') s

shiftNorth :: [String] -> [String]
shiftNorth = transpose . map shiftRow . transpose

totalLoad :: [String] -> Int
totalLoad ss = sum . zipWith (\n l -> n * (length . filter (=='O') $ l)) (reverse [1..length ss]) $ ss

answer1 :: Bool -> IO Int
answer1 = answer 14 $ totalLoad .  shiftNorth . lines

-- Part 2

data Direction = N | E | S | W

shift :: Direction -> [String] -> [String]
shift N = transpose . shift W . transpose
shift W = map shiftRow
shift E = map (reverse . shiftRow . reverse)
shift S = transpose . shift E . transpose

doCycle :: [String] -> [String]
doCycle = shift E . shift S . shift W . shift N

cycleLoads :: [String] -> [(Int, Int)]
cycleLoads = zip (cycle [0..33]) . map (subtract 112400 . totalLoad) . iterate doCycle

-- The loads repeat after 34 cycles
-- 1000000000 mod 34 is 24 -> Get 112400 + 52 
-- First drop 102 to get rid of transient behaviour and get into stable cycle
-- take 34 . drop 102 . cycleLoads . lines <$> readFile "data/day14input.txt"
