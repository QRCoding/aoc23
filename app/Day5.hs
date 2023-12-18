{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day5 (answer1, answer2) where

import Problem (answer)
import Data.List (find, sortBy)
import Data.List.Split (splitOn, chunksOf)

parseInput :: String -> ([Integer], [[(Integer,Integer,Integer)]])
parseInput s = (seeds, maps)
    where seeds      = map read . tail . words . head . lines $ s
          goodLines  = filter ('m' `notElem`) . drop 2 . lines $ s
          maps       = map (map (triple . map read . words)) . splitOn [""] $ goodLines
          triple l   = case l of [a,b,c] -> (a,b,c); _ -> (0,0,0)

toMap :: Integer -> [(Integer,Integer,Integer)] -> Integer
toMap n ts = case find (\(_,s',l') -> s' <= n && n <= s'+l'-1) ts of
    Just (d,s,_) -> d + n - s
    Nothing      -> n

-- Part 1

location :: ([Integer], [[(Integer,Integer,Integer)]]) -> Integer
location (ns,l) = minimum $ map (\n -> foldl toMap n l) ns

answer1 :: Bool -> IO Integer
answer1 = answer 5 $ location . parseInput

-- Part 2

toRevMap :: Integer -> [(Integer,Integer,Integer)] -> Integer
toRevMap n ts = case find (\(d',_,l') -> d' <= n && n <= d'+l'-1) ts of
    Just (d,s,_) -> s + n - d
    Nothing      -> n

locationToSeed :: Integer -> [[(Integer,Integer,Integer)]] -> Integer
locationToSeed n l = foldl toRevMap n (reverse l)

toRanges :: [Integer] -> [(Integer, Integer)]
toRanges = map pair . chunksOf 2
    where pair l = case l of [a,b] -> (a,b); _ -> (0,0)

inRanges :: Integer -> [(Integer, Integer)] -> Bool
inRanges n = any (\(s, l) -> s <= n && n <= s + l + 1)

smallestLocation :: ([Integer], [[(Integer,Integer,Integer)]]) -> Maybe Integer
smallestLocation (seeds, maps) = find (\n -> inRanges (locationToSeed n maps') (toRanges seeds)) tryList
    where maps' = map (sortBy (\(d,_,_) (d',_,_) -> compare d d')) maps

-- Trial and error
{-
Step 1: Smallest valid location is at most 2000000000
Step 2: Smallest valid location is at most 79000000
Step 3: Smallest valid location is at most 78776000
-}

tryList :: [Integer]
tryList = [78775000..78776000]

validLocation :: Integer -> ([Integer], [[(Integer,Integer,Integer)]]) -> Bool
validLocation n (seeds,maps) = inRanges (locationToSeed n maps) (toRanges seeds)

-- Probability around 0.73 to pick a valid location

answer2 :: Bool -> IO (Maybe Integer)
answer2 = answer 5 $ smallestLocation . parseInput

input :: IO String
input = readFile "data/day5input.txt"

seedTotal :: IO (Integer,Integer)
seedTotal = do r <- ranges 
               let total = sum . map snd $ r
               let maxSeed = maximum . map fst $ r
               return (maxSeed, total)
    where ranges = toRanges . fst . parseInput <$> input
