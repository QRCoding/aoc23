module Day13 (answer1, answer2) where

import Data.List (transpose)
import Data.List.Split (splitOn, chunksOf)
import Problem (answer)
import Control.Lens.Setter ((%~))
import Control.Lens.Combinators (element)
import Control.Lens.Operators ((&))
import Data.Maybe (listToMaybe, fromMaybe)

-- Part 1

reflectVertical :: Int -> [String] -> Bool
reflectVertical n = all $ reflectRow n
    where reflectRow n' r = segment (reverse . take n' $ r) (drop n' r)

segment :: String -> String -> Bool
segment s s' = take n s' == take n s
    where n = min (length s) (length s')

reflectHorizontal :: Int -> [String] -> Bool
reflectHorizontal n s = reflectVertical n $ transpose s

findReflections :: [String] -> [Int]
findReflections ss = hor ++ ver
    where ver = filter (`reflectVertical` ss) [1..(length . head $ ss) - 1]
          hor = map (*100) . filter (`reflectHorizontal` ss) $ [1..length ss - 1]

parseInput :: String -> [[String]]
parseInput = splitOn [""] . lines

answer1 :: Bool -> IO Int
answer1 = answer 13 $ sum . map (fromMaybe 0 . listToMaybe . findReflections) . parseInput

-- Part 2

smudges :: [String] -> [[String]]
smudges ss = map (chunksOf width . (\n -> concat ss & element n %~ flipRock)) [0..total-1]
    where width = length . head $ ss
          height = length ss
          total = width * height

flipRock :: Char -> Char
flipRock '#' = '.'
flipRock _   = '#'

newReflection :: [String] -> Int
newReflection ss = head . head . filter (/=[]) . map (filter (/=r) . findReflections) . smudges $ ss
    where r = head $ findReflections ss

answer2 :: Bool -> IO Int
answer2 = answer 13 $ sum . map newReflection . parseInput
