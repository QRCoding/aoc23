module Day1 (answer) where

import Data.Char (isDigit, intToDigit)
import Data.Maybe (fromMaybe)
import Data.List (inits, elemIndex, find, tails)

input :: IO String
input = readFile "data/day1input.txt"

parseInput :: String -> [Int]
parseInput =  map (read . parseLine) . lines

parseLine :: String -> String
parseLine s = [fromMaybe i initial, fromMaybe f final]
    where initial = findDigit True . takeWhile (not . isDigit) $ s
          final   = findDigit False . reverse . takeWhile (not . isDigit) . reverse $ s 
          (i,f)   = (\l -> (head l, last l)). filter isDigit $ s

digits :: [String]
digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

findDigit :: Bool -> String -> Maybe Char
findDigit _ [] = Nothing
findDigit b s  = case find (`elem` digits) . (if b then inits else tails) $ s of
    Nothing -> findDigit b . (if b then tail else init) $ s
    Just d  -> intToDigit . (+1) <$> elemIndex d digits

answer :: IO Int
answer = sum . parseInput <$> input