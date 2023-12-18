module Day3 (answer) where

import Data.Char (isDigit)
import Data.List (nub, groupBy, sort)

type Pos = (Int, Int)

input :: IO String
input = readFile "data/day3input.txt"

coords :: [String] -> [[(Pos, Char)]]
coords ls = [ [((y,x),c) | (c, x) <- zip line [0..length line]] | (line, y) <- zip ls   [0..length ls] ]

touchingGears :: Pos -> [String] -> [Pos]
touchingGears (y,x) g = map snd . filter ((=='*') . fst) $ neighbors
    where neighbors = [((g !! y') !! x',(y',x')) | x' <- [x - 1 .. x + 1], 0 <= x', x' <= width,
                                                   y' <- [y - 1 .. y + 1], 0 <= y', y' <= height]
          height    = length g - 1
          width     = length (head g) - 1

touching :: [String] -> [([Pos], Char)]
touching g = concat [map (\(p,c) -> (touchingGears p g, c)) l ++ [([],'\n')]| l <- coords g]

gearTouches :: [([Pos], Char)] -> [(Pos,Integer)]
gearTouches l = case dropWhile (not . isDigit . snd) l of
    [] -> []
    relevant -> firstTouches ++ (gearTouches . dropWhile (isDigit . snd) $ relevant)
        where firstDigits = takeWhile (isDigit . snd) relevant
              firstNumber = read . map snd $ firstDigits
              firstTouches = [(gear, firstNumber) | gear <- nub $ concatMap fst firstDigits]

gearScore :: [(Pos, Integer)] -> Integer
gearScore = sum . map (product . map snd) . filter (\l -> length l == 2) . groupBy (\x y -> fst x == fst y) . sort

answer :: IO Integer
answer = gearScore . gearTouches . touching . lines <$> input