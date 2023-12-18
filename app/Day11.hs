{-#LANGUAGE TupleSections #-}

module Day11 (answer1, answer2) where

import Problem (answer)

import Data.List (transpose)
import GHC.Utils.Misc (count)

      

-- Part 1

expand :: [String] -> [String]
expand =  transpose . expandOneDir . transpose . expandOneDir
    where expandOneDir = concatMap doubleIfNoGalaxies
          doubleIfNoGalaxies s | hasGalaxies s = [s]
                             | otherwise     = [s,s]
      
galaxyCoordinates :: [String] -> [(Int,Int)]
galaxyCoordinates ss = (concatMap ((map snd . filter ((=='#') . fst)) . lineCoords) . zip [1..h]) ss
    where h = length ss
          w = length . head $ ss
          lineCoords (y,s) = zipWith (\x c -> (c,(y,x))) [1..w] s

distance :: (Int, Int) -> (Int, Int) -> Int
distance (a,b) (c,d) = abs (c - a) + abs (d - b)

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (a:as) = map (a,) as ++ pairs as

totalDistance :: [(Int,Int)] -> Int
totalDistance = sum . map (uncurry distance) . pairs

answer1 :: Bool -> IO Int
answer1 = answer 11 $ totalDistance . galaxyCoordinates . expand . lines

-- Part 2 

hasGalaxies :: String -> Bool
hasGalaxies = elem '#'

emptyBetween :: (Int, Int) -> (Int, Int) -> [String] -> Int
emptyBetween (y,x) (y',x') ss = emptyRows (min y y') (max y y') ss + emptyCols (min x x') (max x x') ss
      where emptyRows l u = count (not . hasGalaxies) . take (u - l - 1) . drop l
            emptyCols l u = emptyRows l u . transpose

distance' :: (Int, Int) -> (Int, Int) -> Int -> [String] -> Int
distance' p q e ss = distance p q + emptyBetween p q ss * (e - 1)

totalDistance' :: Int -> [String] -> Int
totalDistance' e ss = sum . map (\(p,q) -> distance' p q e ss) . pairs . galaxyCoordinates $ ss

answer2 :: Bool -> IO Int
answer2 = answer 11 $ totalDistance' 1000000 . lines
