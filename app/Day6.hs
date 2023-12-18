module Day6 (answer1, answer2) where

import GHC.Float (int2Double)

races :: [(Int, Int)]
races = [(35, 212), (93, 2060), (73, 1201), (66, 1044)]

bigRace :: (Int, Int)
bigRace = (t,d)
    where t = read . concatMap (show . fst) $ races
          d = read . concatMap (show . snd) $ races

waysToWin :: (Int,Int) -> Int
waysToWin (t,d) = upper - lower + 1
    where s     = sqrt $ int2Double t**2 / 4 - int2Double d
          lower = ceiling $ int2Double t / 2 - s
          upper = floor   $ int2Double t / 2 + s

answer1 :: Int
answer1 = product . map waysToWin $ races

answer2 :: Int
answer2 = waysToWin bigRace
