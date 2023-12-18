module Day9 (answer1, answer2) where

import Problem (answer)

parseInput :: String -> [[Int]]
parseInput = map (map read . words). lines

-- Part 1

diff :: [Int] -> [Int]
diff []       = []
diff [_]      = []
diff (a:b:as) = (b-a):diff (b:as)

diffs :: [Int] -> [[Int]]
diffs = takeWhile (not . all (==0)) . iterate diff

newValRight :: [Int] -> Int
newValRight = sum . map last . diffs

answer1 :: Bool -> IO Int
answer1 = answer 9 $ sum . map newValRight . parseInput

-- Part 2

newValLeft :: [Int] -> Int
newValLeft = sum . zipWith (*) (cycle [1,-1]) . map head . diffs

answer2 :: Bool -> IO Int
answer2 = answer 9 $ sum . map newValLeft . parseInput

{-
   d-c+b-a  d
   c-b+a    c
   b-a      b
   a        a
   0        0
-}