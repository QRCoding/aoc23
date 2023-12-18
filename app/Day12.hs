module Day12 (answer1) where

import Data.List.Split (splitOn)
import Problem (answer)
-- import Data.Bifunctor (bimap)
-- import GHC.Utils.Misc (count)

-- Part 1

contiguous :: String -> [Int]
contiguous = map length . filter (not . null) . splitOn "."

possible :: String -> [String]
possible ""                = [""]
possible (c:cs)
    | c == '#' || c == '.' = map (c:) $ possible cs
    | otherwise            = map ('#':) (possible cs) ++ map ('.':) (possible cs)

arrangements :: [Int] -> String -> Int
arrangements ns = length . filter ((== ns) . contiguous) . possible

parseInput :: String -> [([Int], String)]
parseInput = map ((\(s,ns) -> (tolist ns, s)) . pair . splitOn " ") . lines

tolist :: String -> [Int]
tolist s = read $ "[" ++ s ++ "]"

pair :: [String] -> (String, String)
pair [a,b] =  (a,b)
pair _     = ("","")

answer1 :: Bool -> IO Int
answer1 = answer 12 $ sum . map (uncurry arrangements) . parseInput

-- Part 2

-- unfoldS :: String -> String
-- unfoldS s = init . concat . replicate 5 $ s ++ "?"
--
-- unfold :: [Int] -> [Int]
-- unfold = concat . replicate 5
--
-- minimal :: [Int] -> String
-- minimal ns = init . concat $ do
--     n <- ns
--     return $ replicate n '#' ++ "."
--
-- discrepancy :: [Int] -> String -> Int
-- discrepancy ns s = length (unfoldS s) - length (minimal . unfold $ ns)
--
-- test :: [([Int], String)] -> [(Int,Int)]
-- test = map $ \(ns, s) -> 
--     let ns'   = unfold ns
--         s'    = unfoldS s
--         stars = discrepancy ns' s'
--         bars  = length ns'
--      in (stars + bars, bars)
