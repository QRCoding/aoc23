module Day10 (answer1) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)
import Data.List (findIndex, elemIndex)

import Problem (answer)

data Direction = N | E | S | W deriving (Eq, Show, Ord)
type PipeMap   = Pos -> Char
type Pos       = (Int, Int)

input :: IO String
input = readFile "data/day10input.txt"

parseInput :: String -> PipeMap
parseInput s (y,x) = (lines s!!y)!!x

-- Part 1

startPos :: String -> Pos
startPos s = (y,x)
    where y = fromMaybe 0 $ findIndex ('S' `elem`) (lines s)
          x = fromMaybe 0 . elemIndex 'S' . head . filter ('S' `elem`) $ lines s

pipes :: Map (Direction, Char) Direction
pipes = Map.fromList [((N, 'F'), E)
                    , ((W, 'F'), S)
                    , ((E, 'J'), N)
                    , ((S, 'J'), W)
                    , ((E, '7'), S)
                    , ((N, '7'), W)
                    , ((W, 'L'), N)
                    , ((S, 'L'), E)
                    , ((E, '-'), E)
                    , ((W, '-'), W)
                    , ((N, '|'), N)
                    , ((S, '|'), S)]

toVector :: Direction -> Pos
toVector N = (-1,0)
toVector S = (1,0)
toVector W = (0,-1)
toVector E = (0,1)

getDirection :: (Direction, Char) -> Direction
getDirection dc = fromMaybe N $ Map.lookup dc pipes

move :: (Pos, Direction) -> PipeMap -> (Pos, Direction)
move ((y,x),d) m = (p',d')
    where p' = bimap (y+) (x+) $ toVector d
          d' = getDirection (d, m p')

loopLength :: (Pos, Direction) -> PipeMap -> Int
loopLength (pos, d) m = loop (move (pos,d) m) (pos, 1) m
    where loop (pos', d') (goal, steps) m'
            | pos' == goal = steps
            | otherwise    = loop (move (pos',d') m) (goal, steps + 1) m'

start :: (Pos, Direction)
start = ((109,28),S)

answer1 :: Bool -> IO Int
answer1 = answer 10 $ (`div` 2) . loopLength start . parseInput

-- Part 2
