{-# LANGUAGE TupleSections #-}

module Day16 (answer2, answer1, totalB, Beam, LightDir) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Control.Lens ((%~), _1, _2, (^.))
import Control.Monad.State (State, get, put, evalState)
import Problem (answer)

data LightDir    = U | D | L | R deriving (Eq, Show, Read, Ord)
data Tile        = EmptyTile | MirrorR | MirrorL | SplitterV | SplitterH deriving (Eq, Show)
type Pos         = (Int, Int)

type Visited   = Set Beam
type Layout    = Map Pos Tile
type Beam      = (Pos, LightDir)

type LightState = ([Beam], Visited, Layout)

toTile :: Char -> Tile
toTile '/' = MirrorR
toTile '\\'= MirrorL
toTile '|' = SplitterV
toTile '-' = SplitterH
toTile _   = EmptyTile

parseInput :: String -> Layout
parseInput s = M.fromList . concat . zipWith line [0..h-1] . lines $ s
   where h = length . lines $ s
         w = length . head . lines $ s
         line y = zipWith (\x c -> ((y,x), toTile c)) [0..w-1]

nextPos :: Beam -> Pos
nextPos (p, d) = case d of
    U -> _1 %~ subtract 1 $ p
    D -> _1 %~ (+1)       $ p
    L -> _2 %~ subtract 1 $ p
    R -> _2 %~ (+1)       $ p

turn :: Tile -> LightDir -> [LightDir]
turn EmptyTile d = [d]
turn MirrorR   d = case d of U -> [R]; D -> [L]; L -> [D]; R -> [U]
turn MirrorL   d = case d of U -> [L]; D -> [R]; L -> [U]; R -> [D]
turn SplitterV d = case d of U -> [U]; D -> [D]; _ -> [U,D]
turn SplitterH d = case d of R -> [R]; L -> [L]; _ -> [R,L]

newBeams :: Layout -> Beam -> [Beam]
newBeams l b = case M.lookup (nextPos b) l of
    Nothing -> []
    Just t  -> map (nextPos b,) . turn t $ b ^. _2

energized :: State LightState Int
energized = do
    (b, v, l) <- get
    let b' = S.fromList (concatMap (newBeams l) b) `S.difference` v
    if null b' then return . S.size $ S.map fst v
    else do
        let v' = v `S.union` b'
        put (S.toList b', v', l)
        energized

total :: Layout -> Int
total = evalState energized . start

start :: Layout -> LightState
start l = ([((0,-1), R)], S.empty, l)

answer1 :: Bool -> IO Int
answer1 = answer 16 $ total . parseInput

-- It stabilizes after a while at 8034
-- take 1000 . energized . parseInput <$> readFile "data/day16input.txt"

-- Part 2
-- Total number of tiles is 12100

startB :: Layout -> Beam -> LightState
startB l b = ([b], S.empty, l)

totalB :: Layout -> Beam -> Int
totalB l = evalState energized . startB l 

startingBeams :: [Beam]
startingBeams = right ++ left ++ up ++ down
    where right = map (\n -> ((n,110),L)) [0..109]
          left  = map (\n -> ((n,-1), R)) [0..109]
          up    = map (\n -> ((-1,n), D)) [0..109]
          down  = map (\n -> ((110,n),U)) [0..109]

maxEnergized :: Layout -> Int
maxEnergized l = maximum . map (totalB l) $ startingBeams

answer2 :: Bool -> IO Int
answer2 = answer 16 $ maxEnergized . parseInput

-- Biggest seen so far: 8219










