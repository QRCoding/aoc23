{-# LANGUAGE TupleSections #-}
module Day16 where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Lens ((%~), _1, _2, (^.))
import Control.Monad.State (State, get, put, evalState)
import Control.Monad (when)
import Problem (answer)

data LightDir    = U | D | L | R deriving (Eq, Show)
data Tile        = EmptyTile | MirrorR | MirrorL | SplitterV | SplitterH deriving (Eq, Show)
type Pos         = (Int, Int)

type Energized = Set Pos
type Layout    = Map Pos Tile
type Beam      = (Pos, LightDir)

type Light = ([Beam], Energized, Int)

toTile :: Char -> Tile
toTile '/' = MirrorR
toTile '\\' = MirrorL
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

step :: Layout -> State Light Int
step l = do
    (b, e, n) <- get
    if n == 0 then return $ S.size e
        else do let b' = concatMap (newBeams l) b
                let e' = S.union e . S.fromList . map fst $ b'
                if S.size e == S.size e' then put (b',e',n-1)
                    else put (b',e',n)
                step l
                
energized :: Layout -> Int
energized l = evalState (step l) start

start :: Light
start = ([((0,-1), R)], S.empty, 10)

answer1 :: Bool -> IO Int
answer1 = answer 16 $ energized . parseInput

-- It stabilizes after a while at 8034
-- take 1000 . energized . parseInput <$> readFile "data/day16input.txt"

-- Part 2
-- Total number of tiles is 12100



















