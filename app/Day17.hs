{-# LANGUAGE TemplateHaskell #-}

module Day17 (minHeat, answer1, HeatMap) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.PSQueue as Q
import Data.PSQueue (Binding((:->)), PSQ)
import Control.Lens (use, (%=), (.=), makeLenses, (+=))
import Control.Monad.State ( guard, when, liftIO, evalStateT, StateT )
import Data.Maybe (mapMaybe)
import Data.Char (digitToInt)
import System.Console.ANSI (clearScreen)
import System.Time.Extra (sleep)
import GHC.Plugins (Direction)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDayTime), DiffTime)

type Coord   = (Int, Int)
data Dir     = U | D | L | R deriving (Eq, Show, Ord)
type Counter = Int

type HeatMap = Map Coord Int
type Node    = (Coord, Dir, Counter)
type RefNode = (Node, Node)
type Queue      = PSQ RefNode Int
type Visited    = Set Node
type Refs = Map Node Node

data CrucibleState = CrucibleState {_time :: DiffTime , _tick :: Int, _refs :: Refs, _maxC :: Coord, _queue :: Queue, _visited :: Visited, _end :: Coord, _heatMap :: HeatMap }

makeLenses ''CrucibleState

-- Part 1

toCoord :: Node -> Coord
toCoord (c,_,_) = c

isVisited :: Visited -> Node -> Bool
isVisited v n = n `S.member` v

neighbors :: Node -> [Node]
neighbors ((y,x),d,i) = case d of
    U -> [((y,x-1),L,0),((y,x+1),R,0)] ++ [((y-1,x),U,i+1) | i /= 2]
    D -> [((y,x-1),L,0),((y,x+1),R,0)] ++ [((y+1,x),D,i+1) | i /= 2]
    L -> [((y+1,x),D,0),((y-1,x),U,0)] ++ [((y,x-1),L,i+1) | i /= 2]
    R -> [((y+1,x),D,0),((y-1,x),U,0)] ++ [((y,x+1),R,i+1) | i /= 2]

counter :: Node -> Int
counter (_,_,c) = c

heuristic :: Coord -> Coord -> Int
heuristic (y,x) (y',x') = abs (y-y') + abs (x-x')
-- heuristic (y,x) (y',x') = round . sqrt . fromIntegral $ (y-y')^(2 :: Int) + (x-x')^(2 :: Int)

shortestPath :: StateT CrucibleState IO Int
shortestPath = do
    q  <- use queue
    hm <- use heatMap
    v  <- use visited
    e  <- use end
    case Q.findMin q of
         Nothing -> return 0
         Just b  -> do
            let ((node,prev),heat) = (Q.key b, Q.prio b)
            refs %= M.insert node prev
            r <- use refs
            tick += 1
            c <- use tick
            when (c `mod` 10000 == 0) $ do
                liftIO clearScreen
                liftIO . putStr $ drawPath (path node r) (S.map toCoord v)
            queue %= Q.deleteMin
            if toCoord node == e && counter node > 2 then do
                liftIO clearScreen
                liftIO . putStr $ drawPath (path node r) (S.map toCoord v)
                return heat
            else do
                let he = heuristic e $ toCoord node
                visited %= S.insert node
                let ns = mapMaybe (\n -> do h <- M.lookup (toCoord n) hm
                                            let h' = h + heuristic e (toCoord n) - he
                                            -- let h' = h
                                            guard (not $ isVisited v n)
                                            guard (case Q.lookup (n,node) q of
                                                Nothing -> True
                                                Just l' -> heat + h' < l')
                                            return ((n,node), heat + h' )) $ ultraNeighbors node
                mapM_ ((queue %=) . uncurry Q.insert) ns
                shortestPath

parseInput :: String -> HeatMap
parseInput s = M.fromList . concat $ [ [ ((y,x), digitToInt $ (l!!y)!!x) | x <- [0..w-1]] | y <- [0..h-1]]
    where l = lines s
          h = length l
          w = length . head $ l

initialState :: HeatMap -> CrucibleState
initialState h = CrucibleState { _queue = Q.fromList [(((0,0),R,1),((0,0),R,1)) :-> 280
                                                    , (((0,0),D,1),((0,0),D,1)) :-> 280]
                               , _visited = S.empty
                               , _end = (140,140)
                               , _heatMap = h
                               , _maxC = (0,0)
                               , _refs = M.empty
                               , _tick = 0 
                               , _time = 0}
minHeat :: HeatMap -> IO Int
minHeat = evalStateT shortestPath . initialState

answer1 :: IO Int
answer1 = minHeat . parseInput =<< readFile "data/day17input.txt"

-- Part 2

-- Ultra crucible: minimum of four blocks in any direction, maximum of ten blocks
-- Need to update neighbor function 

ultraNeighbors :: Node -> [Node]
ultraNeighbors ((y,x),d,i) = case d of
    U -> (if i > 2 then [((y,x-1),L,0),((y,x+1),R,0)] else []) ++ [((y-1,x),U,i+1) | i < 9]
    D -> (if i > 2 then [((y,x-1),L,0),((y,x+1),R,0)] else []) ++ [((y+1,x),D,i+1) | i < 9]
    L -> (if i > 2 then [((y+1,x),D,0),((y-1,x),U,0)] else []) ++ [((y,x-1),L,i+1) | i < 9]
    R -> (if i > 2 then [((y+1,x),D,0),((y-1,x),U,0)] else []) ++ [((y,x+1),R,i+1) | i < 9]

type Path = Map Coord Dir

dir :: Node -> Dir
dir (_,d,_) = d

path :: Node -> Refs -> Path
path n r | toCoord n == (0,0) = M.singleton (toCoord n) (dir n)
         | otherwise = M.insert (toCoord n) (dir n)  (case M.lookup n r of Nothing -> M.empty; Just p -> path p r)

drawPath :: Path -> Set Coord -> String
drawPath p v = concat [ [ case M.lookup (y, x) p of
                            Nothing -> if (y,x) `S.member` v then '#' else '.'
                            Just d  -> dirToChar d
                      | x <- [0..140] ] ++ "\n" | y <- [0..140]]

dirToChar :: Dir -> Char
dirToChar U = '^'
dirToChar D = 'v'
dirToChar L = '<'
dirToChar R = '>'


