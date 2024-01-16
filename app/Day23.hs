{-# LANGUAGE TemplateHaskell #-}
module Day23 where

import Linear.V2
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Lens (makeLenses, (%~), (.~), (^.), use, (%=), (.=))
import Linear.Vector ((^+^))
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Control.Lens.Lens ((&))

import Problem (answer)

data Dir  = N | S | E | W deriving (Eq, Show)
data Tile = Path | Forest | Slope Dir deriving (Eq, Show)
type Pos = V2 Int

type HikeMap = Map Pos Tile

data Hike = Hike { _pos :: Pos, _visited :: Set Pos, _count :: Int } deriving Show
data HikeState = HikeState { _hikes :: [Hike], _goal :: Pos, _maxCount :: Int } deriving Show
makeLenses ''HikeState
makeLenses ''Hike

charToTile :: Char -> Tile
charToTile c = case c of
    '.' -> Path
    '^' -> Slope N
    'v' -> Slope S
    '>' -> Slope E
    '<' -> Slope W
    _   -> Forest

parseInput :: String -> HikeMap
parseInput s = M.fromList $ do
    (y,l) <- zip [0..] $ lines s
    (x,c) <- zip [0..] l
    return (V2 y x, charToTile c)

start :: HikeMap -> Pos
start = fst . M.findMin . M.filter (== Path)

end :: HikeMap -> Pos
end = fst . M.findMax . M.filter (== Path)

dirs :: [V2 Int]
dirs = map dirToVec [N,S,E,W]

dirToVec :: Dir -> V2 Int
dirToVec d = case d of
    N -> V2 (-1) 0
    S -> V2 1 0
    E -> V2 0 1
    W -> V2 0 (-1)

neighbors :: HikeMap -> Pos -> [Pos]
neighbors h p = fromMaybe [] $ do
    t <- M.lookup p h
    return $ case t of
        Path -> filter (\q -> M.lookup q h `notElem` [Nothing, Just Forest]) $ map (^+^ p) dirs
        Forest -> []
        Slope d -> [p ^+^ dirToVec d]

initialState :: HikeMap -> HikeState
initialState h = HikeState { _hikes = [ Hike { _visited = S.empty
                                             , _pos = start h
                                             , _count = 0 } ]
                           , _goal = end h 
                           , _maxCount = 0 }

walk :: HikeMap -> State HikeState Int
walk hm = do
    l <- use hikes
    if null l then use maxCount
    else do
        g <- use goal
        maxCount %= \m -> maximum . (m: ) . map (^. count) . filter (\h -> h ^. pos == g) $ l
        hikes %= filter (\h -> h ^. pos /= g)
        hikes %= \hs -> do
            h <- hs
            let ns = filter (not . (`S.member` (h ^. visited))) . neighbors hm $ h ^. pos
            n <- ns
            return $ h & visited %~ S.insert (h ^. pos)
                       & pos     .~ n
                       & count   %~ (1+)
        walk hm

maxLength :: HikeMap -> Int
maxLength hm = evalState (walk hm) $ initialState hm

answer1 :: Bool -> IO Int
answer1 = answer 23 $ maxLength . parseInput

-- Part 2

charToTile' :: Char -> Tile
charToTile' c = case c of
    '.' -> Path
    '^' -> Path 
    'v' -> Path 
    '>' -> Path 
    '<' -> Path 
    _   -> Forest
    
parseInput' :: String -> HikeMap
parseInput' s = M.fromList $ do
    (y,l) <- zip [0..] $ lines s
    (x,c) <- zip [0..] l
    return (V2 y x, charToTile' c)

answer2 :: Bool -> IO Int
answer2 = answer 23 $ maxLength . parseInput'

-- Takes too long
