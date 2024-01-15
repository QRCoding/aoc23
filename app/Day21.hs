{-# LANGUAGE TemplateHaskell #-}
module Day21 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Set (Set)
import qualified Data.Set as S

import Linear.V2 ( V2(..) )
import Linear.Vector ((^+^))

import Control.Lens (makeLenses, (%=), (+=), use, (^.))
import Control.Monad.State ( unless, State, execState )

import Problem (answer)

type Pos = V2 Int
data Tile = Rock | Plot | Start deriving (Eq, Show)

type Garden = Map Pos Tile

data GardenerState = GardenerState { _steps :: Int, _reach :: Set Pos }

makeLenses ''GardenerState

-- Input parsing

charToTile :: Char -> Tile
charToTile '.' = Plot
charToTile 'S' = Start
charToTile _   = Rock

parseInput :: String -> (Garden, Pos)
parseInput s = (g, p) 
    where g = M.fromList 
           . concatMap (\(y,l) -> zipWith (\x c -> (V2 y x, charToTile c)) [0..] l) 
           . zip [0..] $ lines s
          p = head . M.keys . M.filter (== Start) $ g

-- Logic

directions :: [Pos]
directions = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

neighbors :: Garden -> Pos -> Set Pos
neighbors g p = S.fromList $ filter (\v -> M.lookup v g `elem` [Just Plot, Just Start]) 
                             [p ^+^ q | q <- directions]

walk :: Garden -> State GardenerState ()
walk g = do
    reach %= S.unions . S.map (neighbors g)
    steps += 1
    s <- use steps
    unless (s >= 64) $ walk g

initialState :: Pos -> GardenerState
initialState p = GardenerState { _steps = 0, _reach = S.singleton p }

result :: (Garden, Pos) -> Int 
result (g,p) = S.size . (^. reach) . execState (walk g) $ initialState p

answer1 :: Bool -> IO Int
answer1 = answer 21 $ result . parseInput 

-- Part 2


