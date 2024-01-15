{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Day22 where

import Linear.V3
import Linear.V2
import Linear.Vector ((^+^))

import Data.Set (Set)
import qualified Data.Set as S

import Data.PSQueue (PSQ, Binding ((:->)))
import qualified Data.PSQueue as P

import Data.Map.Strict (Map)
import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Lens ((^.), makeLenses, uses, (%=), (%~), (+=), use)
import Data.List.Split (splitOn)

import Problem (answer)

type Brick = Set (V3 Int)
data BrickState = BrickState { _falling :: PSQ Brick Int
                             , _resting :: Set Brick
                             , _fallen :: Int
                             , _heightMap :: Map (V2 Int) Int}

makeLenses ''BrickState

-- Input parsing

getBrick :: V3 Int -> V3 Int -> Brick
getBrick (V3 x y z) (V3 x' y' z') =
    S.fromList [V3 a b c | a <- [x..x'], b <- [y..y'], c <- [z..z']]

parseBrick :: String -> Brick
parseBrick s = getBrick (head bs) (head . tail $ bs)
    where bs = map (read . ("V3 " ++))
             . splitOn "~"
             . map (\case {',' -> ' '; c -> c}) $ s

parseInput :: String -> [Brick]
parseInput = map parseBrick . lines

-- Logic

fallB :: Brick -> Brick
fallB = S.map (^+^ V3 0 0 (-1))

height :: Brick -> Int
height = minimum . S.map (^. _z)

fall :: MaybeT (State BrickState) ()
fall = do
    b :-> oldZ <- MaybeT $ uses falling P.findMin
    falling %= P.deleteMin
    maxZ <- uses heightMap
        (\h -> S.findMax . S.map (\p -> M.findWithDefault 0 (p ^. _xy) h) $ b)
    let dist = oldZ - maxZ - 1
    let b' = S.map (_z %~ subtract dist) b
    fallen += if dist /= 0 then 1 else 0
    resting %= S.insert b'
    mapM_ (\p -> heightMap %= M.insert (p ^. _xy) (p ^. _z)) $ S.toAscList b'
    fall

initialState :: [Brick] -> BrickState
initialState bs = BrickState { _falling = P.fromList . map (\b -> b :-> height b) $ bs
                             , _resting = S.empty
                             , _heightMap = M.empty
                             , _fallen = 0 }

fallenState :: [Brick] -> Set Brick
fallenState bs = (^. resting) . execState (runMaybeT fall) $ initialState bs

-- Part 1

data Dir = Up | Down

support :: Dir -> Set Brick -> Brick -> Set Brick
support d s b = S.filter (\c -> (c /= b) && not (S.disjoint c b')) s
    where b' = S.map (_z %~ (case d of Up -> (+1); Down -> subtract 1)) b

canRemove :: Set Brick -> Brick -> Bool
canRemove s b = all (> 1) . S.map (S.size . support Down s) $ support Up s b

total :: Set Brick -> Int
total s = S.size $ S.filter (canRemove s) s

answer1 :: Bool -> IO Int
answer1 = answer 22 $ total . fallenState . parseInput

-- Part 2

totalFallen :: [Brick] -> Int
totalFallen bs = (^. fallen) . execState (runMaybeT fall) $ initialState bs

wouldFall :: Brick -> Set Brick -> Int
wouldFall b = totalFallen . S.toList . S.delete b

allFall :: Set Brick -> Int
allFall s = sum . map (`wouldFall` s) $ S.toList s

answer2 :: Bool -> IO Int
answer2 = answer 22 $ allFall . fallenState . parseInput

-- Very slow, optimize by not running the simulation for every brick?
-- answer: 70609
