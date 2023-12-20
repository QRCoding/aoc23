{-# LANGUAGE TemplateHaskell #-}

module Day18 (answer1, test, answer2) where

import Problem (answer)

import Data.Set (Set, unions)
import qualified Data.Set as S
import Control.Monad.State (State, execState, get, put, unless)
import Control.Lens (makeLenses, use, (%=), (+=), (^.))
import Linear.V2 (V2 (..), _x, _y)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

type Pos       = V2 Int
data Direction = U | D | L | R deriving (Eq, Ord, Show, Read)
type Trench    = Set Pos
type Instruction = (Direction, Int)

data Digger = Digger { _trench :: Trench, _pos :: Pos }

makeLenses ''Digger

-- Part 1

toPos :: Direction -> Pos
toPos U = V2 0 1
toPos D = V2 0 (-1)
toPos L = V2 (-1) 0
toPos R = V2 1 0

instruction :: Instruction -> State Digger ()
instruction (_,n) | n <= 0 = do return ()
instruction (d,n) = do
    pos += toPos d
    p <- use pos
    trench %= S.insert p
    instruction (d,n-1)

parseInput :: String -> [Instruction]
parseInput s = do
    l <- lines s
    let ss = splitOn " " l
    return (read $ head ss, read . head . tail $ ss)

draw :: Trench -> String
draw t = unlines [ [  case (y,x) of
                        (0,0) -> 'S'
                        (_,_) -> if V2 y x `S.member` t then '#' else '.'
                    | x <- [xmin..xmax] ] | y <- [ymin..ymax] ]
    where (ymin, ymax) = (fromMaybe 0 . S.lookupMin $ S.map (^. _x) t, fromMaybe 0 . S.lookupMax $ S.map (^. _x) t)
          (xmin, xmax) = (fromMaybe 0 . S.lookupMin $ S.map (^. _y) t, fromMaybe 0 . S.lookupMax $ S.map (^. _y) t)

initial :: Digger
initial = Digger { _trench = S.empty, _pos = V2 0 0 }

getTrench :: [Instruction] -> Trench
getTrench is = (^. trench) $ execState (mapM_ instruction is) initial

neighbors :: Pos -> Set Pos
neighbors p  = S.fromList [p + V2 x y | x <- [- 1 .. 1], y <- [- 1 .. 1], (x, y) /= (0, 0)]

interior :: Trench -> State Trench ()
interior edge = do
    i <- get
    let ns = unions . S.map (S.filter (\n -> not (n `S.member` edge) && not (n `S.member` i)) . neighbors) $ i
    let i' = S.union i ns
    put i'
    unless (S.size i == S.size i') $ interior edge

interiorStart :: Trench
interiorStart = S.singleton (V2 0 1)

getInterior :: Trench -> Trench
getInterior e = execState (interior e) interiorStart

getSize :: [Instruction] -> Int
getSize is = S.size t + S.size i
    where t = getTrench is
          i = getInterior t

answer1 :: Bool -> IO Int
answer1 = answer 18 $ getSize .  parseInput

test :: IO ()
test = do
    is <- parseInput <$> readFile "data/day18input.txt"
    let t = getTrench is
    let i = getInterior t
    putStr . draw $ S.union t i

-- Part 2
data Digger' = Digger' { _area' :: Integer, _trench' :: Integer, _pos' :: Pos }

makeLenses ''Digger'


instruction' :: Instruction -> State Digger' ()
instruction' (_,n) | n <= 0 = do return ()
instruction' (d,n) = do
    pos' += V2 (n * (toPos d ^. _x)) (n * (toPos d ^. _y))
    p <- use pos'
    area' += case d of
        U -> fromIntegral $ n * (p ^. _x)
        D -> fromIntegral $ n * (-(p ^. _x))
        _ -> 0
    trench' += fromIntegral n

initial' :: Digger'
initial' = Digger' { _area' = 0, _trench' = 0, _pos' = V2 0 0 }

getArea :: [Instruction] -> Integer
getArea is = abs a + c `div` 2 + 1
    where d = execState (mapM_ instruction' is) initial'
          a = d ^. area'
          c = fromIntegral $ d ^. trench'

hexToInstruction :: String -> Instruction
hexToInstruction s = (i,h)
    where h = read $ "0x" ++ init s
          i = case read [last s] :: Int of
                0 -> R
                1 -> D
                2 -> L
                _ -> U

parseInput' :: String -> [Instruction]
parseInput' s = do
    l <- lines s
    let h = takeWhile (/= ')') . tail . dropWhile (/= '#') $ l
    return $ hexToInstruction h

answer2 :: Bool -> IO Integer
answer2 = answer 18 $ getArea . parseInput'
