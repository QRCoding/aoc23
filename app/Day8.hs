{-# LANGUAGE LambdaCase #-}

module Day8 (answer1) where

import Problem (answer)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict

data Instruction = L | R deriving (Eq, Show)
type Network     = Map String (String, String)
type Position    = String
type NetworkData = ([Instruction], Int, [Position], Network, Int)

-- Parsing input

parseInput :: String -> ([Instruction], Network)
parseInput s = (instructions, network)
  where instructions = parseInstructions . head . lines $ s
        network      = parseNetwork . drop 2 . lines $ s

parseInstructions :: String -> [Instruction]
parseInstructions = map (\case 'L' -> L; _ -> R)

parseNetwork :: [String] -> Network
parseNetwork = Map.fromList . map parseLine
  where parseLine = triple . filter (/= "") . splitOn " " . filter (`notElem` "=(,)")
        triple [a,b,c] = (a,(b,c))
        triple _       = ("",("",""))

-- Part 1

move :: Instruction -> Position -> Network -> Maybe Position
move i p n = do (l,r) <- Map.lookup p n
                return $ case i of L -> l; R -> r

findEnd :: Position -> [Instruction] -> Int -> Network -> Maybe (Int, Position)
findEnd _ [] _ _         = Nothing
findEnd p (i:is) steps n = case move i p n of
    Nothing    -> Nothing
    Just p'    -> if last p' == 'Z' then Just $ (steps + 1, p')
                  else findEnd p' is (steps + 1) n

totalSteps :: [Position] -> ([Instruction], Network) -> [(Int,Position)]
totalSteps ps (is,n) = do
  pos <- ps
  return . fromMaybe (0,"") $ findEnd pos (cycle is) 0 n

startPos :: Network -> [Position]
startPos = filter ((=='A') . last) . Map.keys

firstCycle :: ([Instruction], Network) -> [(Int, Position)]
firstCycle (i,n) = totalSteps (startPos n) (i,n)

lcmAll :: [Int] -> Int
lcmAll = foldl1 lcm

answer1 :: Bool -> IO Int 
answer1 = answer 8 $ lcmAll . map fst . firstCycle . parseInput
