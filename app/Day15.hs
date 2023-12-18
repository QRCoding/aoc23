{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleContexts #-}
module Day15 where

import Data.Char (ord, digitToInt)
import Data.List.Split (splitOn)
import Problem (answer)
import Control.Monad.State (State, execState, put, get)
import Control.Lens
    ( makeLenses,
      (^.),
      use,
      (.=),
      Traversal', ix, (%~) )
import Control.Lens.Setter ((%=))
import Data.List (findIndex)
import Control.Lens.Combinators (element)

-- Part 1

hash :: String -> Int
hash = foldl step 0
    where step value c = (`mod` 256) . (*17) . (+ ord c) $ value

parseInput :: String -> [String]
parseInput = splitOn "," . concat . lines

answer1 :: Bool -> IO Int
answer1 = answer 15 $ sum . map hash . parseInput

-- Part 2

data MyLens  = MyLens { _label  :: Label, _focal :: FocalLength } deriving Show
data Box     = Box    { _boxNumber :: BoxNumber, _myLenses :: [MyLens] } deriving Show
data Boxes   = Boxes  { _boxes  :: [Box]} deriving Show

type BoxNumber   = Int
type Label       = String
type FocalLength = Int

data Operation   = Remove | Replace FocalLength deriving Show
type Instruction = (Label, Operation)

parseInstruction :: String -> Instruction
parseInstruction s
    | '-' `elem` s = (init s, Remove)
    | otherwise    = (init . init $ s, Replace . digitToInt . last $ s)

makeLenses ''MyLens
makeLenses ''Box
makeLenses ''Boxes

initialState :: Boxes
initialState = Boxes {_boxes = map (\n -> Box { _boxNumber = n, _myLenses = [] }) [0..255] }

box :: Int -> Traversal' Boxes [MyLens]
box n = boxes . ix n . myLenses

removeLens :: (Label, BoxNumber) -> State Boxes ()
removeLens (l,n) = box n %= filter (\l' -> l' ^. label /= l)

replaceLens :: (MyLens, BoxNumber) -> State Boxes ()
replaceLens (l,n) = do
    ls <- use $ box n
    case findIndex (\l' -> l' ^. label == l ^. label) ls of
        Nothing -> box n %= (l:)
        Just i  -> box n . ix i .= l

handleInstruction :: Instruction -> State Boxes ()
handleInstruction (l,o) = case o of
    Remove    -> removeLens  (l, hash l)
    Replace f -> replaceLens ( MyLens { _label = l, _focal = f}, hash l)

handleInstructions :: [Instruction] -> Boxes
handleInstructions is = execState (mapM_ handleInstruction is) initialState

nonEmptyBoxes :: Boxes -> Boxes
nonEmptyBoxes = boxes %~ filter (\b -> not . null $ b ^. myLenses)

boxPower :: Box -> Int
boxPower b = sum $ zipWith (\i l -> (1 + n) * i * l ^. focal) [t,t-1..1] ls
    where n  = b ^. boxNumber
          ls = b ^. myLenses
          t  = length ls 

totalPower :: Boxes -> Int
totalPower bs = sum . map boxPower $ bs ^. boxes

answer2 :: Bool -> IO Int
answer2 = answer 15 $ totalPower . handleInstructions . map parseInstruction . parseInput



