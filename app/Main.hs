module Main where

-- import System.Random
-- import qualified Data.Map as M
-- import Day17 (HeatMap, minHeat, answer1)
-- import Control.Monad (replicateM)
--
-- main :: IO () 
-- main = do
--     ns <- replicateM 19881 $ randomRIO (1, 9) :: IO [Int]
--     -- print ns
--     h  <- minHeat $ makeHeatMap ns
--     print h
--
-- makeHeatMap :: [Int] -> HeatMap
-- makeHeatMap ns = M.fromList $ concat [ [ ((y,x),ns!!(y*140+x)) | x <- [0..140]] | y <- [0..140]]

import Day21 (answer1)

main :: IO ()
main = answer1 True >>= print
