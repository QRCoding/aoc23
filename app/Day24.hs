{-# LANGUAGE TupleSections #-}
module Day24 where

import Linear.V2 ( R1(_x), R2(_y), V2 (..))
import Linear.V3 (R2(_xy), V3 (..))
import Linear.Vector ((^+^), (*^))
import Control.Lens ((^.))
import GHC.Float (int2Float)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Pos = V2 Int
type Vel = V2 Int

det :: Vel -> Vel -> Int
det v1 v2 = v1 ^. _x * v2 ^. _y - v1 ^. _y * v2 ^. _x

intersection :: (Pos, Vel) -> (Pos, Vel) -> Maybe (V2 Float)
intersection (p1,v1) (p2,v2) = case det v1 v2 of
    0 -> Nothing
    d -> if l < 0 || m < 0
         then Nothing
         else Just $ (int2Float <$> p1) ^+^ (l *^ (int2Float <$> v1))
         where dl = v2 ^. _y * (p1 ^. _x - p2 ^. _x) - v2 ^. _x * (p1 ^. _y - p2 ^. _y)
               l  = - int2Float dl / int2Float d
               dm = - v1 ^. _y * (p1 ^. _x - p2 ^. _x) + v1 ^. _x * (p1 ^. _y - p2 ^. _y)
               m  = int2Float dm / int2Float d

pair :: [V2 Int] -> (V2 Int,V2 Int)
pair [v1,v2] = (v1,v2)
pair _       = (V2 0 0, V2 0 0)

parseLine :: String -> (V2 Int, V2 Int)
parseLine = pair
          . map ((^. _xy) . (read :: String -> V3 Int) . ("V3 " ++) . filter (/= ','))
          . splitOn " @ "

parseInput :: String -> [(Pos, Vel)]
parseInput = map parseLine . lines

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (a:as) = map (a,) as ++ pairs as

checkBounds :: Float -> Float -> V2 Float -> Bool
checkBounds l u v = l <= v ^. _x
                 && v ^. _x <= u
                 && l <= v ^. _y
                 && v ^. _y <= u

inArea :: [(Pos, Vel)] -> Int
inArea = length
       . filter (checkBounds (int2Float 200000000000000) (int2Float 400000000000000))
       . mapMaybe (uncurry intersection) . pairs
