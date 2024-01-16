{-# LANGUAGE TemplateHaskell #-}
module Day25 where

import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad.State

import Control.Lens

type Component = String
type Connection = [Component]

data ConnectState = ConnectState { _connections :: Set Connection, _components :: Set Component }
makeLenses ''ConnectState

parseInput :: String -> Set Connection
parseInput = S.unions . map ((\(c,cs) -> S.fromList $ map (\c' -> [c,c']) cs) . parseLine) . lines

parseLine :: String -> (Component, [Component])
parseLine = (\l -> (head l, tail l)) . words . filter (/= ':')

connect :: State ConnectState ()
connect = do
    comps <- use components
    conns <- use connections
    let new = S.filter ((==1) . length . filter (`S.member` comps)) conns
    if null new then return ()
    else do components  %= S.union (S.map (head . filter (`S.notMember` comps)) new)
            connections %= (`S.difference` new)
            connect

compSize :: Component -> Set Connection -> Int
compSize c s = S.size . (^. components) . execState connect $
    ConnectState { _components  = S.singleton c
                 , _connections = s }

setsOf :: Ord a => Int -> Set a -> Set (Set a)
setsOf 0 _ = S.singleton S.empty
setsOf n s = S.unions . S.map (\s' -> S.map (`S.insert` s') $ S.difference s s') $ setsOf (n-1) s
