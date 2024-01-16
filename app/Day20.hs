{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day20 (makeHandler, parseInput, pulses, initialState
            , Pulse (..), stack) where

import Control.Lens (makeLenses, uses, (%=), use, (+=), (.=), (&), _1, _2)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.List.Split (splitOn)
import Control.Monad.IO.Class (liftIO)
import System.Console.ANSI (clearScreen)
import Control.Monad (when)

data Pulse = Low | High deriving (Eq, Show)
type Name = String
type Source = String
type Destination = String
type Packet = (Source, Pulse, Destination)
type Stack = [Packet]
type ModuleHandler = Map Name Module

data Module = FlipFlop Pulse [Name] 
            | Conjunction (Map Name Pulse) [Name] 
            | Broadcast [Name] 
            | Output deriving Show

data ModuleState = ModuleState { _handler :: ModuleHandler
                               , _stack :: Stack 
                               --, _count :: (Int, Int)
                               , _presses :: Int } deriving Show

makeLenses ''ModuleState

flipP :: Pulse -> Pulse
flipP High = Low
flipP Low  = High
              
pulses :: StateT ModuleState IO Int
pulses = do
    n <- uses stack null
    if n 
    then do
        p <- use presses
        {-if p >= 1000 (this is only used for part 1)
        then do (lo, hi) <- use count
                return $ lo * hi
        else do-} 
        stack .= [("button", Low, "broadcaster")]
        when (p `mod` 100000 == 0) $ do
            liftIO clearScreen
            liftIO . print $ "Presses: " ++ show p
        presses += 1   
        pulses
    else do
        {-s <- uses stack length
        liftIO clearScreen
        liftIO . print $ "Stack size: " ++ show s-}
        (source, pulse, dest) <- uses stack head
        --liftIO . putStrLn $ "Sending " ++ show pulse ++ " pulse from " ++ source ++ " to " ++ dest
        stack %= tail
        {-if pulse == Low
            then count . _1 += 1
            else count . _2 += 1-}
        mm <- uses handler $ M.lookup dest
        case mm of 
            -- Here we sent a pulse to rx
            Nothing -> case (pulse, dest) of 
                (Low, "rx") -> use presses
                _           -> pulses
                -- liftIO . print $ "Could not find " ++ dest
            Just m -> do
                let (ps,m') = handlePulse (source, pulse) m 
                handler %= M.insert dest m'
                let packets = map (\(p,d) -> (dest, p, d) ) ps
                stack %= (++ packets)
                pulses

handlePulse :: (Source, Pulse) -> Module -> ([(Pulse, Destination)], Module)
handlePulse (_, pulse) m@(Broadcast ds) = (map (pulse,) ds, m)
handlePulse (_, pulse) m@(FlipFlop s ds) = case pulse of
    High -> ([], m)
    Low  -> (map (flipP s,) ds , FlipFlop (flipP s) ds)
handlePulse (source, pulse) (Conjunction mem ds) =
    (map (pulse',) ds,  Conjunction mem' ds)
    where mem'   = M.insert source pulse mem
          pulse' = if all (== High) $ M.elems mem' then Low else High
handlePulse (_,_) Output = ([], Output)

parseModule :: String -> (Name, Module)
parseModule l = case head l of
    '%' -> (name, FlipFlop Low ds)
    '&' -> (name, Conjunction M.empty ds)
    _  -> ("broadcaster", Broadcast ds)
    where name = takeWhile (/= ' ') . tail $ l
          ds   = splitOn ", " . tail . tail . dropWhile (/= '>') $ l 

parseInput :: String -> [(Name, Module)]
parseInput = map parseModule . lines

isConjunction :: Module -> Bool
isConjunction (Conjunction _ _) = True
isConjunction _                 = False

destinations :: Module -> [Name]
destinations (FlipFlop _ ds) = ds
destinations (Conjunction _ ds) = ds
destinations (Broadcast ds) = ds
destinations Output = []

makeHandler :: [(Name, Module)] -> ModuleHandler
makeHandler l = M.fromList $ notCon ++ con' -- ++ [("output", Output)]
    where con    = filter (isConjunction . snd) l
          notCon = filter (not . isConjunction . snd) l
          con' = map (\(n,c) -> 
            let names = map fst . filter ((n `elem`) . destinations . snd) $ l
            in  (n,Conjunction (M.fromList $ map (,Low) names) (destinations c))) con

initialState :: ModuleHandler -> ModuleState
initialState h = ModuleState
    { _handler = h, _stack = [] ,{- _count = (0,0) ,-} _presses = 0 }