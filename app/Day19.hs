{-# LANGUAGE TemplateHaskell #-}

module Day19 (answer1, answer2) where

-- Imports

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Lens (makeLenses, (^.), (%~))
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.List.Split (splitOn)
import Data.Char (isDigit)

import Problem (answer)
import Data.List (findIndices)

-- Types

data Destination = Name Name | Accepted | Rejected deriving (Eq, Show, Ord, Read)
type Name = String
data Category = X | M | A | S deriving (Eq, Show, Ord, Read)
data Op = Bigger | Smaller | BiggerEq | SmallerEq deriving (Eq, Show)

newtype Workflow = Workflow { _rules :: [Rule] } deriving Show
type System   = Map Name Workflow
data Rule     = Rule { _condition :: Maybe Condition, _destination :: Destination } deriving Show
type Part     = Map Category Int
data Condition = Condition { _category :: Category, _op :: Op, _value :: Int } deriving (Eq, Show)

makeLenses ''Workflow
makeLenses ''Rule
makeLenses ''Condition

-- Input parsing

charToCategory :: Char -> Category
charToCategory c = case c of 'x' -> X; 'm' -> M; 'a' -> A; _ -> S

parseRule :: String -> Rule
parseRule s = case splitOn ":" s of
    [s1]    -> Rule { _condition = Nothing , _destination = case s1 of "A" -> Accepted; "R" -> Rejected; _ -> Name s1 }
    [s1,s2] -> Rule { _condition = Just $ parseCondition s1
                    , _destination = case s2 of "A" -> Accepted
                                                "R" -> Rejected; _ -> Name s2 }
    _       -> rejectR

parseCondition :: String -> Condition
parseCondition s  = Condition { _category = c, _op = o, _value = v }
    where c = charToCategory $ head s
          v = read . filter isDigit $ s
          o = case head . tail $ s of '>' -> Bigger ;_ -> Smaller

parseWorkflow :: String -> Workflow
parseWorkflow s = Workflow { _rules = map parseRule $ splitOn "," s }

parseLine :: String -> (Name, Workflow)
parseLine s = (name, parseWorkflow s')
    where name = takeWhile (/= '{') s
          s'   = init . tail . dropWhile (/= '{') $ s

parseSystem :: String -> System
parseSystem s = M.fromList . map parseLine $ lines s

parseCategory :: String -> (Category, Int)
parseCategory s = (charToCategory $ head s, read . filter isDigit $ s)

parsePart :: String -> Part
parsePart = M.fromList . map parseCategory . splitOn "," . tail . init

parseInput :: String -> (System, [Part])
parseInput s = (system, parts)
    where ls = splitOn [""] $ lines s
          system = parseSystem . unlines . head $ ls
          parts  = map parsePart $ last ls

-- System logic

checkOp :: Op -> Int -> Int -> Bool
checkOp o n m = case o of
    Bigger  -> n > m
    Smaller -> n < m
    BiggerEq -> n >= m
    SmallerEq -> n <= m

checkCondition :: Maybe Condition -> Part -> Bool
checkCondition Nothing _ = True
checkCondition (Just cond) part = case M.lookup (cond ^. category) part of
    Nothing -> False
    Just i' -> checkOp (cond ^. op) i' v
    where v = cond ^. value

rejectR :: Rule
rejectR = Rule { _condition = Nothing, _destination = Rejected }

rejectW :: Workflow
rejectW = Workflow { _rules = [ rejectR ] }

getDestination :: Part -> Workflow -> Destination
getDestination p w = head . mapMaybe (\r -> if checkCondition (r ^. condition) p
                                            then Just $ r ^. destination
                                            else Nothing) $ w ^. rules

applyNameSystem :: Name -> Part -> System -> Destination
applyNameSystem n p s = case getDestination p w of
    Accepted -> Accepted
    Rejected -> Rejected
    Name n'  -> applyNameSystem n' p  s
    where w = fromMaybe rejectW $ M.lookup n s

applySystem :: Part -> System -> Destination
applySystem = applyNameSystem "in"

rating :: Part -> Int
rating p = sum $ M.elems p

totalRating :: (System, [Part]) -> Int
totalRating (s,ps) = sum . map rating . filter (\p -> applySystem p s == Accepted) $ ps

answer1 :: Bool -> IO Int
answer1 = answer 19 $ totalRating . parseInput

-- Part 2

-- Find which workflows have a certain destination in their rule list

convert :: [(a,[[Condition]])] -> [(a, [Condition])]
convert = concatMap convert'
    where convert' (n, ccs) = [ (n,cs) | cs <- ccs]

findWorkflows :: Destination -> System -> [(Destination, [Condition])]
findWorkflows d s = convert . M.toList . M.mapKeys Name $  M.map (getTo d) s

flipC :: Maybe Condition -> Maybe Condition
flipC = fmap $ op %~ flipOp
    where flipOp Bigger = SmallerEq
          flipOp Smaller = BiggerEq
          flipOp SmallerEq = Bigger
          flipOp BiggerEq = Smaller

getTo :: Destination -> Workflow -> [[Condition]]
getTo d w = do
    i <- is
    return . catMaybes $ [ flipC $ (rs!!j) ^. condition  | j <- [0..i-1]] ++ [(rs!!i) ^. condition]
    where rs = w ^. rules
          is = findIndices (\r -> r ^. destination == d) rs

findConditions :: Destination -> System -> [[Condition]]
findConditions (Name "in") _ = [[]]
findConditions d s = do
    (d',cs) <- findWorkflows d s
    let css' = findConditions d' s
    cs' <- css'
    return $ cs ++ cs'

catCombinations :: [Condition] -> Category -> Int
catCombinations cs cat = length . filter (\n -> all (\c -> checkOp (c ^. op) n (c ^. value)) cs') $ [1..4000]
    where cs' = filter ((==cat) . (^. category) ) cs
    
combinations :: [Condition] -> Integer
combinations cs = product . map (fromIntegral . catCombinations cs) $ [X,M,A,S] 

totalCombinations :: [[Condition]] -> Integer
totalCombinations = sum . map combinations

answer2 :: Bool -> IO Integer
answer2 = answer 19 $ totalCombinations . findConditions Accepted . fst . parseInput
