module Day2 (answer1, answer2) where
    
import Data.Char (isDigit)
import Data.List.Split (splitOn)

data Color = Red | Green | Blue deriving (Eq, Show)
type Draw = [(Int, Color)]
type Game = [Draw]

-- Parsing input 

input :: IO String
input = readFile "data/day2input.txt"

parseInput :: String -> [Game]
parseInput = map parseLine . lines

parseLine :: String -> Game
parseLine = map parseDraw . splitOn "; " . last . splitOn ": " . dropWhile (not . isDigit)

parseDraw :: String -> Draw
parseDraw = map parseColor . splitOn ", "

parseColor :: String -> (Int, Color)
parseColor s = (number s, color s) 
    where number = read . head . splitOn " "
          color  = toColor . last . splitOn " "

toColor :: String -> Color
toColor s = case s of
    "red" -> Red
    "green" -> Green
    _ -> Blue

-- Determining if game is valid

isValid :: Game -> Bool
isValid = all drawValid
    where drawValid = all valid
          valid (n,c) = case c of
                            Red -> n <= 12
                            Green -> n <= 13
                            Blue -> n <= 14

gameScore :: [Game] -> Int
gameScore gs = sum $ zipWith (\g n -> if isValid g then n else 0) gs [1..length gs]

answer1 :: IO Int
answer1 = gameScore . parseInput <$> input

-- Determining fewest possible number of each cube

fewest :: Game -> Draw
fewest g = [(few c g, c) | c <- [Red, Green, Blue]] 
    where few c g' = let l = concatMap (map fst . filter ((== c) . snd)) g'
                     in if null l then 0 else maximum l

power :: Draw -> Int
power = product . map fst

totalPower :: [Game] -> Int
totalPower = sum . map (power . fewest)

answer2 :: IO Int
answer2 = totalPower . parseInput <$> input