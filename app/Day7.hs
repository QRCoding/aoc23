module Day7 (answer1) where

import Problem (answer)

import Data.Char (digitToInt, isDigit)
import Data.List (sort, group, sortOn, sortBy)
import Data.Ord  (Down (Down))
import Data.List.Split (splitOn)

data Card = Joker | Number Int | Jack | Queen | King | Ace deriving (Eq, Ord, Show)
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Show)
newtype Hand = Hand {cards :: [Card]} deriving (Show, Eq)

-- Part 1

toCard :: Char -> Card
toCard c | isDigit c = Number . digitToInt $ c
         | otherwise = case c of
                           'T' -> Number 10
                           'J' -> Joker -- Change to Jack for Part 1
                           'Q' -> Queen
                           'K' -> King
                           'A' -> Ace
                           _   -> error "Invalid card."

getHand :: String -> Hand
getHand = Hand . map toCard

getType :: Hand -> HandType
getType (Hand h)
    | length h /= 5 = error "Invalid hand size."
    | otherwise = case sortOn Data.Ord.Down . map length . group . sort $ h of
                     [5]     -> FiveOfAKind
                     [4,1]   -> FourOfAKind
                     [3,2]   -> FullHouse
                     [3,1,1] -> ThreeOfAKind
                     [2,2,1] -> TwoPair
                     2:_     -> OnePair
                     _       -> HighCard

instance Ord Hand where
    h1 `compare` h2
        | getTypeJoker h1 < getTypeJoker h2 = LT -- Change to getType for Part 1
        | getTypeJoker h1 > getTypeJoker h2 = GT
        | otherwise = case filter (/= EQ) $ zipWith compare (cards h1) (cards h2) of
                         []  -> EQ
                         c:_ -> c

parseInput :: String -> [(Hand, Int)]
parseInput l = [(hand s, bid s) | s <- lines l]
    where hand = getHand . head . splitOn " "
          bid  = read . last . splitOn " "

winnings :: [(Hand, Int)] -> Int
winnings = sum . zipWith (*) [1..] . map snd . sortBy (\(c,_) (c',_) -> c `compare` c')

answer1 :: Bool -> IO Int
answer1 = answer 7 $ winnings . parseInput

-- Part 2

getTypeJoker :: Hand -> HandType
getTypeJoker h = case (getType h, length . filter (== Joker) . cards $ h) of
    (t, 0)        -> t
    (HighCard, _) -> OnePair
    (OnePair,  _) -> ThreeOfAKind
    (TwoPair,  1) -> FullHouse
    (TwoPair,  _) -> FourOfAKind
    (ThreeOfAKind, _) -> FourOfAKind
    _             -> FiveOfAKind