module Problem (answer) where

answer :: Int -> (String -> a) -> Bool -> IO a
answer d f b = do
    let file = "data/day" ++ show d ++ (if b then "input" else "test") ++ ".txt"
    input <- readFile file
    return . f $ input
