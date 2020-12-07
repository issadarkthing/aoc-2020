


type Position = (Int, Int)
type World = [String]

slopes :: [Position]
slopes = [(1, 1)
         ,(3, 1)
         ,(5, 1)
         ,(7, 1)
         ,(1, 2)]

createMove :: Position -> Position -> Position
createMove (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


buildPath :: (Position -> Position) -> Int -> Position -> [Position]
buildPath _ 0 pos = [pos]
buildPath m i pos = pos : buildPath m (i - 1) (m pos)

-- get the value from map based on position
getVal :: World -> Position -> Char
getVal map' (x, y) = cycle l !! x
    where l = map' !! y

solve :: World -> (Position -> Position) -> Int
solve world m = length $ filter (== '#') path
    where yStep = snd $ m (0, 0)
          step = (pred $ length world) `div` yStep
          path = map (getVal world) $ buildPath m step (0, 0)

main :: IO ()
main = do
    world <- lines <$> readFile "input.txt"
    putStr "Part 1: "
    print $ solve world (createMove (3, 1))
    putStr "Part 2: "
    print $ product $ map (solve world . createMove) slopes
    return ()
