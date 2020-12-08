import Data.List ((\\))

-- Range is used to identify the lower and upper bound of a region
type Range = (Int, Int)

split :: Range -> (Range, Range)
split (low, high) = ((low, middle), (middle + 1, high))
    where middle = (low + high) `div` 2

pickSeat :: Char -> Range -> Int
pickSeat 'F' (lower, _) = lower
pickSeat 'L' (lower, _) = lower
pickSeat 'B' (_, upper) = upper
pickSeat 'R' (_, upper) = upper


findPosition :: Range -> String -> Int
findPosition seats (x:[]) = pickSeat x seats
findPosition seats (x:xs) = 
    case x of
    'F' -> findPosition lower xs
    'L' -> findPosition lower xs
    'B' -> findPosition upper xs
    'R' -> findPosition upper xs
    where (lower, upper) = split seats


findRow :: String -> Int
findRow = findPosition (0, 127)

findColumn :: String -> Int
findColumn = findPosition (0, 7)

findSeat :: String -> (Int, Int)
findSeat code = (findRow x, findColumn y)
    where (x, y) = splitAt (length code - 3) code


calcSeatId :: (Int, Int) -> Int
calcSeatId (row, column) = row * 8 + column


main :: IO ()
main = do
    input <- lines <$> readFile "./input.txt"
    let seatIds = map (calcSeatId . findSeat) input
        maxSeat = foldl1 max seatIds
        minSeat = foldl1 min seatIds
    putStr "Part 1: "
    print maxSeat
    putStr "Part 2: "
    print $ head $ [minSeat..maxSeat] \\ seatIds
