
xor :: Bool -> Bool -> Bool
xor True x  = not x
xor False x = x

isOnValidPosition :: Int -> Char -> String -> Bool
isOnValidPosition pos delim target = c == delim
    where c = target !! pos

splitOn :: Char -> String -> [String]
splitOn delim [] = [""]
splitOn delim (x:xs)
    | x == delim = "" : rest
    | otherwise  = (x: head rest) : tail rest
    where rest = splitOn delim xs

readInt :: String -> Int
readInt = read

getMinMax :: String -> (Int, Int)
getMinMax x = (v !! 0, v !! 1)
    where v = map readInt $ splitOn '-' x

countOccurrences :: Char -> String -> Int
countOccurrences x = length . filter (== x)

isValidP1 :: (Int, Int) -> Char -> String -> Bool
isValidP1 (min, max) letter password = result >= min && result <= max
    where result = countOccurrences letter password

isValidP2 :: (Int, Int) -> Char -> String -> Bool
isValidP2 (min, max) letter password = check min `xor` check max
    where check x = isOnValidPosition (x - 1) letter password

isValidWrapper :: ((Int, Int) -> Char -> String -> Bool) -> String -> Bool
isValidWrapper pred target = pred minMax letter password
    where splitted = splitOn ' ' target
          minMax = getMinMax $ splitted !! 0
          letter = splitted !! 1 !! 0
          password = splitted !! 2


main :: IO ()
main = do
    input <- lines <$> readFile "./input.txt"
    putStr "Part 1: "
    print $ length $ filter (isValidWrapper isValidP1) input
    putStr "Part 2: "
    print $ length $ filter (isValidWrapper isValidP2) input
    return ()
