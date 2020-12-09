import           Data.List
import           Data.List.Extra


readInt :: String -> Int
readInt = read


isValid :: [Int] -> Int -> Bool
isValid xs v = v `elem` [ x + y | x <- xs, y <- xs, x /= y ]

findNotValid :: [Int] -> [Int] -> Int
findNotValid preamble (x : xs)
    | not $ isValid preamble x = x
    | otherwise                = findNotValid ((tail preamble) ++ [x]) xs


between :: Int -> Int -> [a] -> [a]
between low high = drop low . take high

main :: IO ()
main = do
    input <- map readInt <$> lines <$> readFile "./input.txt"
    let (preamble, rest) = splitAt 25 input
        part1            = findNotValid preamble rest
    putStr "Part 1: "
    print $ part1
    putStr "Part 2: "
    print
        $ head
        $ [ let res = between x y input
                x1 = maximum res
                x2 = minimum res
            in x1 + x2
          | x <- [0 .. (length input)]
          , y <- [0 .. (length input)]
          , (sum $ between x y input) == part1
          ]
    return ()
