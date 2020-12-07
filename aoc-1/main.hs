

readInt :: String -> Int
readInt = read

part1 :: [Int] -> Int
part1 input = head [x * y | x <- input, y <- input, (x + y) == 2020]

part2 :: [Int] -> Int
part2 input = 
    head [x * y * z | x <- input, y <- input, z <- input, (x + y + z) == 2020]

main :: IO ()
main = do
    input <- map readInt <$> lines <$> readFile "input.txt"
    putStr "Part 1: "
    print $ part1 input
    putStr "Part 2: "
    print $ part2 input
    return ()
