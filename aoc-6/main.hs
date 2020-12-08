import Data.List.Split
import Data.List


part1 :: [String] -> Int
part1 = sum . map (length . nub)

part2 :: [[String]] -> Int
part2 = sum . map (length . foldl intersect ['a'..'z'])

getInput :: String -> IO [String]
getInput file = splitOn "\n\n" <$> readFile file

main :: IO ()
main = do
    input <- getInput "./input.txt"
    putStr "Part 1: "
    print $ part1 $ map (filter (/= '\n')) input
    putStr "Part 2: "
    print $ part2 $ map words input
    return ()
