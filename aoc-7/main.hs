import Data.List.Split
import Data.List ((\\))
import Data.Maybe (fromJust)

type Rule = (String, [(String, Int)])

readInt :: String -> Int
readInt = read

parse :: String -> [Rule]
parse = map makeBag . lines

dropLast :: Int -> [a] -> [a]
dropLast i xs = take (length xs - i) xs

-- pale cyan bags contain 2 posh black bags, 4 wavy gold bags, 2 vibrant brown bags.
-- vibrant blue bags contain no other bags.
makeBag :: String -> Rule
makeBag str = (name, content)
    where (front:back:_) = splitOn " contain " str
          name = dropLast 5 front
          getContent (x:y:z:_) = (y ++ " " ++ z, readInt x)
          content = if back == "no other bags."
                       then []
                       else map (getContent . words) $ splitOn ", " back

findRule :: String -> [Rule] -> [(String, Int)]
findRule name = fromJust . lookup name

contains :: [Rule] -> String -> String -> Bool
contains rules target name = 
    if target == name 
        then True 
        else any (contains rules target . fst) bags
    where bags = findRule name rules

part1 :: [Rule] -> Int
part1 input = length $ filter (contains input "shiny gold" . fst) $ 
    filter ((/=) "shiny gold" . fst) input

part2 :: [Rule] -> Int
part2 input = pred $ countBags input ("shiny gold", 1)

countBags :: [Rule] -> (String, Int) -> Int
countBags rules (name, count) = count * (1 + totalBags)
    where bags = findRule name rules
          totalBags = sum $ map (countBags rules) bags

main :: IO ()
main = do
    input <- parse <$> readFile "./input.txt"
    putStr "Part 1: "
    print $ part1 input
    putStr "Part 2: "
    print $ part2 input
    return ()

