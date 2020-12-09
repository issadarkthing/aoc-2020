import           Data.List.Split
import           Data.List
import           Data.Maybe

readInt :: String -> Int
readInt ('+' : xs) = readInt xs
readInt xs         = read xs

parse :: String -> (String, Int)
parse str = (code, readInt arg) where (code : arg : _) = splitOn " " str

-- exec codes position acc visitedIndex (acc, isTerminated)
exec :: [(String, Int)] -> Int -> Int -> [Int] -> (Int, Bool)
exec codes i acc ls
    | i `elem` ls = (acc, False)
    | i == (length codes) = (acc, True)
    | otherwise = case code of
        "jmp" -> exec codes (i + arg) acc (i : ls)
        "acc" -> exec codes (i + 1) (acc + arg) (i : ls)
        "nop" -> exec codes (i + 1) acc (i : ls)
    where (code, arg) = codes !! i

exec' :: [(String, Int)] -> (Int, Bool)
exec' input = exec input 0 0 []

replaceAt :: Int -> (a -> a) -> [a] -> [a]
replaceAt index replacer ls =
    [ if index == i then replacer x else x | (x, i) <- zip ls [0 ..] ]

replaceCode :: String -> String
replaceCode "jmp" = "nop"
replaceCode "nop" = "jmp"

mapFst :: (a -> a) -> (a, b) -> (a, b)
mapFst f (a, b) = (f a, b)

isJmpOrNop :: String -> Bool
isJmpOrNop code = code == "jmp" || code == "nop"

modify :: ([Int], [(String, Int)]) -> ([Int], [(String, Int)])
modify (ls, codes) = ((index : ls), replaceAt index (mapFst replaceCode) codes)
  where
    index =
        fromJust
            $ findIndex (\(i, (code, _)) -> (notElem i ls) && isJmpOrNop code)
            $ zip [0 ..] codes

fixCodes :: ([Int], [(String, Int)]) -> Int
fixCodes source@(ls, input) = if terminated then acc else fixCodes (ls', input)
  where
    (ls', input'    ) = modify source
    (acc, terminated) = exec' input'

main :: IO ()
main = do
    input <- map parse <$> lines <$> readFile "./input.txt"
    putStr "Part 1: "
    print $ fst $ exec' input
    putStr "Part 2: "
    print $ fixCodes ([], input)
    return ()
