import Data.List
import Text.Read
import Data.Maybe

required = [
             "byr" 
           , "iyr" 
           , "eyr"
           , "hgt" 
           , "hcl" 
           , "ecl" 
           , "pid" 
           -- , "cid" this is optional
           ]

eyeColors = ["amb", "blu", "brn", "gry","grn","hzl","oth"]
validators = [
               ("byr", (\x -> let y = readInt x in y >= 1920 && y <= 2002)) 
             , ("iyr", (\x -> let y = readInt x in y >= 2010 && y <= 2020)) 
             , ("eyr", (\x -> let y = readInt x in y >= 2020 && y <= 2030))
             , ("hgt", isValidHeight) 
             , ("hcl", isHex) 
             , ("ecl", (flip elem eyeColors)) 
             , ("pid", (\x -> validNumber x && length x == 9)) 
             , ("cid", const True) 
             ]

isValidHeight :: String -> Bool
isValidHeight str = inRange $ (readInt $ fst splitted, snd splitted)
    where splitted = splitAt (length str - 2) str
          inRange (value, "cm") = value >= 150 && value <= 193
          inRange (value, "in") = value >= 59 && value <= 76
          inRange (_, _) = False

validNumber :: String -> Bool
validNumber x = isJust (readMaybe x :: Maybe Int)

readInt :: String -> Int
readInt = read

isHex :: String -> Bool
isHex str = head str == '#' && checkHex digits && length digits == 6
    where inRange x = x `elem` ['0'..'9'] || x `elem` ['a'..'f']
          checkHex = foldl (\acc v -> acc && inRange v) True 
          digits = tail str

splitOn :: Char -> String -> [String]
splitOn delim [] = [""]
splitOn delim (x:xs)
    | x == delim = "" : rest
    | otherwise  = (x: head rest) : tail rest
    where rest = splitOn delim xs

splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc

getAttrib :: String -> (String, String)
getAttrib str = (key, value)
    where (key:value:_) = splitOn ':' str

lookupAttrib :: [(String, String)] -> [Maybe String]
lookupAttrib xs = map (flip lookup xs) required

isValid :: [Maybe String] -> Bool
isValid l = isJust $ foldl1 (>>) l

hasAllRequired :: [(String, String)] -> Bool
hasAllRequired = isValid . lookupAttrib

generatePassports :: [String] -> [[(String, String)]]
generatePassports = map (map getAttrib) . map words

part1 :: [[(String, String)]] -> Int
part1 = length . filter hasAllRequired

part2 :: [[(String, String)]] -> Int
part2 = length . filter validPassport

validPassport :: [(String, String)] -> Bool
validPassport p = all validField p && hasAllRequired p

validField :: (String, String) -> Bool
validField (attb, value) = case lookup attb validators of
                                  Nothing -> False
                                  Just validator -> validator value

main :: IO ()
main = do
    input <- splitStr "\n\n" <$> readFile "./input.txt"
    let passports = generatePassports input
    putStr "Part 1: "
    print $ part1 passports
    putStr "Part 2: "
    print $ part2 passports
    return ()
