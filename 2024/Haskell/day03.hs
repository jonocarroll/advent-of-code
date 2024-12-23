import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Text.Regex

getMatches :: String -> Maybe (String, String, String, [String])
getMatches s = matchRegexAll r s
    where r = mkRegex "mul\\([0-9]{1,3},[0-9]{1,3}\\)"

getDoDontMatches :: String -> Maybe (String, String, String, [String])
getDoDontMatches s = matchRegexAll r s
    where r = mkRegex "mul\\([0-9]{1,3},[0-9]{1,3}\\)|don't\\(\\)|do\\(\\)"

recurseMatches :: String -> [Int]
recurseMatches x = case getMatches x of
    Nothing -> []
    Just (_, a, b, _) -> ([getDigits a] ++ recurseMatches b)

recurseMatches2 :: String -> [[Char]]
recurseMatches2 x = case getDoDontMatches x of
    Nothing -> []
    Just (_, a, b, _) -> ([a] ++ recurseMatches2 b)

getDigits :: String -> Int
getDigits = doMult . map (filter isDigit) . splitOn "," 

keepMuls :: Int -> [String] -> [[Char]] -> [String]
keepMuls _ s [] = s
keepMuls isActive s (x:xs) 
    | take 5 x == "don't" = keepMuls 0 s xs 
    | take 2 x == "do" = keepMuls 1 s xs 
    | isActive == 1 && take 3 x == "mul" = keepMuls 1 (s ++ [x]) xs 
    | isActive == 0 && take 3 x == "mul" = keepMuls 0 s xs 
    | otherwise = error "Something else"

doMult :: (Num a, Read a) => [String] -> a
doMult [a, b] = (read  a) * (read b)
doMult _ = 0

main :: IO ()
main = do
    -- p <- readFile "tmp.txt"
    p <- readFile "R/inst/input03.txt"
    print $ sum $ recurseMatches p
    print $ sum $ map getDigits $ keepMuls 1 [] (recurseMatches2 p)
