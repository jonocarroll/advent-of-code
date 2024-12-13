import Data.List

parse :: String -> [[Int]]
parse = map (map readInt . words) . lines

readInt :: String -> Int
readInt = read

score :: [Int] -> Int -> Int
score y x = x * (length $ filter (==x) y)

main :: IO ()
main = do
    p <- readFile "R/inst/input01.txt"
    let valpairs = parse p
    let x = map (!! 0) valpairs
    let y = map (!! 1) valpairs
    print $ sum $ map abs $ zipWith (-) (sort y) (sort x) -- part 1
    print $ sum $ map (score y) x                         -- part 2
    