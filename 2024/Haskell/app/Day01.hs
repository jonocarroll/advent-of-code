module Day01 where

import Data.List

parse :: String -> [[Int]]
parse = map (map read . words) . lines

score :: [Int] -> Int -> Int
score y x = x * (length $ filter (==x) y)

day01 :: IO ()
day01 = do
    p <- readFile "../R/inst/input01.txt"
    let valpairs = parse p
    let x = map (!! 0) valpairs
    let y = map (!! 1) valpairs
    print $ sum $ map abs $ zipWith (-) (sort y) (sort x) -- part 1
    print $ sum $ map (score y) x                         -- part 2
    