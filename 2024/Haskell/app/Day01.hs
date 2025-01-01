module Day01 where

import Data.List

parse :: String -> [(Int, Int)]
parse = map (toTuple . map read . words) . lines
  where
    toTuple [x, y] = (x, y)
    toTuple _      = error "Invalid input format"

score :: [Int] -> Int -> Int
score y x = x * (length $ filter (==x) y)

day01 :: IO ()
day01 = do
    p <- readFile "../R/inst/input01.txt"
    let valpairs = parse p
        (x, y) = unzip valpairs
        sortedX = sort x
        sortedY = sort y
    print $ sum $ map abs $ zipWith (-) sortedY sortedX -- part 1
    print $ sum $ map (score y) x                       -- part 2
    