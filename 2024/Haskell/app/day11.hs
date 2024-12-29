module Day11 where

import Data.MemoTrie

parse :: String -> [Int]
parse = map read . words

halves :: Int -> (Int, Int)
halves n = ((read l), (read r))
    where (l, r) = splitAt (length s `div` 2) s
          s = show n

blink :: Int -> Int -> Int
blink x n 
    | n == 0 = 1
    | x == 0 = blink_mem 1 (n-1)
    | even (length s) = blink_mem l (n-1) + blink_mem r (n-1)
    | otherwise = blink_mem (x * 2024) (n-1)
    where (l, r) = halves x
          s = show x

blink_mem :: Int -> Int -> Int
blink_mem = memo2 blink

count_em :: String -> Int -> IO ()
count_em s n = print $ sum $ map (\x -> blink_mem x n) $ parse s

day11 :: IO ()
day11 = do
    p <- readFile "../R/inst/input11.txt"
    count_em p 25
    count_em p 75
