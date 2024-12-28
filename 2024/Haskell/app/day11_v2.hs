module Day11_2 where

-- inspired by https://work.njae.me.uk/2024/12/11/advent-of-code-2024-day-11/

import Data.IntMultiSet (IntMultiSet)
import qualified Data.IntMultiSet as MS

parse :: String -> [Int]
parse = map read . words

halves :: Int -> [Int]
halves n = [read l, read r]
    where (l, r) = splitAt (length s `div` 2) s
          s = show n

blinkOnce :: Int -> [Int]
blinkOnce 0 = [1]
blinkOnce n
  | even (length (show n)) = halves n
  | otherwise = [n * 2024]

blink :: IntMultiSet -> IntMultiSet
blink = MS.concatMap blinkOnce

count_em s n = print $ MS.size $ (!! n) $ iterate blink . MS.fromList $ parse s

day11_2 :: IO ()
day11_2 = do
    p <- readFile "../R/inst/input11.txt"
    count_em p 25
    count_em p 75
