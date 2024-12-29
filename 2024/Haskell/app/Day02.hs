module Day02 where

import Data.List

slow :: [Int] -> Bool
slow xs = all ((>= 1) . abs) xs && 
          all ((<= 3). abs) xs

monotonic :: [Int] -> Bool
monotonic xs 
    | all (>0) xs = True
    | all (<0) xs = True
    | otherwise = False

isSafe :: [Int] -> Bool
isSafe xs = monotonic diffs && slow diffs 
  where diffs = zipWith (-) xs (drop 1 xs)

damped :: [Int] -> [[Int]]
damped line = zipWith (++) (inits line) (drop 1 $ tails line)

day02 :: IO ()
day02 = do
    p <- readFile "../R/inst/input02.txt"
    let reports = map (map read) $ map words $ lines p
    print $ length $ filter isSafe reports                  -- part 1
    print $ length $ filter ((any isSafe) . damped) reports -- part 2
