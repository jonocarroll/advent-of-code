module Day19 where

import Data.List.Split (splitOn)
import Data.List (isPrefixOf)
import Data.MemoTrie
import Control.Parallel.Strategies (parMap, rpar)

type Towel = String
type Towels = [Towel]
type Pattern = String
type Patterns = [Pattern]

parse :: String -> (Towels, Patterns)
parse = parse' . splitOn [""] . lines
    where parse' ((towels:_):patterns:_) = (splitOn ", " towels, patterns)

count_with_towels :: Towels -> (Pattern -> Int)
count_with_towels towels = count_designs_mem
    where
        count_designs_mem = memo count_designs
        count_designs p
            | null p = 1
            | null prefixes = 0
            | otherwise = sum $ map next prefixes
            where 
                prefixes = filter (`isPrefixOf` p) towels
                next z = count_designs_mem (drop (length z) p)

day19 :: IO ()
day19 = do
    p <- readFile "../R/inst/input19.txt"
    let (towels, patterns) = parse p
    let solves = filter (>0) $ parMap rpar (count_with_towels towels) patterns
    print $ length solves
    print $ sum solves



