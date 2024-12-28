module Day07 where

import Control.Arrow ((&&&))

parse :: String -> [(Integer, [Integer])]
parse = map (parseLine . words) . lines

parseLine :: (Read a, Read b) => [[Char]] -> (a, [b])
parseLine (x:xs) = (read (init x), map read xs)

apply_funs :: [Integer] -> Integer -> [Integer]
apply_funs x y = concatMap (\val -> [val + y, val * y]) x

concat' :: (Num a, Show a) => a -> a -> a
concat' a b = a * (10 ^ (length (show b))) + b

apply_funs_2 :: [Integer] -> Integer -> [Integer]
apply_funs_2 x y = concatMap (\val -> [val + y, val * y, concat' val y]) x

test_ops :: Integer -> [Integer] -> [Integer]
test_ops z (x:xs) = foldl (\acc x -> filter (<=z) $ apply_funs acc x) [x] xs

test_ops_2 :: Integer -> [Integer] -> [Integer]
test_ops_2 z (x:xs) = foldl (\acc x -> filter (<=z) $ apply_funs_2 acc x) [x] xs

day07 :: IO ()
day07 = do
    p <- readFile "../R/inst/input07.txt"
    --p <- readFile "../tmp.txt"
    let parsed = parse p
    let (res, vals) = (map fst &&& map snd) parsed
    print $ sum $ zipWith (\x y -> if any (==x) (test_ops x y) then x else 0) res vals
    print $ sum $ zipWith (\x y -> if any (==x) (test_ops_2 x y) then x else 0) res vals

