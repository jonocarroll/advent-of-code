module Main where

import qualified System.TimeIt as T hiding (timeIt)
import Text.Printf

import Day01 as D01
import Day02 as D02
import Day03 as D03
import Day04 as D04
import Day05 as D05
import Day06 as D06
import Day07 as D07
import Day08 as D08
import Day11 as D11
import Day17 as D17
import Day18 as D18
import Day19 as D19
import Day23 as D23

-- use a more precise version of timeIt
timeIt :: IO a -> IO a
timeIt ioa = do
    (t, a) <- T.timeItT ioa
    printf "%s: %9.6fs\n" "CPU time" t
    return a

-- Run all days with 
--   /usr/bin/time -p cabal run AdventOfCode2024 -- +RTS -N8
-- some parMap rpar in day06.hs, day19.hs, 

main :: IO ()
main = do

    putStrLn "⭐️ Day 01 ⭐️ :" <> timeIt D01.day01
    putStrLn "⭐️ Day 02 ⭐️ :" <> timeIt D02.day02
    putStrLn "⭐️ Day 03 ⭐️ :" <> timeIt D03.day03
    putStrLn "⭐️ Day 04 ⭐️ :" <> timeIt D04.day04
    putStrLn "⭐️ Day 05 ⭐️ :" <> timeIt D05.day05
    putStrLn "⭐️ Day 06 ⭐️ :" <> timeIt D06.day06
    putStrLn "⭐️ Day 07 ⭐️ :" <> timeIt D07.day07
    putStrLn "⭐️ Day 08 ⭐️ :" <> timeIt D08.day08
    putStrLn "⭐️ Day 11 ⭐️ :" <> timeIt D11.day11
    putStrLn "⭐️ Day 17 ⭐️ :" <> timeIt D17.day17
    putStrLn "⭐️ Day 18 ⭐️ :" <> timeIt D18.day18
    putStrLn "⭐️ Day 19 ⭐️ :" <> timeIt D19.day19
    putStrLn "⭐️ Day 23 ⭐️ :" <> timeIt D23.day23
    putStrLn ""
    putStrLn (replicate 12 '⭐')
    putStrLn "⭐️ 🎄🎄🎄🎄🎄🎄🎄🎄🎄 ⭐️"
    putStrLn (replicate 12 '⭐')
    putStrLn ""