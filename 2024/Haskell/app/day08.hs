module Day08 where

import qualified Data.HashMap.Strict as M
import Data.List (intercalate, nub)

type Point2d = (Int, Int)
type Roof = M.HashMap Point2d Char

parse :: String -> Roof
parse input = M.fromList vs
  where vs = [ ((i, j), v)
               | (i, line) <- zip [0 ..] $ lines input,
                 (j, v) <- zip [0 ..] line ]

freqs :: M.HashMap Point2d Char -> [Char]
freqs = nub . M.elems . M.filter (/= '.') 

antinodes :: [Point2d] -> [Int] -> Roof -> Roof
antinodes points klist grid = foldl (\acc (x, y) -> addToGrid x y acc) grid pairs
    where
        pairs = [(x, y) | x <- points, y <- points, x /= y]
        addToGrid x y g = foldl (\acc k -> M.insert (newPos k x y) '#' acc) g klist
        newPos k (x1, y1) (x2, y2) = (x1 + k * (x2 - x1), y1 + k * (y2 - y1))

allAntinodes :: [Char] -> [Int] -> Roof -> Roof
allAntinodes nodes k grid = foldl (\acc x -> antinodes (getNodes x) k acc) grid nodes
    where getNodes x = nodeLocations x grid

prettyPrintGrid :: Roof -> String
prettyPrintGrid grid = intercalate "\n" rows
  where
    keys = M.keys grid
    maxX = maximum $ map fst keys
    maxY = maximum $ map snd keys
    rows = [[M.lookupDefault '.' (y, x) grid | x <- [0..maxX]] | y <- [0..maxY]]

nodeLocations :: Char -> Roof -> [Point2d]
nodeLocations c = M.keys . M.filter (== c)

n_inbound ::  Roof -> Roof -> Int
n_inbound grid ogrid = length $ 
                       filter (\(x, y) -> x >= 0 && y >= 0 && x <= maxX && y <= maxY) $ 
                       nodeLocations '#' grid
    where 
        keys = M.keys ogrid
        maxX = maximum $ map fst keys
        maxY = maximum $ map snd keys

day08 :: IO ()
day08 = do
    p <- readFile "../R/inst/input08.txt"
    -- p <- readFile "../tmp.txt"
    let grid = parse p
    let nodes = freqs grid
    let allnodes2 = allAntinodes nodes [2, -1] grid
    print $ n_inbound allnodes2 grid
    let allnodes60 = allAntinodes nodes [-60..60] grid
    print $ n_inbound allnodes60 grid
