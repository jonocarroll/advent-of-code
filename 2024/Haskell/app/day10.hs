module Day10 where

import Numeric.LinearAlgebra (Matrix, (<>), fromLists, toLists, cmap, sumElements, tr)
import qualified Numeric.LinearAlgebra as M

-- c←,p←⍎¨↑⊃⎕NGET'../R/inst/input10.txt'1
-- adj←(1=∘.-⍨c)^1=+/¨|∘.-⍨,⍳⍴p
-- paths←(c=0)/(c=9)⌿(⊣++.×)⍣≡⍨adj
-- ⎕←+/+/paths⍪⍨↑,⊂×paths ⍝ Part 1 and 2

parse :: String -> Matrix Double
parse = fromLists . map (map (read . (:[]))) . lines

cc :: Matrix Double -> Matrix Double
cc p = fromLists $ outer (\x y -> if abs (x - y) == 1 then 1.0 else 0.0) c c
    where c = concat $ toLists p

pp :: Matrix Double -> Matrix Double
pp p = fromLists $ map (map (\z -> if z == 1 then 1.0 else 0.0)) $ outer (tupleAbsDiff) a a
    where a = [(x, y) | x <- [0..n-1], y <- [0..m-1]]
          n = M.cols p
          m = M.rows p

tupleAbsDiff :: (Int, Int) -> (Int, Int) -> Int
tupleAbsDiff (x1, y1) (x2, y2) = abs(x2 - x1) + abs(y2 - y1)

walkStep :: Matrix Double -> Matrix Double -> Matrix Double
walkStep p a = p + (a M.<> p)

findPaths :: Matrix Double -> Matrix Double
findPaths a = foldl (\acc _ -> walkStep a acc) a [1..9]

outer :: (a -> b -> c) -> [a] -> [b] -> [[c]]
outer f xs ys = [[f x y | y <- ys] | x <- xs]

adjm :: Matrix Double -> Matrix Double
adjm p = (cc p) `elementwiseAnd` (pp p)

elementwiseAnd :: Matrix Double -> Matrix Double -> Matrix Double
elementwiseAnd a b = fromLists $ map (map boolToDouble) $ zipWith (zipWith (&&)) (map (map (/= 0)) $ toLists a) (map (map (/= 0)) $ toLists b)

boolToDouble :: Bool -> Double
boolToDouble True = 1.0
boolToDouble False = 0.0

day10 :: IO ()
day10 = do
    -- p <- readFile "../tmp.txt"
    p <- readFile "../R/inst/input10.txt"
    let m = parse p
    let c = concat $ toLists m
    let paths = findPaths $ adjm m
    let c0 = [i | (i, x) <- zip [0..] c, x == 0]
    let c9 = [i | (i, x) <- zip [0..] c, x == 9]
    let submatrix = fromLists [[paths `M.atIndex` (c9 !! (i-1), c0 !! (j-1)) | j <- [1..length c0]] | i <- [1..length c9]]
    let nonZeroCounts = map (length . filter (/= 0)) (toLists $ M.tr submatrix)
    print $ sum nonZeroCounts
    print $ round $ sumElements submatrix
