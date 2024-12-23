import qualified Data.Matrix as M
import qualified Data.Vector as V

parse = M.fromLists . lines

oneByFourSub i j = M.submatrix i i j (j + 3)

oneByFours m = map M.toList [oneByFourSub i j m | i <- [1..(M.nrows m)], j <- [1..(M.ncols m - 3)]]

reverseMat = M.fromLists . reverse . M.toLists

diagFourSub i j = M.submatrix i (i + 3) j (j + 3) 
antiDiagFourSub i j = reverseMat . diagFourSub i j

getDiags f m = map (V.toList . M.getDiag) [f i j m | i <- [1..(M.nrows m - 3)], j <- [1..(M.ncols m - 3)]]

diagFours = getDiags diagFourSub
antiDiagFours = getDiags antiDiagFourSub

threeByThreeSub i j = M.submatrix i (i + 2) j (j + 2) 

getX m = [threeByThreeSub i j m | i <- [1..(M.nrows m - 2)], j <- [1..(M.ncols m - 2)]]

getXelems m = [M.getElem i j m | (i, j) <- [(1, 1), (1, 3), (2, 2), (3, 1), (3, 3)]]

countXmas = length . filter (`elem` ["XMAS", "SAMX"])

countMAS = length . filter (`elem` ["MSAMS", "MMASS", "SMASM", "SSAMM"])

main :: IO ()
main = do
    -- p <- readFile "tmp.txt"
    p <- readFile "R/inst/input04.txt"
    let m = parse p
    let h = countXmas $ oneByFours $ m
    let v = countXmas $ oneByFours $ M.transpose $ m
    let d1 = countXmas $ diagFours $ m
    let d2 = countXmas $ antiDiagFours $ m
    print $ h + v + d1 + d2
    print $ countMAS $ map getXelems $ getX m
