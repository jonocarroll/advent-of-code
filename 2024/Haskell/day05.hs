import Data.List.Split (splitOn)
import Data.List (minimum, maximum)
import Data.Graph as Graph

-- https://5outh.blogspot.com/2012/12/graphs-and-topological-sorting-in.html
-- seemed like a good option but a library is also good

parse :: String -> ([(Int, Int)], [[Int]])
parse = parse' . splitOn [""] . lines
    where parse' (rules:orders:_) = (map parseRule rules, map parseOrder orders)

parseRule = parseRule' . splitOn "|"
parseRule' (a:b:_) = (read a, read b)
parseOrder = map read . splitOn ","

tupleListMin :: [(Int, Int)] -> Int
tupleListMin lst = Data.List.minimum $ map (uncurry min) lst
-- or foldl1 min $ concatMap (\(a, b) -> [a, b]) lst

tupleListMax :: [(Int, Int)] -> Int
tupleListMax lst = Data.List.maximum $ map (uncurry max) lst
-- or foldl1 max $ concatMap (\(a, b) -> [a, b]) lst

makeGraph :: [(Int, Int)] -> Graph.Graph
makeGraph e = Graph.buildG (tupleListMin e, tupleListMax e) e

mid :: [Int] -> Int
mid x = x !! n 
    where n = (length x) `div` 2

topoSortedSubGraph :: [(Int, Int)] -> [Int] -> [Int]
topoSortedSubGraph r o = filter (`elem` o) g
    where g = topSort $ makeGraph relevant_rules
          relevant_rules = filter (from_nodes) r
          from_nodes x = (fst x `elem` o) 

isSorted :: [(Int, Int)] -> [Int] -> Bool
isSorted r o =  o == topoSortedSubGraph r o

partOne :: [(Int, Int)] -> [[Int]] -> Int
partOne r o = sum $ map (\z -> correct_order r z) o
        
correct_order :: [(Int, Int)] -> [Int] -> Int
correct_order rules order
    | isSorted rules order = mid order
    | otherwise = 0

partTwo :: [(Int, Int)] -> [[Int]] -> Int
partTwo r o = sum $ map (\z -> re_sort r z) o

re_sort :: [(Int, Int)] -> [Int] -> Int
re_sort rules order
    | isSorted rules order = 0
    | otherwise = mid $ topoSortedSubGraph rules order

main :: IO ()
main = do
    --p <- readFile "tmp.txt"
    p <- readFile "R/inst/input05.txt"
    let (rules, orders) = parse p
    print $ partOne rules orders 
    print $ partTwo rules orders 
