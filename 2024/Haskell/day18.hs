import Algorithm.Search
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Maybe
import Numeric.Search.Integer

type Byte = (Int, Int)
type Bytes = [Byte]

parse :: String -> Bytes
parse = map (toTuple . splitOn ",") . lines
    where
        toTuple [x, y] = (read x, read y)

neighbors :: Int -> Byte -> [Byte]
neighbors m (x, y) = neighbors' 
    where
        neighbors' = filter inBounds offsets
        offsets = [(x, y + 1), (x - 1, y), (x + 1, y), (x, y - 1)]
        inBounds (x, y) = x >= 0 && x <= m && y >= 0 && y <= m

dist :: Byte -> Byte -> Int
dist (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)

isWall :: S.Set Byte -> Byte -> Bool
isWall wall = isWall' 
    where isWall' = (`S.member` wall)

findPath :: Int -> Bytes -> Int -> Maybe (Int, Bytes)
findPath size bytes n = aStar g dist (dist end) (== end) (0, 0)
    where g = neighbors size `pruning` (isWall corrupted)
          corrupted = S.fromList $ take n bytes
          end = (size, size)

find_blocked_path :: Int -> Bytes -> (Integer -> Bool)
find_blocked_path gsize bytes = \n -> (isNothing $ findPath gsize bytes (fromInteger n))

print_as_pair :: Byte -> String
print_as_pair (a, b) = show a ++ "," ++ show b

main :: IO ()
main = do
    --p <- readFile "tmp.txt"
    p <- readFile "R/inst/input18.txt"
    let bytes = parse p
    let maxBytes = 1024
    let gridSize = 70
    print $ fst $ fromJust $ findPath gridSize bytes maxBytes -- part 1
    let searchf = find_blocked_path gridSize bytes 
    let first_blocked = fromInteger $ searchFrom searchf (toInteger maxBytes)
    putStrLn $ print_as_pair $ (bytes !! (first_blocked - 1)) -- part 2
 