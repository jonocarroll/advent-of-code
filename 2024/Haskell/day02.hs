import Data.List

slow xs = all ((>= 1) . abs) xs && all ((<= 3). abs) xs

monotonic xs 
    | all (>0) xs = True
    | all (<0) xs = True
    | otherwise = False

isSafe xs = monotonic diffs && slow diffs 
  where diffs = zipWith (-) xs (drop 1 xs)

damped line = zipWith (++) (inits line) (drop 1 $ tails line)

main = do
    p <- readFile "../R/inst/input02.txt"
    let reports = map (map readInt) $ map words $ lines p
    print $ length $ filter isSafe reports -- part 1
    print $ length $ filter ((any isSafe) . damped) reports -- part 2

readInt :: String -> Int
readInt = read 
