import Data.MemoTrie

parse :: String -> [Int]
parse = map read . words

halves :: Int -> (Int, Int)
halves n = ((read l), (read r))
    where (l, r) = splitAt (length s `div` 2) s
          s = show n

blink :: Int -> Int -> Int
blink x n = blink' x n
    where blink' _ 0 = 1
          blink' 0 n = blink_mem 1 (n-1)
          blink' z n 
            | even (length (show z)) = (blink_mem (fst $ halves z) (n-1)) + 
                                       (blink_mem (snd $ halves z) (n-1))
            | otherwise = blink_mem (z * 2024) (n-1)

blink_mem = memo2 blink

count_em s n = print $ sum $ map (\x -> blink_mem x n) $ parse s

main :: IO ()
main = do
    p <- readFile "R/inst/input11.txt"
    count_em p 25
    count_em p 75
