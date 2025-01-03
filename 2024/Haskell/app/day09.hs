module Day09 where

import Data.Char
import Data.List.Split (chunksOf)
import qualified Data.Set as S

-- mostly inspired by Jonathan Travaglia's solution

data Block = Block { index :: Int, full :: Int, empty :: Int } deriving Show

parse :: String -> [Block]
parse = zipWith makeBlock [0..] . chunksOf 2 . map digitToInt 

makeBlock :: Int -> [Int] -> Block
makeBlock ix [f] = Block ix f 0
makeBlock ix [f, e] = Block ix f e
makeBlock _ _ = error "Invalid block"

compact1 :: [Block] -> [Int]
compact1 blocks = take n_blocks $ moveBlocks blocks idsRev
    where n_blocks = sum $ map full blocks
          idsRev = reverse $ concatMap expandBlock blocks

moveBlocks :: [Block] -> [Int] -> [Int]
moveBlocks [] _ = []
moveBlocks (b@(Block _ _ e):bs) revIds = expandBlock b ++ toFill ++ moveBlocks bs revIds'
            where (toFill, revIds') = splitAt e revIds

expandBlock :: Block -> [Int]
expandBlock (Block ix f _) = replicate f ix
    
checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

compact2 :: S.Set Int -> [Block] -> [Int]
compact2 _ [] = []
compact2 movedBlocks ((Block ix f e):xs) = replicate f value ++ fillEmpty movedBlocks xs e (reverse xs)
        where value = if S.member ix movedBlocks then 0 else ix

fillEmpty :: S.Set Int -> [Block] -> Int -> [Block] -> [Int]
fillEmpty movedBlocks xs toFill [] = 
    let zeros = replicate toFill 0
        compacted = compact2 movedBlocks xs
    in zeros ++ compacted
fillEmpty movedBlocks xs toFill (b@(Block ix f _):xsRev)
    | toFill < f = fillEmpty movedBlocks xs toFill xsRev
    | S.member ix movedBlocks = fillEmpty movedBlocks xs toFill xsRev
    | otherwise = expandBlock b ++ fillEmpty movedBlocks' xs remainingSpace xsRev
        where movedBlocks' = S.insert ix movedBlocks
              remainingSpace = toFill - f

day09 :: IO ()
day09 = do
    p <- readFile "../R/inst/input09.txt"
    -- p <- readFile "../tmp.txt"
    let mem = parse $ (!! 0) $ lines p 
    print $ checksum $ compact1 mem
    print $ checksum $ compact2 S.empty mem
