module Day01 where

type Turn = (Int, Int)
type State = (Int, Int, Int)

readTurns :: String -> Turn
readTurns (d:s) = (if d=='R' then 1 else -1, read s)
readTurns [] = (1,0)

doTurn :: State -> Turn -> State
doTurn (pos,pwd1,pwd2) (dir,n) = (pos',pwd1+pwd1',pwd2+pwd2') where
  pos' = (pos + dir*n) `mod` 100
  pwd1' = if pos' == 0 then 1 else 0
  pwd2' = (dir*pos `mod` 100 + n) `div` 100

day01 :: IO ()
day01 = do
    p <- readFile "../R/inst/input01.txt"
    let (_,p1,p2) = foldl' doTurn (50,0,0) . map readTurns $ lines p 
    print p1
    print p2
    