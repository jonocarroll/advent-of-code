import Data.Char (isDigit)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Bits (Bits(xor))
import Data.Maybe
--import Debug.Trace


parse :: String -> ([Int], [Int])
parse = parse' . splitOn [""] . lines
    where parse' (registers:(program:_):_) = (map parseRegister registers, parseProgram program)

parseRegister :: String -> Int
parseRegister = read . last . words

parseProgram :: String -> [Int]
parseProgram =  map read . splitOn "," . last . words 

-- (a, b, c)
data Registers = Registers Int Int Int deriving Show

-- (ptr, registers, outs)
data State = State Int Registers [Maybe Int] deriving Show

combo :: Int -> Registers -> Int
combo 0 _ = 0
combo 1 _ = 1
combo 2 _ = 2
combo 3 _ = 3
combo 4 reg@(Registers a _ _) = a
combo 5 reg@(Registers _ b _) = b
combo 6 reg@(Registers _ _ c) = c
combo _ _ = error "Unreachable"

opCode :: Int -> State -> [Int] -> [Maybe Int]
opCode 0 s@(State ptr reg@(Registers a b c) outs) p = newOuts
    where newState = (State newPtr newReg newRes)
          newPtr = ptr + 2
          newReg = (Registers res b c)
          res = a `div` den
          lit = (p !! (ptr + 1))
          den = 2 ^ (combo lit reg)
          newRes = outs ++ [Nothing]
          newOuts = run newState p 

opCode 1 s@(State ptr reg@(Registers a b c) outs) p = newOuts
    where newState = (State newPtr newReg newRes)
          newPtr = ptr + 2
          newReg = (Registers a res c)
          res = xor b lit
          lit = (p !! (ptr + 1))
          newRes = outs ++ [Nothing]
          newOuts = run newState p 

opCode 2 s@(State ptr reg@(Registers a b c) outs) p = newOuts
    where newState = (State newPtr newReg newRes)
          newPtr = ptr + 2
          newReg = (Registers a res c)
          res = (combo lit reg) `mod` 8
          lit = (p !! (ptr + 1))
          newRes = outs ++ [Nothing]
          newOuts = run newState p 
          
opCode 3 s@(State ptr reg@(Registers 0 b c) outs) p = newOuts
    where newState = (State newPtr newReg newRes)
          newPtr = ptr + 2
          newReg = (Registers 0 b c)
          newRes = outs ++ [Nothing]
          newOuts = run newState p 

opCode 3 s@(State ptr reg@(Registers a b c) outs) p = newOuts
    where newState = (State newPtr newReg newRes)
          newPtr = lit
          lit = (p !! (ptr + 1))
          newReg = (Registers a b c)
          newRes = outs ++ [Nothing]
          newOuts = run newState p 

opCode 4 s@(State ptr reg@(Registers a b c) outs) p = newOuts
    where newState = (State newPtr newReg newRes)
          newPtr = ptr + 2
          newReg = (Registers a res c)
          res = xor b c
          newRes = outs ++ [Nothing]
          newOuts = run newState p 

opCode 5 s@(State ptr reg@(Registers a b c) outs) p = newOuts
    where newState = (State newPtr newReg newRes)
          newPtr = ptr + 2
          newReg = (Registers a b c)
          res = (combo lit reg) `mod` 8
          lit = (p !! (ptr + 1))
          newRes = outs ++ [Just res]
          newOuts = run newState p 

opCode 6 s@(State ptr reg@(Registers a b c) outs) p = newOuts
    where newState = (State newPtr newReg newRes)
          newPtr = ptr + 2
          newReg = (Registers a res c)
          res = a `div` den
          lit = (p !! (ptr + 1))
          den = 2 ^ (combo lit reg)
          newRes = outs ++ [Nothing]
          newOuts = run newState p 

opCode 7 s@(State ptr reg@(Registers a b c) outs) p = newOuts
    where newState = (State newPtr newReg newRes)
          newPtr = ptr + 2
          newReg = (Registers a b res)
          res = a `div` den
          lit = (p !! (ptr + 1))
          den = 2 ^ (combo lit reg)
          newRes = outs ++ [Nothing]
          newOuts = run newState p 


run :: State -> [Int] -> [Maybe Int]
run s@(State ptr reg outs) p 
    | ptr + 2 <= length p = opCode (p !! ptr) s p
    | otherwise = outs

part1 :: State -> [Int] -> [Int]
part1 s prog = mapMaybe id $ run s prog

tryVal :: [Int] -> Int -> Int -> Int -> Maybe Int
tryVal prog val i acc
    | ((part1 (State 0 (Registers (val + (acc * 8)) 0 0) []) prog) !! 0) == (prog !! i) = Just (val + (acc * 8))
    | otherwise = Nothing

quine :: [Int] -> Int -> Int -> Int
quine p i acc 
    | i == 0 = quine' p 0 acc
    | otherwise = quine p (i-1) $ quine' p i acc

quine' :: [Int] -> Int -> Int -> Int
quine' p i acc = ( !! 0 ) $ mapMaybe id $ map (\z -> tryVal p z i acc) [0..8]

main :: IO ()
main = do
    p <- readFile "R/inst/input17.txt"
    --p <- readFile "tmp.txt"
    let ([a, b, c], prog) = parse p
    let runp = part1 (State 0 (Registers a b c) []) prog
    putStrLn $ "Part 1: " <> (intercalate "," $ map show $ runp)
    putStrLn $ "Part 2: " <> (show $ quine prog 15 0)
