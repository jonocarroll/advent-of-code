module Day17 where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Bits (Bits(xor))
import Data.Maybe

parse :: String -> ([Int], [Int])
parse = parse' . splitOn [""] . lines
    where parse' (registers:(program:_):_) = (map parseRegister registers, parseProgram program)
          parse' _ = error "Unreachable"

parseRegister :: String -> Int
parseRegister = read . last . words

parseProgram :: String -> [Int]
parseProgram =  map read . splitOn "," . last . words 

type Program = [Int]

-- (a, b, c)
data Registers = Registers Int Int Int deriving Show

-- (ptr, registers, outs)
data State = State Int Registers [Maybe Int] deriving Show

combo :: Int -> Registers -> Int
combo 0 _ = 0
combo 1 _ = 1
combo 2 _ = 2
combo 3 _ = 3
combo 4 (Registers a _ _) = a
combo 5 (Registers _ b _) = b
combo 6 (Registers _ _ c) = c
combo _ _ = error "Unreachable"

opCode :: Int -> State -> Program -> [Maybe Int]
opCode 0 (State ptr reg@(Registers a b c) outs) p =
    let val = combo (p !! (ptr + 1)) reg
        denominator = 2^val
        result = a `div` denominator
    in run (State (ptr + 2) (Registers result b c) (outs ++ [Nothing])) p

opCode 1 (State ptr (Registers a b c) outs) p = 
    let result = xor b (p !! (ptr + 1))
    in run (State (ptr + 2) (Registers a result c) (outs ++ [Nothing])) p
          
opCode 2 (State ptr reg@(Registers a _ c) outs) p = 
    let result = (combo (p !! (ptr + 1)) reg) `mod` 8
    in run (State (ptr + 2) (Registers a result c) (outs ++ [Nothing])) p

opCode 3 (State ptr (Registers 0 b c) outs) p = 
    run (State (ptr + 2) (Registers 0 b c) (outs ++ [Nothing])) p

opCode 3 (State ptr (Registers a b c) outs) p = 
    run (State (p !! (ptr + 1)) (Registers a b c) (outs ++ [Nothing])) p

opCode 4 (State ptr (Registers a b c) outs) p =     
    run (State (ptr + 2) (Registers a (xor b c) c) (outs ++ [Nothing])) p

opCode 5 (State ptr reg@(Registers a b c) outs) p = 
    let result = (combo (p !! (ptr + 1)) reg) `mod` 8
    in run (State (ptr + 2) (Registers a b c) (outs ++ [Just result])) p

opCode 6 (State ptr reg@(Registers a _ c) outs) p = 
    let val = combo (p !! (ptr + 1)) reg
        denominator = 2^val
        result = a `div` denominator
    in run (State (ptr + 2) (Registers a result c) (outs ++ [Nothing])) p

opCode 7 (State ptr reg@(Registers a b _) outs) p = 
    let val = combo (p !! (ptr + 1)) reg
        denominator = 2^val
        result = a `div` denominator
    in run (State (ptr + 2) (Registers a b result) (outs ++ [Nothing])) p

opCode _ _ _ = error "Unreachable"

run :: State -> Program -> [Maybe Int]
run s@(State ptr _ outs) p 
    | ptr + 2 <= length p = opCode (p !! ptr) s p
    | otherwise = outs

part1 :: State -> Program -> [Int]
part1 s = catMaybes . run s

tryVal :: Program -> Int -> Int -> Int -> Maybe Int  
tryVal prog val i acc
    | isValid = Just newVal
    | otherwise = Nothing
    where
        newVal = val + (acc * 8)
        isValid = (part1 (aState newVal) prog !! 0) == (prog !! i)
        aState z = (State 0 (Registers z 0 0) [])

quine :: Program -> Int -> Int -> Maybe Int
quine p i acc 
    | i == 0 = quine' p 0 acc
    | otherwise = quine p (i-1) =<< quine' p i acc

quine' :: Program -> Int -> Int -> Maybe Int
quine' p i acc = safeHead $ mapMaybe (\z -> tryVal p z i acc) [0..8]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

day17 :: IO ()
day17 = do
    p <- readFile "../R/inst/input17.txt"
    --p <- readFile "tmp.txt"
    -- let ([a, b, c], prog) = parse p
    let (regs, prog) = parse p
        in case regs of
        [a, b, c] -> do
            let runp = part1 (State 0 (Registers a b c) []) prog
            putStrLn $ intercalate "," $ map show $ runp
            putStrLn $ show $ fromJust $ quine prog 15 0
        [] -> error "List is empty"
        [_] -> error "List has only one element"
        [_, _] -> error "List has only two elements"
        (_:_:_:_:_) -> error "List has more than three elements"

