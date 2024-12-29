module Day06 where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable, hashWithSalt)
import Data.Maybe
import Data.List (partition, nub)
import Control.Parallel.Strategies (parMap, rpar)

type Point2d = (Int, Int)
data Dir = North | East | South | West deriving (Show, Enum, Eq, Ord)

instance Hashable Dir where
  hashWithSalt salt = hashWithSalt salt . fromEnum

type Room = M.HashMap Point2d Char

moveOneStepInDir :: Point2d -> Dir -> Point2d
moveOneStepInDir (i, j) d = case d of 
    North -> (i - 1, j)
    East -> (i, j + 1)
    South -> (i + 1, j)
    West -> (i, j - 1)

turn90 :: Dir -> Dir
turn90 d = case d of
    North -> East
    East -> South
    South -> West
    West -> North

parse :: String -> (M.HashMap Point2d Char, Point2d)
parse input = (M.insert start '^' $ M.fromList vs, start)
  where
    start = fst (fromJust $ safeHead startList)
    (startList, vs) = 
      partition 
      ((== '^') . snd)
      [ ((i, j), v)
        | (i, line) <- zip [0 ..] $ lines input,
          (j, v) <- zip [0 ..] line ]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

walk :: (S.HashSet (Point2d, Dir)) -> Point2d -> Dir -> Room -> Maybe (S.HashSet (Point2d, Dir))
walk visited pos dir room 
    | S.member (pos, dir) visited = Nothing
    | otherwise = case M.lookup pos' room of 
        Just '#' -> walk visited' pos (turn90 dir) room 
        Just _ -> walk visited' pos' dir room 
        Nothing -> Just visited'
    where 
        pos' = moveOneStepInDir pos dir
        visited' = S.insert (pos, dir) visited

addObstacles :: [Point2d] -> Point2d -> Room -> Int
addObstacles path pos room = length $ filter isNothing placeObs 
    where 
        placeObs = parMap rpar (\z -> walk S.empty pos North (room' z)) path
        room' z = M.insert z '#' room

day06 :: IO () 
day06 = do
    p <- readFile "../R/inst/input06.txt"
    let (room, start) = parse p
    let path = nub $ map fst $ S.toList $ fromJust $ walk S.empty start North room
    print $ length path
    print $ addObstacles path start room
