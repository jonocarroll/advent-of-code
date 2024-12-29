module Day23 where

import Data.List.Split (splitOn)
import qualified Data.Algorithm.MaximalCliques as MC
import Data.List
import Data.Function (on)
import qualified Data.Set as S
import Data.MemoTrie

parse :: String -> [[String]]
parse = map (splitOn "-") . lines

type Connections = S.Set [Char]
type Edge = [String]
type Node = String
type Triple = (Node, Node, Node)

isEdge :: Connections -> Node -> Node -> Bool
isEdge edgeSet n1 n2 = S.member (asConnection [n1, n2]) edgeSet || 
                       S.member (asConnection [n2, n1]) edgeSet
    where 
        asConnection = intercalate "-"

isEdge_mem :: Connections -> Node -> Node -> Bool
isEdge_mem edgeSet = memo2 (\n1 n2 -> isEdge edgeSet n1 n2)

getCliques :: Connections -> [Node] -> [Edge]
getCliques edgeSet = MC.getMaximalCliques (isEdge_mem edgeSet)

triangles :: [Node] -> Connections -> [Node] -> [Triple]
triangles [a, b] edgeSet nodes = [(a, b, c) | c <- nodes, 
                                              b /= a, 
                                              c /= b,
                                              c /= a,
                                              isEdge_mem edgeSet a c, 
                                              isEdge_mem edgeSet b c]
triangles _ _ _ = []

sortSTriple :: S.Set Triple -> S.Set [Node]
sortSTriple = S.map (\(a, b, c) -> sort [a, b, c])

day23 :: IO ()
day23 = do
    -- p <- readFile "../tmp3.txt"
    p <- readFile "../R/inst/input23.txt"
    let edgeSet = S.fromList $ lines p
    let nodes = nub $ concat $ parse p
    let t_pairs = filter (any ("t" `isPrefixOf`)) $ parse p
    let t_triangles = sortSTriple $ S.fromList $ concatMap (\z -> triangles z edgeSet nodes) $ t_pairs
    let cliques = getCliques edgeSet nodes

    print $ length t_triangles
    putStrLn $ intercalate "," $ sort $ maximumBy (compare `on` length) cliques
