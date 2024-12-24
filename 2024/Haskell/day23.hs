import Data.List.Split (splitOn)
import qualified Data.Algorithm.MaximalCliques as MC
import Data.List
import Data.Function (on)
import qualified Data.Set as S

parse = map (splitOn "-") . lines

type Connections = [String]
type Edge = [String]
type Node = String
type Triple = (Node, Node, Node)

isEdge :: Connections -> Node -> Node -> Bool
isEdge edgelist n1 n2 = directEdgeExists || reverseEdgeExists
    where directEdgeExists = (asConnection [n1, n2]) `elem` edgelist
          reverseEdgeExists = (asConnection [n2, n1]) `elem` edgelist
          asConnection = intercalate "-"

getCliques :: Connections -> [Node] -> [Edge]
getCliques edgelist nodes = MC.getMaximalCliques (isEdge edgelist) nodes

triangles :: Node -> Connections -> [Node] -> [Triple]
triangles a edgelist nodes = [(a, b, c) | b <- nodes, 
                                          c <- nodes, 
                                          isEdge edgelist a b, 
                                          isEdge edgelist a c, 
                                          isEdge edgelist b c,
                                          b /= c, 
                                          b /= a, 
                                          c /= a]

sortSTriple :: S.Set Triple -> S.Set [Node]
sortSTriple = S.map (\(a, b, c) -> sort [a, b, c])

main :: IO ()
main = do
    p <- readFile "tmp3.txt"
    -- p <- readFile "R/inst/input23.txt"
    let edgelist = lines p
    let nodes = nub $ concat $ parse p
    let t_nodes = filter (\z -> take 1 z == "t") nodes
    let t_triangles = sortSTriple $ S.fromList $ concatMap (\z -> triangles z edgelist nodes) $ t_nodes
    let cliques = getCliques edgelist nodes

    print $ length t_triangles
    putStrLn $ intercalate "," $ sort $ maximumBy (compare `on` length) cliques
