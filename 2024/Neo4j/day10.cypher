// based on https://github.com/halftermeyer/AoC_2024/blob/main/day10/code.ipynb

// read input data as :Tile nodes with row, col, value attributes
LOAD CSV FROM 'https://raw.githubusercontent.com/jonocarroll/advent-of-code/refs/heads/main/2024/R/inst/input10.txt' AS line
WITH line[0] AS rowString, linenumber() - 1 AS rowIndex
UNWIND range(0, size(rowString)-1) AS colIndex
WITH rowIndex, colIndex, toInteger(substring(rowString, colIndex, 1)) AS val
CREATE (:Tile {row: rowIndex, col: colIndex, val: val})

// get east-west connections
MATCH (t:Tile)
WITH t.row AS row_num, t ORDER BY t.col
WITH row_num, collect(t) AS row
CALL apoc.nodes.link(row, 'EW')

// get north-south connections
MATCH (t:Tile)
WITH t.col AS col_num, t ORDER BY t.row
WITH col_num, collect(t) AS col
CALL apoc.nodes.link(col, 'NS')

// link connected consecutive val nodes
MATCH (a)-[:NS|EW]-(b)
WHERE a.val + 1 = b.val
MERGE (a)-[:POSSIBLE_MOVE]->(b)

// calculate number of paths from val:0 to val:9
MATCH (trail_head:Tile {val:0})
CALL (trail_head) {
  MATCH SHORTEST 1 (trail_head)-[:POSSIBLE_MOVE]->*(top {val: 9})
  RETURN count(DISTINCT top) AS score
} IN CONCURRENT TRANSACTIONS OF 1 ROWS
RETURN sum(score) AS part1

// calculate number of trail heads
MATCH (trail_head:Tile {val:0})
CALL (trail_head) {
  MATCH (trail_head)-[:POSSIBLE_MOVE]->*(top {val: 9})
  RETURN count(top) AS score
} IN CONCURRENT TRANSACTIONS OF 1 ROWS
RETURN sum(score) AS part2
