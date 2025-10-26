// clear the database
MATCH (n) DETACH DELETE n;

// read input data as :Tile nodes with row, col, value attributes
LOAD CSV FROM 'https://raw.githubusercontent.com/jonocarroll/advent-of-code/refs/heads/main/2024/R/inst/input10.txt' AS line
WITH line[0] AS rowString, linenumber() - 1 AS rowIndex
UNWIND range(0, size(rowString)-1) AS colIndex
WITH rowIndex, colIndex, toInteger(substring(rowString, colIndex, 1)) AS val
CREATE (:Tile {row: rowIndex, col: colIndex, val: val});

// get horizontal connections
MATCH (t:Tile)
WITH t.row AS row_num, t ORDER BY t.col
WITH row_num, collect(t) AS row
CALL apoc.nodes.link(row, 'horiz');

// get vertical connections
MATCH (t:Tile)
WITH t.col AS col_num, t ORDER BY t.row
WITH col_num, collect(t) AS col
CALL apoc.nodes.link(col, 'vert');

// link connected consecutive val nodes
MATCH (a)-[:horiz|vert]-(b)
WHERE a.val + 1 = b.val
MERGE (a)-[:POSSIBLE_MOVE]->(b);

///
///

// Part 1
MATCH SHORTEST 1 (trail_head:Tile {val:0})-[:POSSIBLE_MOVE]->*(top {val: 9})
WITH trail_head, count(DISTINCT top) AS score
WITH sum(score) AS part1
// Part 2
MATCH (trail_head:Tile {val:0})-[:POSSIBLE_MOVE]->*(top {val: 9})
WITH part1, trail_head, count(top) AS score
RETURN part1, sum(score) AS part2;
