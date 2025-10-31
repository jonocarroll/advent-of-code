// clear the database
MATCH (n) DETACH DELETE n;

// read input as space-delimited with no header to LeftNode and RightNode 
// with value as attribute
CALL apoc.load.csv('https://raw.githubusercontent.com/jonocarroll/advent-of-code/refs/heads/main/2024/R/inst/input01.txt', {sep: ' ', header: FALSE}) YIELD list
WITH toInteger(list[0]) AS left, toInteger(list[3]) AS right
CREATE (l:LeftNode {value: left})
CREATE (r:RightNode {value: right});

// Create sorted connections
MATCH (l:LeftNode)
WITH l ORDER BY l.value
WITH collect(l) AS leftSorted

MATCH (r:RightNode)
WITH leftSorted, r ORDER BY r.value
WITH leftSorted, collect(r) AS rightSorted

// calculate difference between paired rows
UNWIND range(0, size(leftSorted)-1) AS idx
WITH leftSorted[idx] AS left, rightSorted[idx] AS right
CREATE (left)-[:PAIRED_WITH {difference: abs(left.value - right.value)}]->(right);

//
//

// Part 1
MATCH (l:LeftNode)-[p:PAIRED_WITH]->(r:RightNode)
RETURN sum(p.difference) as part1;

// Part 2
MATCH (l:LeftNode)
MATCH (r:RightNode)
WHERE l.value = r.value
WITH l.value AS value, count(r) AS occurrences
WITH value * occurrences AS similarity
RETURN sum(similarity) AS part2;