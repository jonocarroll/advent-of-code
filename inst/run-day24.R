library(adventofcode22)
x <- readLines("./inst/input24.txt")

p1 <- f24a(x)
p2 <- f24b(x)

# 844 (too low)
# 846 (too low)

stopifnot(p1 == aoc_solutions$day24a)
stopifnot(p2 == aoc_solutions$day24b)
