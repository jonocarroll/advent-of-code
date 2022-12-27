library(adventofcode22)
x <- readLines("./inst/input16.txt")

p1 <- f16a(x)
p2 <- f16b(x)

# not 3246 (too high)
# not 2376
# not 2402 (too low)

stopifnot(p1 == aoc_solutions$day16a)
stopifnot(p2 == aoc_solutions$day16b)
