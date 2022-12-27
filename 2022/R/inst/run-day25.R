library(adventofcode22)
x <- readLines("./inst/input25.txt")

p1 <- f25a(x)
p2 <- f25b(x)


# 20-1-11==0-=01120-11

stopifnot(p1 == aoc_solutions$day25a)
stopifnot(p2 == aoc_solutions$day25b)
