library(adventofcode22)
x <- readLines("./inst/input09.txt")

p1 <- f09a(x)
p2 <- f09b(x)

stopifnot(p1 == aoc_solutions$day09a)
stopifnot(p2 == aoc_solutions$day09b)

ggsave(filename = "~/Projects/adventofcode22/inst/vis-day09.png", width = 8, height = 8, bg = "white")
