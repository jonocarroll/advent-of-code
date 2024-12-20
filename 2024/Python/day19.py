from functools import cache

@cache
def count_designs(p):
    if len(p) == 0:
        return 1
    opts = [x for x in towels if p[0:len(x)] == x]
    if len(opts) == 0:
        return 0
    return sum([count_designs(p[len(x):]) for x in opts])

with open("../R/inst/input19.txt") as f:
    input = f.read().split("\n")

towels = input[0].split(", ")
patterns = [x for x in input[2:] if x != ""]

solves = [count_designs(p) for p in patterns]
print(len([x for x in solves if x > 0])) # Part 1
print(sum(solves))                       # Part 2
