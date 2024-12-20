using Memoize

input = "../R/inst/input19.txt"
p = split(read(input, String), r"\R\R")
towels = split(p[1], ", ")
patterns = split(p[2], r"\n")

substr(s, x) = last(s, length(s) - length(x))

@memoize function count_designs(p) 
    if length(p) == 0 return 1 end
    opts = filter(x -> startswith(p, x), towels)
    if length(opts) == 0 return 0 end
    return sum(map(x -> count_designs(substr(p, x)), opts))
end

solves = map(count_designs, patterns)
println(sum(solves .> 0)) # Part 1
println(sum(solves))      # Part 2
