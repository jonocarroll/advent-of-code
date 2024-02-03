using Memoize

function day12a(ex=false)
    patterngroups = process_a(input(ex))
    sum(count_combinations(p, g) for (p, g) ∈ patterngroups)
end

function process_a(x)
    patterngroups = []
    for (pattern, instructions) ∈ split.(eachsplit(rstrip(x), "\n"), " ")
        numbers = Tuple(parse.(Int, split(instructions, ",")))
        push!(patterngroups, (pattern, numbers))
    end
    patterngroups
end

@memoize function count_combinations(pattern::AbstractString, groups::Tuple)
    count = 0
    if isempty(groups)
        '#' ∈ pattern && return 0
        return 1
    end
    length(pattern) < sum(groups) + length(groups) - 1 && return 0
    if '.' ∉ pattern[begin:groups[begin]]
        if length(pattern) == groups[begin] || pattern[groups[begin]+1] != '#'
            count += count_combinations(pattern[groups[begin]+2:end], groups[begin+1:end])
        end
    end
    if pattern[begin] != '#'
        count += count_combinations(pattern[begin+1:end], groups)
    end
    return count
end

function day12b(ex=false)
    patterngroups = process_a(input(ex))
    ans = 0
    for (pattern, groups) ∈ patterngroups
        pattern = pattern * "?" * pattern * "?" * pattern * "?" * pattern * "?" * pattern
        ans += count_combinations(pattern, Tuple(repeat(collect(groups), 5)))
    end
    ans
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "12" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day12a(true))
day12a(false)
println(@btime day12a(false))

println("***")

println("Part 2:")
println(day12b(true))
day12b(false)
println(@btime day12b(false))


