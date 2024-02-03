using Pipe

function day23a(ex=false)
end

function process_a(input)
end

function process_b(input)
end

function day23b(ex=false)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "23" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

# println("Part 1:")
# println(day23a(true))
# #println(day23a(false))
# println(@btime day23a(false))

# println("***")

# println("Part 2:")
# println(day23b(true))
# println(@btime day23b(false))
# #println(day23b(false))


