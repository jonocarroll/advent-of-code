using Pipe

function day21a(ex=false)
end

function process_a(input)
end

function process_b(input)
end

function day21b(ex=false)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "21" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

# println("Part 1:")
# println(day21a(true))
# #println(day21a(false))
# println(@btime day21a(false))

# println("***")

# println("Part 2:")
# println(day21b(true))
# println(@btime day21b(false))
# #println(day21b(false))


