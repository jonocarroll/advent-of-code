using Pipe

function day24a(ex=false)
end

function process_a(input)
end

function process_b(input)
end

function day24b(ex=false)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "24" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

# println("Part 1:")
# println(day24a(true))
# #println(day24a(false))
# println(@btime day24a(false))

# println("***")

# println("Part 2:")
# println(day24b(true))
# println(@btime day24b(false))
# #println(day24b(false))


