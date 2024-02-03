using Pipe

function day20a(ex=false)
end

function process_a(input)
end

function process_b(input)
end

function day20b(ex=false)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "20" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

# println("Part 1:")
# println(day20a(true))
# #println(day20a(false))
# println(@btime day20a(false))

# println("***")

# println("Part 2:")
# println(day20b(true))
# println(@btime day20b(false))
# #println(day20b(false))


