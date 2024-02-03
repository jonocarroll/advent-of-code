using Pipe

function day25a(ex=false)
end

function process_a(input)
end

function process_b(input)
end

function day25b(ex=false)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "25" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

# println("Part 1:")
# println(day25a(true))
# #println(day25a(false))
# println(@btime day25a(false))

# println("***")

# println("Part 2:")
# println(day25b(true))
# println(@btime day25b(false))
# #println(day25b(false))


