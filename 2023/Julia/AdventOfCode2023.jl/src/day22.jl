using Pipe

function day22a(ex=false)
end

function process_a(input)
end

function process_b(input)
end

function day22b(ex=false)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "22" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

# println("Part 1:")
# println(day22a(true))
# #println(day22a(false))
# println(@btime day22a(false))

# println("***")

# println("Part 2:")
# println(day22b(true))
# println(@btime day22b(false))
# #println(day22b(false))


