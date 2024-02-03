using Pipe

function day19a(ex=false)
end

function process_a(input)
end

function process_b(input)
end

function day19b(ex=false)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "19" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

# println("Part 1:")
# println(day19a(true))
# #println(day19a(false))
# println(@btime day19a(false))

# println("***")

# println("Part 2:")
# println(day19b(true))
# println(@btime day19b(false))
# #println(day19b(false))


