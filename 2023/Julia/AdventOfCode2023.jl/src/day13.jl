using Pipe

function day13a(ex=false)
    patterns = stack.(split.(split(rstrip(input(ex)), "\n\n"), "\n"), dims=1)
    sum([detect_mirror(p, 2) + 100 * (detect_mirror(p, 1)) for p in patterns])
end

function process_a(x)

end

function detect_mirror(x, dims)
    dimlen = size(x, dims)
    for i in 1:dimlen
        w = max(1, min(dimlen - i + 1, i - 1))
        if dims == 1
            x[i:i+w-1, :] == x[(i-1):-1:max(1, i - w), :] && return (i - 1)
        else
            x[:, i:i+w-1] == x[:, (i-1):-1:max(1, i - w)] && return (i - 1)
        end
    end
    return 0
end

function process_b(input)
end

function day13b(ex=false)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "13" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day13a(true))
day13a(false)
println(@btime day13a(false))

# println("***")

# println("Part 2:")
# println(day13b(true))
# println(@btime day13b(false))
# #println(day13b(false))


