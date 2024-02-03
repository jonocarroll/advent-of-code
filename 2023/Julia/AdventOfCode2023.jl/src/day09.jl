using Pipe

function day09a(ex=false)
    histories = split(rstrip(input(ex)), "\n")
    ans = 0
    for h in histories
        ans += pred(parse.(Int, split(h, " ")))
    end
    ans
end

function pred(h)
    d = 1
    vals = Vector{Vector{Int}}()
    push!(vals, h)
    while !(all(vals[d] .== 0))
        push!(vals, diff(vals[d]))
        d += 1
    end
    for i in reverse(eachindex(vals))[2:end]
        vals[i][end] = vals[i][end] + vals[i+1][end]
    end
    vals[1][end]
end

function day09b(ex=false)
    histories = split(rstrip(input(ex)), "\n")
    ans = 0
    for h in histories
        ans += pred(reverse!(parse.(Int, split(h, " "))))
    end
    ans
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "09" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day09a(true))
day09a(false)
println(@btime day09a(false))

println("***")

println("Part 2:")
println(day09b(true))
day09b(false)
println(@btime day09b(false))


