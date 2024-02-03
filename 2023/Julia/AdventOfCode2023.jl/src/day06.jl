function day06a(ex=false)
    times, dists = process_a(input(ex))
    prod(calc_race.(times, dists))
end

function process_a(x)
    xx = split(rstrip(x), "\n")
    time = [parse(Int, m.match) for m in eachmatch(r"\d+", xx[1])]
    dist = [parse(Int, m.match) for m in eachmatch(r"\d+", xx[2])]
    (time, dist)
end

function calc_race(t, d)
    sum(map(z -> z * (t - z) > d, Base.OneTo(t)))
end

function process_b(x)
    xx = replace(rstrip(x), " " => "")
    time, dist = [parse(Int, m.match) for m in eachmatch(r"\d+", xx)]
    (time, dist)
end

function day06b(ex=false)
    times, dists = process_b(input(ex))
    prod(calc_race.(times, dists))
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "06" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day06a(true))
day06a(false)
println(@btime day06a(false))

println("***")

println("Part 2:")
println(day06b(true))
day06b(false)
println(@btime day06b(false))


