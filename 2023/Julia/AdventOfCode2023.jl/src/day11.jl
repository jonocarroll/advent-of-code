# using Distances
# using DataFrames
# using GLM
using StatsBase
using Printf

function day11a(ex=false, factor=1)
    x = stack(split(rstrip(input(ex)), "\n"))
    newcols = 1 .+ factor * map(y -> '#' ∉ y, eachcol(x))
    colreps = vcat(hcat([repeat([elem], times) for (elem, times) in zip(1:size(x, 2), newcols)])...)
    x = x[:, colreps]
    newrows = 1 .+ factor * map(y -> '#' ∉ y, eachrow(x))
    rowreps = vcat(hcat([repeat([elem], times) for (elem, times) in zip(1:size(x, 1), newrows)])...)
    x = x[rowreps, :]
    galaxies = findall(z -> z == '#', x)
    # galaxies = Tuple.(findall(z -> z == '#', x))
    round(Int, sum(pairwise(cb, galaxies; symmetric=true)) / 2)
    # M = hcat(first.(galaxies), last.(galaxies))
    # round(Int, sum(pairwise(TotalVariation(), M')))
    # round(Int, sum(pairwise(Cityblock(), M')) / 2)
end

function cb(a, b)
    abs(a[1] - b[1]) + abs(a[2] - b[2])
end

function day11b(ex=false)
    d = [1, 5, 10]
    v = map(z -> day11a(ex, z), d)
    fitlm = [d ones(3)] \ v
    @sprintf "%.15g" round(Int, fitlm[2] + fitlm[1] * (1e6 - 1))
    # fitlm = lm(@formula(y ~ 1 + x), DataFrame(y=v, x=d))
    # @sprintf "%.15g" predict(fitlm, DataFrame(x=1e6 - 1))[1]
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "11" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day11a(true))
day11a(false)
println(@btime day11a(false))

println("***")

println("Part 2:")
println(day11b(true))
day11b(false)
println(@btime day11b(false))


