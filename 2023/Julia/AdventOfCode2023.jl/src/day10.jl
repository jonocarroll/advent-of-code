using LinearAlgebra

function day10a(ex=false)
    m = stack(split.(split(rstrip(input(ex)), "\n"), ""), dims=1)
    s = findall(z -> z == "S", m)[1]
    d = inputs(m, s)[1]
    _, ans = search(m, d, s)
    ans
end

function search(m, d, s)
    camefrom = Vector{CartesianIndex{2}}()
    visited = Vector{CartesianIndex{2}}()
    j = 0
    push!(visited, s)
    pushfirst!(camefrom, s)
    push!(visited, d)
    pushfirst!(camefrom, d)
    while true
        j += 1
        d = exits(m, d)
        if s ∉ camefrom && (d[1] == s || d[2] == s)
            return (visited, Int((j + 1) / 2))
        end
        if d[1] ∈ camefrom
            if d[2] ∈ camefrom
                error("Both ways are blocked!")
            else
                d = d[2]
            end
        else
            d = d[1]
        end
        push!(visited, d)
        pushfirst!(camefrom, d)
        pop!(camefrom)
    end
end

function exits(maze, pos::CartesianIndex)
    if maze[pos] == "F"
        [pos + CartesianIndex(1, 0), pos + CartesianIndex(0, 1)]
    elseif maze[pos] == "-"
        [pos + CartesianIndex(0, -1), pos + CartesianIndex(0, 1)]
    elseif maze[pos] == "|"
        [pos + CartesianIndex(-1, 0), pos + CartesianIndex(1, 0)]
    elseif maze[pos] == "J"
        [pos + CartesianIndex(0, -1), pos + CartesianIndex(-1, 0)]
    elseif maze[pos] == "L"
        [pos + CartesianIndex(-1, 0), pos + CartesianIndex(0, 1)]
    elseif maze[pos] == "7"
        [pos + CartesianIndex(0, -1), pos + CartesianIndex(1, 0)]
    end
end

function inputs(m, pos::CartesianIndex)
    ins = Vector{CartesianIndex}()
    if occursin(m[pos+CartesianIndex(-1, 0)], "|7F")
        push!(ins, pos + CartesianIndex(-1, 0))
    end
    if occursin(m[pos+CartesianIndex(1, 0)], "|JL")
        push!(ins, pos + CartesianIndex(1, 0))
    end
    if occursin(m[pos+CartesianIndex(0, -1)], "L-F")
        push!(ins, pos + CartesianIndex(0, -1))
    end
    if occursin(m[pos+CartesianIndex(0, 1)], "7J-")
        push!(ins, pos + CartesianIndex(0, 1))
    end
    ins
end

function day10b(ex=false)
    m = stack(split.(split(rstrip(input(ex, "b")), "\n"), ""), dims=1)
    s = findall(z -> z == "S", m)[1]
    visited, furthest = search(m, inputs(m, s)[1], s)
    internal = 0
    for v in 1:(2*furthest-1)
        internal += det([visited[v][1] visited[v+1][1]; visited[v][2] visited[v+1][2]])
    end
    internal += det([visited[end][1] visited[1][1]; visited[end][2] visited[1][2]])
    round(Int, abs(internal / 2) - furthest + 1)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "10" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day10a(true))
day10a(false)
println(@btime day10a(false))

println("***")

println("Part 2:")
println(day10b(true))
day10b(false)
println(@btime day10b(false))


