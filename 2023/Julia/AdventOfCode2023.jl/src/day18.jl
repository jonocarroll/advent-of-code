using LinearAlgebra

function day18a(ex=false)
    x = split.(split(rstrip(input(ex)), "\n"), " ")
    lava = Vector{CartesianIndex}()
    sizehint!(lava, length(x))
    pos = CartesianIndex((0, 0))
    push!(lava, pos)
    perim = 0
    for xx in x
        dir, dist = xx[1], parse(Int, xx[2])
        pos += movedir(dir, dist)
        push!(lava, pos)
        perim += dist
    end
    internal = 0
    for v in eachindex(lava)[1:(end-1)]
        internal += det([lava[v][1] lava[v+1][1]; lava[v][2] lava[v+1][2]])
    end
    internal += det([lava[end][1] lava[1][1]; lava[end][2] lava[1][2]])
    round(Int, abs(internal / 2) + (perim / 2) + 1)
end

function movedir(c::AbstractString, d::Int)
    if occursin(c, "L2")
        CartesianIndex((0, -d))
    elseif occursin(c, "R0")
        CartesianIndex((0, d))
    elseif occursin(c, "U3")
        CartesianIndex((-d, 0))
    elseif occursin(c, "D1")
        CartesianIndex((d, 0))
    end
end

function extract_hex(hexstr)
    hexstr = replace(hexstr, r"[#)(]" => s"")
    diri = SubString(hexstr, length(hexstr))
    dist = parse(Int, SubString(hexstr, 1, 5), base=16)
    diri, dist
end

function day18b(ex=false)
    x = split.(split(rstrip(input(ex)), "\n"), " ")
    lava = Vector{CartesianIndex}()
    sizehint!(lava, length(x))
    pos = CartesianIndex((0, 0))
    push!(lava, pos)
    perim = 0
    for xx in last.(x)
        diri, dist = extract_hex(xx)
        pos += movedir(diri, dist)
        push!(lava, pos)
        perim += dist
    end
    internal = 0
    for v in eachindex(lava)[1:(end-1)]
        internal += det([lava[v][1] lava[v+1][1]; lava[v][2] lava[v+1][2]])
    end
    internal += det([lava[end][1] lava[1][1]; lava[end][2] lava[1][2]])
    round(Int64, abs(internal / 2) + (perim / 2) + 1)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "18" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day18a(true))
day18a(false)
println(@btime day18a(false))

println("***")

println("Part 2:")
println(day18b(true))
day18b(false)
println(@btime day18b(false))


