using Pipe
using Printf

function day05a(ex=false)
    seeds, maps = process_a(input(ex))
    minimum(iterate_maps.(seeds, Ref(maps)))
end

function process_a(x)
    xx = split(x, "\n\n")
    _, seeds = split(xx[1], ": ")
    seeds = parse.(Int, split(seeds, " "))
    mapd = [Vector{Vector{Int64}}(undef, 3) for _ = 1:(length(xx)-1)]
    mi = 1
    for mapdat in xx[2:end]
        _, dat = split(mapdat, ":\n")
        mapd[mi] = [parse.(Int, z) for z in split.(rstrip.(split(dat, "\n"; keepempty=false)), " ")]
        mi += 1
    end
    (seeds, mapd)
end

function process_b(input)
end

function day05b(ex=false)
    seeds, maps = process_a(input(ex))
    ss = seeds[1:2:end]
    sl = seeds[2:2:end]
    dists = 2 .* floor.(Int, sqrt.(sl))
    sampseeds = Vector{StepRange}(undef, length(ss))
    for i in eachindex(ss)
        sampseeds[i] = ss[i]:dists[i]:(ss[i]+sl[i])
    end
    ans = Inf
    mini = Inf
    minj = Inf
    for ii in eachindex(sampseeds)
        loc = iterate_maps.(sampseeds[ii], Ref(maps))
        minl = findmin(loc)
        if minl[1] < ans
            ans = minl[1]
            mini = ii
            minj = minl[2]
        end
    end
    foundinrange = sampseeds[mini]
    loc = iterate_maps.(foundinrange[minj-1]:foundinrange[minj], Ref(maps))
    @sprintf "%.15g" minimum(loc)
end

function iterate_maps(loc, maps)
    for m in eachindex(maps)
        mm = maps[m]
        for r in mm
            drs, srs, len = r
            if srs <= loc <= (srs + len - 1)
                loc = drs + (loc - srs)
                break
            end
        end
    end
    loc
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "05" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day05a(true))
day05a(false)
println(@btime day05a(false))

println("***")

println("Part 2:")
println(day05b(true))
day05b(false)
println(@btime day05b(false))
