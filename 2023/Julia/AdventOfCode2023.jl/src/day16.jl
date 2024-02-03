# using Base.Threads

function day16a(ex=false)
    grid = stack(split(rstrip(input(ex)), "\n"), dims=1)
    fire_beam(grid, 1, 1, 0, 1)
end

function fire_beam(grid, nx, ny, dx, dy)
    # queue = Vector{Tuple{CartesianIndex,Tuple{Int,Int}}}()
    queue = Vector{Tuple{Int,Int,Int,Int}}()
    sizehint!(queue, 1000)
    # visiteddir = Dict{Tuple{CartesianIndex,Tuple{Int,Int}},Int}()
    # visiteddir = Vector{Tuple{Int,Int,Int,Int,Int,Int}}()
    visiteddir = zeros(Bool, size(grid, 1), size(grid, 2), 3, 3)
    # sizehint!(visiteddir, 10000)
    # nextpos = np
    nextx = nx
    nexty = ny
    dirx = dx
    diry = dy
    # visiteddir = Vector{Tuple{CartesianIndex,Tuple{Int,Int}}}()
    straight = true
    while straight || !isempty(queue)
        if !straight
            qk = pop!(queue)
            nextx, nexty, dirx, diry = qk
            straight = true
        end
        if !checkbounds(Bool, grid, nextx, nexty)
            straight = false
            continue
        end
        if visiteddir[nextx, nexty, dirx+2, diry+2]
            straight = false
            continue
        end
        visiteddir[nextx, nexty, dirx+2, diry+2] = true
        mirror = grid[nextx, nexty]
        if mirror == '|'
            if diry != 0
                push!(queue, (nextx, nexty, 1, 0))
                dirx = -1
                diry = 0
            end
        elseif mirror == '-'
            if dirx != 0
                push!(queue, (nextx, nexty, 0, 1))
                dirx = 0
                diry = -1
            end
        elseif mirror == '\\'
            c = (complex(dirx, diry) * -1im)'
            dirx, diry = real(c), imag(c)
        elseif mirror == '/'
            c = (complex(dirx, diry) * 1im)'
            dirx, diry = real(c), imag(c)
        end

        nextx += dirx
        nexty += diry
    end

    energized = zeros(Bool, size(grid))
    for i ∈ 1:3
        for j ∈ 1:3
            energized .|= @view visiteddir[:, :, i, j]
        end
    end

    count(energized)
end

function day16b(ex=false)
    grid = stack(split(rstrip(input(ex)), "\n"), dims=1)
    nrow, ncol = size(grid)
    e1 = 0

    for beam in 1:ncol
        e1 = max(e1, fire_beam(grid, 1, beam, 1, 0))
    end

    for beam in 1:nrow
        e1 = max(e1, fire_beam(grid, beam, ncol, 0, -1))
    end

    for beam in 1:ncol
        e1 = max(e1, fire_beam(grid, nrow, beam, -1, 0))
    end

    for beam in 1:nrow
        e1 = max(e1, fire_beam(grid, beam, 1, 0, 1))
    end

    e1
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "16" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

# println("Using $(nthreads()) threads")

println("Part 1:")
println(day16a(true))
day16a(false)
println(@btime day16a(false))

println("***")

println("Part 2:")
println(day16b(true))
day16b(false)
println(@btime day16b(false))


