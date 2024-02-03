using Pipe

function day17a(ex=false)
    M = stack(split(rstrip(input(ex)), "\n"), dims=1) .- '0'
    solve(M, 1:3)
end

function solve(M, R)
    n = size(M, 1)
    H, V = fill(sum(M), n, n), fill(sum(M), n, n)
    H[1] = V[1] = 0
    while true
        Σ = sum(H) + sum(V)
        for δ ∈ R, t ∈ 1:n
            t >= δ + 1 && (V[[t], :] .= min.(V[[t], :], H[[t - δ], :] + sum(M[t-δ+1:t, :], dims=1)))
            t + δ <= n && (V[[t], :] .= min.(V[[t], :], H[[t + δ], :] + sum(M[t:t+δ-1, :], dims=1)))
            t >= δ + 1 && (H[:, [t]] .= min.(H[:, [t]], V[:, [t - δ]] + sum(M[:, t-δ+1:t], dims=2)))
            t + δ <= n && (H[:, [t]] .= min.(H[:, [t]], V[:, [t + δ]] + sum(M[:, t:t+δ-1], dims=2)))
        end
        Σ == sum(H) + sum(V) && break
    end
    min(last(H), last(V))
end

function day17b(ex=false)
    M = stack(split(rstrip(input(ex)), "\n"), dims=1) .- '0'
    solve(M, 4:10)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "17" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day17a(true))
day17a(false)
println(@btime day17a(false))

println("***")

println("Part 2:")
println(day17b(true))
day17b(false)
println(@btime day17b(false))


