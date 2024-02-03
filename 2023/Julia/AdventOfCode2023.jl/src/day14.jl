
function day14a(ex=false)
    x = rotl90(stack(split(rstrip(input(ex)), "\n"), dims=1))
    roll_n!(x)
    sum(last.(Tuple.(findall(z -> z == 'O', rot180(x)))))
end

function roll_n!(m)
    # stack(roll_vec.(eachrow(m)), dims=1)
    vcat(permutedims.(roll_vec.(eachrow(m)))...)
end

function roll_vec(xx)
    for j in eachindex(xx)[2:end]
        xx[j] in ".#" && continue

        if xx[j] == 'O'
            xx[j] = '.'
            kk = 0
            for k in j-1:-1:1
                kk = k
                xx[k] == '.' && continue
                kk = k + 1
                break
            end

            xx[kk] = 'O'
        end
    end

    return xx
end

function day14b(ex=false)
    iters = Vector{Int}()
    x = stack(split(rstrip(input(ex)), "\n"), dims=1)
    for i in 1:220
        x = rot180(roll_n!(rotr90(roll_n!(rotr90(roll_n!(rotr90(roll_n!(rotl90(x)))))))))
        push!(iters, sum(size(x, 2) + 1 .- first.(Tuple.(findall(z -> z == 'O', x)))))
    end
    cycles = detect_cycle(iters)
    ncycles = 1e9
    offset = ncycles - cycles[:start]
    cycles[:seq][round(Int, mod(offset, cycles[:len]) + 1)]
end

function detect_cycle(x)
    n = length(x)
    for len in 2:300
        if 4 * len - 1 > n
            break
        end

        for start in 1:500
            if start + 4 * len - 1 > n
                break
            end

            m = reshape(x[start:(start+4*len-1)], (len, 4))'
            u = 0

            for r in 1:len
                if length(Set(m[:, r])) != 1
                    break
                end
                u += 1
            end

            if u == len
                return Dict(:start => start, :len => len, :seq => x[start:(start+len-1)])
            end
        end
    end

    return nothing
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "14" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day14a(true))
day14a(false)
println(@btime day14a(false))

println("***")

println("Part 2:")
println(day14b(true))
day14b(false)
println(@btime day14b(false))


