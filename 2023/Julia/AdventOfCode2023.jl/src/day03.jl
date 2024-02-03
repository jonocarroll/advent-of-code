using Pipe

function day03a(ex=false)
    x = @pipe input(ex) |>
              split(_) |>
              map(x -> split(x, ""), _) |>
              stack(_, dims=1)
    allparts = parts(x)
    @pipe map(z -> check_border(x, z), allparts) |>
          filter(z -> z[2] != 0, _) |>
          first.(_) |>
          sum(_)
end

function parts(x)
    n = []
    for i in axes(x, 1)
        num = 0
        col = 1
        for j in axes(x, 2)
            v = x[i, j]
            if v in string.(collect(range(0, 9)))
                num = (10 * num) + parse(Int64, v)
                if (j == size(x, 2))
                    push!(n, (i, col - ndigits(num), num, ndigits(num)))
                    num = 0
                end
            elseif (num != 0)
                push!(n, (i, col - ndigits(num), num, ndigits(num)))
                num = 0
            end
            col += 1
        end
    end
    return (n)
end

function check_border(x, part)
    nonsymbols = push!(string.(collect(range(0, 9))), ".")
    xvals = filter(y -> checkbounds(Bool, x, y, :), part[1] .+ (-1:1))
    yvals = filter(y -> checkbounds(Bool, x, :, y), part[2] .+ (-1:part[4]))
    (part[3], length(findall(z -> z ∉ nonsymbols, x[xvals, yvals])))
end

function check_gear(x, part)
    gearsym = "*"
    xvals = filter(y -> checkbounds(Bool, x, y, :), part[1] .+ (-1:1))
    yvals = filter(y -> checkbounds(Bool, x, :, y), part[2] .+ (-1:part[4]))
    gear = findall(z -> z == gearsym, x[xvals, yvals])
    if (length(gear) > 0)
        gearx = xvals[gear[1][1]]
        geary = yvals[gear[1][2]]
        return ((gearx, geary, part[3], true))
    end
    (0, 0, part[3], false)
end

function process_b(input)
end

function day03b(ex=false)
    x = @pipe input(ex) |>
              split(_) |>
              map(x -> split(x, ""), _) |>
              stack(_, dims=1)
    allparts = parts(x)
    gearparts = @pipe map(z -> check_gear(x, z), allparts) |>
                      filter(z -> z[4] != 0, _) |>
                      map(z -> [string(z[1]) * "_" * string(z[2]), z[3]], _)
    geard = Dict()
    for i in gearparts
        if !haskey(geard, i[1])
            merge!(geard, Dict(i[1] => i[2]))
        else
            push!(geard, i[1] => [get(geard, i[1], ""), i[2]])
        end
    end
    @pipe geard |>
          values(_) |>
          collect(_) |>
          filter(z -> length(z) > 1, _) |>
          map(prod, _) |>
          sum(_)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "03" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day03a(true))
println(day03a(false))
println(@btime day03a(false))

println("***")

println("Part 2:")
println(day03b(true))
day03b(false)
println(@btime day03b(false))


