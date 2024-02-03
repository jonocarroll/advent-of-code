using Pipe

function day04a(ex=false)
    @pipe input(ex) |>
          strip(_) |>
          split(_, "\n") |>
          map(process_card, _) |>
          map(z -> intersect(z[1], z[2]), _) |>
          map(score, _) |>
          sum(_)
end

function score(x)
    l = length(x)
    l == 0 ? 0 : 2^(l - 1)
end

function process_card(x)
    x = replace(x, r"Card.*:" => s"")
    xx = strip.(split(x, "|"))
    winning = [parse(Int, m.match) for m in eachmatch(r"\d+", xx[1])]
    mine = [parse(Int, m.match) for m in eachmatch(r"\d+", xx[2])]
    (winning, mine)
end

function day04b(ex=false)
    wins = @pipe input(ex) |>
                 strip(_) |>
                 split(_, "\n") |>
                 map(process_card, _) |>
                 map(z -> intersect(z[1], z[2]), _) |>
                 map(length, _)
    l = length(wins)
    cards = ones(Int, l)
    for i in eachindex(wins)
        if wins[i] == 0
            continue
        else
            for j in min(l, i + 1):min(l, i + wins[i])
                cards[j] += cards[i]
            end
        end
    end
    sum(cards)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "04" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day04a(true))
day04a(false)
println(@btime day04a(false))

println("***")

println("Part 2:")
println(day04b(true))
day04b(false)
println(@btime day04b(false))


