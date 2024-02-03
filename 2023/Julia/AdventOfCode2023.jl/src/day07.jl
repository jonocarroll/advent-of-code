using Pipe
using StatsBase

function day07a(ex=false)
    x = @pipe input(ex) |>
              rstrip(_) |>
              split(_, "\n") |>
              map(z -> split(z, " "), _)
    cards = collect("23456789TJQKA")
    hands = first.(x)
    bids = parse.(Int, last.(x))
    types = handtype.(hands)
    order = @pipe indexin.(collect.(hands), Ref(cards)) |>
                  stack(_, dims=1) |>
                  hcat(types, _) |>
                  eachslice(_, dims=1) |>
                  collect(_) |>
                  sortperm(_)
    sum(bids[order] .* (1:length(bids)))
end

function handtype(hand)
    res = @pipe hand |>
                countmap(_) |>
                values(_) |>
                collect(_) |>
                sort!(_, rev=true) |>
                first(_, 2) |>
                join(_)

    parse(Int, rpad(res, 2, "0"))
end

function besthandtype(hand)
    if 'J' ∉ hand
        return (handtype(hand))
    end
    maxtype = 0
    for repl in split("AKQT98765432", "")
        subcard = replace(hand, 'J' => repl)
        maxtype = max(maxtype, handtype(subcard))
    end
    maxtype
end

function day07b(ex=false)
    x = @pipe input(ex) |>
              rstrip(_) |>
              split(_, "\n") |>
              map(z -> split(z, " "), _)
    cards = collect("J23456789TQKA")
    hands = first.(x)
    bids = parse.(Int, last.(x))
    types = besthandtype.(hands)
    order = @pipe indexin.(collect.(hands), Ref(cards)) |>
                  stack(_, dims=1) |>
                  hcat(types, _) |>
                  eachslice(_, dims=1) |>
                  collect(_) |>
                  sortperm(_)
    sum(bids[order] .* (1:length(bids)))
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "07" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day07a(true))
day07a(false)
println(@btime day07a(false))

println("***")

println("Part 2:")
println(day07b(true))
day07b(false)
println(@btime day07b(false))


