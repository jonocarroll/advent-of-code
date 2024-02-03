using Pipe

function day02a(ex=false)
    poss = @pipe input(ex) |>
                 rstrip(_) |>
                 split(_, "\n") |>
                 map(process_a, _)

    sum(poss .* collect(range(1, length(poss))))
end

function process_a(input)
    lim = Dict("red" => 12, "green" => 13, "blue" => 14)

    @pipe input |>
          split(_, ":")[2] |>
          split(_, ";") |>
          map(x -> map(strip, split(x, ",")), _) |>
          reduce(vcat, _) |>
          map(x -> game_is_possible(x, lim), _) |>
          all(_)
end

function game_is_possible(game, limit)
    col = replace(game, r"\d+ (.*)" => s"\1")
    n = parse(Int, replace(game, r"(\d+).*" => s"\1"))
    if n > limit[col]
        return (false)
    end
    return (true)
end

function process_b(input)
    @pipe input |>
          split(_, ":")[2] |>
          split(_, ";") |>
          strip.(join(_, ",")) |>
          split(_, ",") |>
          min_required(_)
end

function min_required(game)
    cubes = replace.(game, r".*?: " => s"")
    seen = Dict("red" => 0, "green" => 0, "blue" => 0)
    cube = strip.(cubes)
    for cubeval in cube
        col = replace(cubeval, r"\d+ (.*)" => s"\1")
        n = parse(Int, replace(cubeval, r"(\d+).*" => s"\1"))
        seen[col] = max(seen[col], n)
    end
    seen
end

function day02b(ex=false)
    @pipe input(ex) |>
          rstrip(_) |>
          split(_, "\n") |>
          map(process_b, _) |>
          map(values, _) |>
          map(prod, _) |>
          sum(_)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "02" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day02a(true))
day02a(false)
println(@btime day02a(false))

println("***")

println("Part 2:")
println(day02b(true))
day02b(false)
println(@btime day02b(false))


