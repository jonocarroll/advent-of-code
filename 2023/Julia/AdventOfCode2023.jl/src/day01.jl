using Pipe

function day01a(ex=false)
    @pipe input(ex) |>
          split(_, "\n") |>
          filter(x -> x != "") |> # or rstrip
          collect(_) |>
          map(process_a, _) |>
          sum
end

function process_a(input)
    @pipe input[findall(map(isdigit, collect(input)))][[1, end]] |>
          parse(Int, _)
end

function process_b(input)
    @pipe replace(input,
              "oneight" => "18",
              "twone" => "21",
              "threeight" => "38",
              "fiveight" => "58",
              "eightwo" => "82",
              "eighthree" => "83",
              "nineight" => "98",
          ) |>
          replace(_,
              "one" => "1",
              "two" => "2",
              "three" => "3",
              "four" => "4",
              "five" => "5",
              "six" => "6",
              "seven" => "7",
              "eight" => "8",
              "nine" => "9") |>
          _[findall(map(isdigit, collect(_)))][[1, end]] |>
          parse(Int, _)
end

function day01b(ex=false)
    @pipe input(ex, "b") |>
          split(_, "\n") |>
          filter(x -> x != "") |> # or rstrip
          collect(_) |>
          map(process_b, _) |>
          sum
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "01" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day01a(true))
#println(day01a(false))
day01a(false)
println(@btime day01a(false))

println("***")

println("Part 2:")
println(day01b(true))
day01b(false)
println(@btime day01b(false))
#println(day01b(false))


