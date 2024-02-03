using Pipe

function day08a(ex=false)
    x = split(input(ex), "\n")
    instr = repeat(x[1], 100)
    nodes = process_a(input(ex))
    this_node = "AAA"
    steps = 0
    for i in instr
        dir = i == 'L' ? 1 : 2
        this_node = nodes[this_node][dir]
        steps += 1
        if this_node == "ZZZ"
            break
        end
    end
    if this_node ≠ "ZZZ"
        error("not enough")
    end
    steps
end

function process_a(x)
    xx = split.(replace.(split(rstrip(x), "\n")[3:end], r"[)(]" => s""), " = ")
    d = Dict()
    for i in xx
        merge!(d, Dict(first(i) => split(last(i), ", ")))
    end
    d
end

function day08b(ex=false)
    y = input(ex)
    x = split(y, "\n")
    instr = repeat(x[1], 100)
    nodes = process_a(y)
    this_node = filter(z -> endswith(z.first, 'A'), nodes)
    node_steps = Vector{Int}()
    for tn in keys(this_node)
        node_i = tn
        steps = 0
        for i in instr
            node_i = nodes[node_i][i == 'L' ? 1 : 2]
            steps += 1
            if endswith(node_i, 'Z')
                break
            end
        end
        push!(node_steps, steps)
    end
    lcm(node_steps...)
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "08" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day08a(true))
day08a(false)
println(@btime day08a(false))

println("***")

println("Part 2:")
println(day08b(true))
day08b(false)
println(@btime day08b(false))


