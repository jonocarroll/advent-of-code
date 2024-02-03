
function day15a(ex=false)
    seqs = split(rstrip(input(ex)), ",")
    sum(init.(seqs))
end

function init(x::AbstractString)
    vals = Int.(only.(eachsplit(x, "")))
    ans = 0
    for i in vals
        ans = 17 * (ans + i) % 256
    end
    ans
end

struct lens
    label::String
    focal::Int
end

function day15b(ex=false)
    seqs = split.(split(rstrip(input(ex)), ","), r"(?=\=|-)")
    boxes = [Vector{lens}() for _ in 1:256]
    # for b in 1:256
    #     boxes[b] = []
    # end
    for instr in seqs
        op = SubString(instr[2], 1, 1)
        label = instr[1]
        initlabel = init(label)
        focal = replace(instr[2], r"[-=]" => "")
        if !isempty(focal)
            focal = parse(Int, focal)
        end
        thisbox = boxes[initlabel+1]
        if op == "-"
            if !isempty(thisbox) && label in [a.label for a in thisbox]
                deleteat!(thisbox, findall(z -> z.label == label, thisbox))
            end
        else
            if !isempty(thisbox) && label in [a.label for a in thisbox]
                for box in eachindex(thisbox)
                    if thisbox[box].label == label
                        thisbox[box] = lens(label, focal)
                    end
                end
            else
                push!(thisbox, lens(label, focal))
            end
        end
    end
    ans = 0
    for b in eachindex(boxes)
        if !isempty(boxes[b])
            ans += b * powers(boxes[b])
        end
    end
    ans
end

function powers(box::Vector)
    p = 0
    for j in eachindex(box)
        p += j * box[j].focal
    end
    p
end

function input(ex=false, part="a")
    filename = ex ? "example" : "input"
    part = (part == "a")^ex ? "" : "b"
    path::String = joinpath(@__DIR__, "..", "data", filename * "15" * part * ".txt")
    s = open(path, "r") do file
        read(file, String)
    end
    return s
end

using BenchmarkTools

println("Part 1:")
println(day15a(true))
day15a(false)
println(@btime day15a(false))

println("***")

println("Part 2:")
println(day15b(true))
day15b(false)
println(@btime day15b(false))


