input = "R/inst/input06.txt"
M = stack(split(rstrip(read(input, String)), "\n"), dims=1)
startpos = findfirst(z -> z == '^', M)
pos = startpos
blocks = findall(z -> z == '#', M)
startdir = CartesianIndex(-1, 0)
dir = startdir
visited = []

function turn_right(d) 
    if d == CartesianIndex(-1, 0) return CartesianIndex(0, 1) end
    if d == CartesianIndex(0, 1) return CartesianIndex(1, 0) end 
    if d == CartesianIndex(1, 0) return CartesianIndex(0, -1) end
    if d == CartesianIndex(0, -1) return CartesianIndex(-1, 0) end
end

while checkbounds(Bool, M, pos[1], pos[2])
    if CartesianIndex(pos + dir) in blocks
        dir = turn_right(dir)
    else 
        push!(visited, pos)
        pos += dir
    end
end

length(unique(visited))

function walkroute(b, pos, dir)
    b_seen = zeros(length(b))
    while checkbounds(Bool, M, pos[1], pos[2])
        if CartesianIndex(pos + dir) in b
            this_b = findfirst(z -> z == CartesianIndex(pos + dir), b)
            b_seen[this_b] += 1
            if any(x -> x > 3, b_seen) 
                return 1
            end
            dir = turn_right(dir)
        else 
            pos += dir
        end
    end
    return 0
end

n = 0
for newpos in unique(visited)
    tmpblocks = push!(deepcopy(blocks), newpos)
    n += walkroute(tmpblocks, startpos, startdir)
end
n
