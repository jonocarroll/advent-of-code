# based on APL:
# c←,p←⍎¨↑⊃⎕NGET'../R/inst/input10.txt'1
# adj←(1=∘.-⍨c)^1=+/¨|∘.-⍨,⍳⍴p
# paths←(c=0)/(c=9)⌿(⊣++.×)⍣≡⍨adj
# ⎕←+/+/paths⍪⍨↑,⊂×paths ⍝ Part 1 and 2

# input = "tmp.txt"
input = "R/inst/input10.txt"

absdiff(x, y) = abs.(x - y)

walk(p, a) = p + (a * p)

p = parse.(Int, stack(split(rstrip(read(input, String)), "\n"), dims=1));
c = vec(p');
pp = vec([[x, y] for x in 1:size(p, 1), y in 1:size(p, 2)]);
adj = ((c .- c').==1) .& (sum.(absdiff.(pp, permutedims(pp))).==1);
paths = foldl((acc, _) -> walk(adj, acc), 1:9, init=adj);
trails = paths[findall(c .== 9), findall(c .== 0)];

println(sum(sign.(trails)))
println(sum(trails))    

