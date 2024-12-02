reports = split.(readlines("R/inst/input02.txt"))

function is_safe(x) 
    v = diff(parse.(Int, x));
    all(Base.between.(v, 1, 3)) | all(Base.between.(v, -3, -1))
end

# Part 1
sum(is_safe.(reports))

# leave one out
function loo(x) 
    vars = [];
    map(i -> push!(vars, deleteat!(deepcopy(x), i)), eachindex(x));
    vars
end

function is_safe_2(x) 
    any(is_safe.(loo(x)))
end

# Part 2
sum(is_safe_2.(reports))
