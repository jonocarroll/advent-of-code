function f17(M,R)
    n = size(M,1)
    H,V = fill(sum(M),n,n),fill(sum(M),n,n)
    H[1] = V[1] = 0
    while true
        Σ = sum(H)+sum(V)
        for δ∈R, t∈1:n
            t>=δ+1 && (V[[t],:].=min.(V[[t],:],H[[t-δ],:]+sum(M[t-δ+1:t,:],dims=1)))
            t+δ<=n && (V[[t],:].=min.(V[[t],:],H[[t+δ],:]+sum(M[t:t+δ-1,:],dims=1)))
            t>=δ+1 && (H[:,[t]].=min.(H[:,[t]],V[:,[t-δ]]+sum(M[:,t-δ+1:t],dims=2)))
            t+δ<=n && (H[:,[t]].=min.(H[:,[t]],V[:,[t+δ]]+sum(M[:,t:t+δ-1],dims=2))) 
        end
        Σ==sum(H)+sum(V) && break
    end
    min(last(H),last(V))    
end

M = hcat(collect.(readlines("./inst/input17.txt"))...).-'0'
println(f17(M,1:3))
println(f17(M,4:10))