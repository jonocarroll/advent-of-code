import networkx as nx

# with open("../tmp3.txt") as f:
with open("../R/inst/input23.txt") as f:
    input = f.read().splitlines()

cliques = list(nx.enumerate_all_cliques(nx.Graph([x.split("-") for x in input])))

# part 1
print(len([x for x in cliques if len(x) == 3 and any(y[0] == "t" for y in x)]))

# part 2
print(','.join(sorted(max(cliques, key=len))))