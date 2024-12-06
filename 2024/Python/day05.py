import functools

with open("../R/inst/input05.txt") as f:
    input = f.read().splitlines()

rules = list(filter(lambda x: len(x) == 5, input))
updates = list(filter(lambda x: len(x) > 5, input))
updates = [x.split(",") for x in updates]

def lt_bool(a, b):
    return f"{a}|{b}" in rules

def lt_cmp(a, b):
    res = -1 if lt_bool(a, b) else 1
    return res

def is_sorted(x):
    return all(lt_bool(x[i], x[i+1]) for i in range(len(x)-1))

def mid(x):
    return int(x[(len(x)-1)//2])

p1 = p2 = 0
for update in updates:
    if (is_sorted(update)):
        p1 += mid(update)
    else:
        p2 += mid(sorted(update, key = functools.cmp_to_key(lt_cmp)))

print (p1, p2)