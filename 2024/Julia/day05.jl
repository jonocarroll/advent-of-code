input = "../R/inst/input05.txt"
rules, updates = split(read(input, String), r"\R\R")
rules = split(rules)
updates = split.(split(updates), ',')

lt = (a, b) -> "$a|$b" in rules
mid = x -> parse(Int, x[div(end,2)+1])

p1 = 0
p2 = 0
for update in updates
    issorted(update; lt) ? p1 += mid(update) : p2 += mid(sort(update; lt))
end

p1, p2
