from functools import cache
import math

@cache
def blink(x, n):
    if n==0:
        return 1
    if x == 0:
        return blink(1, n-1)
    digits = math.floor(math.log10(x)) + 1
    if digits % 2 == 0:
        den = (10 ** (digits // 2))
        return blink(x // den, n - 1) + blink(x % den, n - 1)
    return blink(x * 2024, n - 1)

with open("../R/inst/input11.txt") as f:
    input = f.read().split(" ")

stones = [int(x) for x in input]

print(sum([blink(s, 25) for s in stones])) # Part 1
print(sum([blink(s, 75) for s in stones])) # Part 2
