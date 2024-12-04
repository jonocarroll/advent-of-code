Input ← &fras "/Users/jono/Projects/advent-of-code/2024/R/inst/input03.txt"
P₁    ← /+≡/×≡(⋕⍜⍉↘1) regex "mul\\((\\d+),(\\d+)\\)" Input # Part 1
# Res    ← ⊡0⍉⬚@0regex "mul\\((\\d+),(\\d+)\\)|don't\\(\\)|do\\(\\)" Input
# DoDont ← ∩≍"don't()","do()"°□

P₁
