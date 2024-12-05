# https://github.com/tommyip/aoc2024/blob/main/uiua/day04.ua

R  ← ⍥(≡⇌⍉)⇡4¤
D  ← ⊡⍉⊟.⇡⊸⧻
P₁ ← ⬚@ ⧈(≡(≍"XMAS")⊂≡⊃D⊢R)4_4
P₂ ← ⧈(=2/+≡(≍"MAS"D)R)3_3

&fras "/Users/jono/Projects/advent-of-code/2024/R/inst/input04.txt"
∩(/+♭)⊃P₂P₁⊜∘⊸≠@\n
