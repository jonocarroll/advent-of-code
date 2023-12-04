#!/usr/bin/dyalogscript

⍝
p←⊃⎕NGET'p04.txt'1
⍝ p←⊃⎕NGET'../R/inst/input04.txt'1
∇ res←solve input
  p←'Card.*: '⎕R'' ⊢input
  sep←⍸ p='|'
  winning←⍎¨' '(≠⊆⊢)(sep-1)↑p
  nums←⍎¨' '(≠⊆⊢)p[sep + ⍳(≢p)-sep]
  wins←winning ∩ nums
  count←≢wins
  res←⌊2*(count-1)
∇
⎕←+/solve¨p

⍝ p←⊃⎕nget'p04.txt'1
p←⊃⎕NGET'../R/inst/input04.txt'1
c←{+/⊃∊⍨/⍎¨1↓⍵⊆⍨~⍵∊':|'}¨p
⎕←+/⌊2*c-1 ⍝ part 1
m←1=d∘.⍸⍨c(1+⊢,+)¨d←⍳≢c
⎕←+/(s+m+.×⊢)⍣≡s←1⍨¨c ⍝ part 2