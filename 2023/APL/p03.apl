#!/usr/bin/dyalogscript

⍝
p←↑⊃⎕NGET'p03.txt'1
⍝ p←↑⊃⎕NGET'../R/inst/input03.txt'1
n←{⍎¨⊃,/{⍵(∊⊆⊣)⎕D}¨↓⍵} ⍝ extract the part numbers

nums←n p
locs←{(∨/~(⎕D,' .')∊⍨,⍵)∧⎕D∊⍨2 2⌷⍵}⌺3 3⊢p ⍝ 3x3 window contains digit and symbol
⎕←+/nums/⍨2∊¨0(≠⊆⊢),(p∊⎕D)+locs

d←(⊂2 4)+(⊂0 0)~⍨,∘.,⍨¯1 0 1
g←,{(2=≢⊃,/{⍵(∊⊆⊣)⎕D}¨↓⍵)∧'*'=2 2⌷⍵}⌺3 3
⎕←+/{×/(n ⍵)/⍨2∊¨⊃,/0(≠⊆⊢)¨↓(⍵∊⎕D)(⊣+∧)(1@d)0⍨¨⍵}¨(g p)/,{⊂⍵}⌺3 7⊢p