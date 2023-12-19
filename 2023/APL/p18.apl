#!/usr/bin/dyalogscript

⍝'det'⎕cy'dfns' ⍝ )copy dfns det

⍝
p←' '(≠⊆⊢)¨⊃⎕NGET'../R/inst/input18.txt'1
⍝p←' '(≠⊆⊢)¨⊃⎕NGET'p18.txt' 1
dist←⍎¨⊣/↑1↓¨p
dirs←(1 0)(0 ¯1)(0 1)(¯1 0)['DLRU' ⍸ ⊃¨p]
pts←↓⍉↑+⍀dist×dirs
x←2,⌿⊃pts[1] ⋄ y←2,⌿⊃pts[2]
fill←2÷⍨|+/x{-/⍺×⌽⍵}¨y ⍝ or x{det↑⍺ ⍵}¨y with dfns.det
⎕←fill + (2÷⍨+/dist) + 1