#!/usr/bin/dyalogscript MAXWS=2G

⍝
⍝p←⊃⎕NGET'p06.txt'1
p←⊃⎕NGET'../R/inst/input06.txt'1
m←{∊⍎¨1↓':'(≠⊆⊢)⍵}¨p
dist←{n←⍳⍺ ⋄ +/⍵<n×(⍺-n)}
⎕←×/dist/⍉↑m

m2←{∊⍎¨1↓':'(≠⊆⊢)(~' '=⍵)/⍵}¨p
⎕←×/dist/⍉↑m2
