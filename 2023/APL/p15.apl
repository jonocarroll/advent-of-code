#!/usr/bin/dyalogscript

⍝
⍝p←⊃⎕NGET'../R/inst/input15.txt'1
p←⊃⎕NGET'p15.txt' 1
p←⊃','(≠⊆⊢)¨p
hash←{⍺←0 ⋄ 0=≢⍵:⍺ ⋄(256|17×⍺+⎕UCS⊃⍵)∇ 1↓⍵}
⎕←+/hash¨p