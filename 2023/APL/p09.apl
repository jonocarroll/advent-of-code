#!/usr/bin/dyalogscript

⎕fr←1287 ⍝ use 128 bit floats

⎕IO←0

⍝
r←≢p←⍉↑' '(⍎¨≠⊆⊢)¨⊃⎕NGET'p09.txt'1
⍝r←≢p←⍉↑' '(⍎¨≠⊆⊢)¨⊃⎕NGET'../R/inst/input09.txt'1
f←{⌈+/r⊥⊖⍵+.×⍨⌹∘.*⍨⍳r}
⎕←f p
⎕←f ⊖ p