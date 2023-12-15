#!/usr/bin/dyalogscript

⍝
⍝p←⊃⎕NGET'../R/inst/input15.txt'1
p←⊃⎕NGET'p15.txt' 1
p←⊃','(≠⊆⊢)¨p
hash←(256|17×+)/∘⌽0,⎕UCS ⍝ credit to Adam
⎕←+/hash¨p