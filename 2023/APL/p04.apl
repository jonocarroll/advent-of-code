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
  :If count=0
      res←0
  :EndIf
  :If count>0
      res←2*(count-1)
  :EndIf
∇
⎕←+/solve¨ p

