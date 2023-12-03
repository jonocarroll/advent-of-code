#!/usr/bin/dyalogscript

⍝
p←⊃⎕NGET'p02.txt'1
⍝ p←⊃⎕NGET'../R/inst/input02.txt'1
∇ res←solve input 
  parts←':'(≠⊆⊢)input
  game←⍎'Game '⎕R''⊃parts
  cubes←∊','(≠⊆⊢)¨(';'(≠⊆⊢)⊃1↓parts)
  counts←'[0-9]+'⎕S'&'⊢∊cubes
  cols←'red' 'green' 'blue'⎕S'&'⊢∊cubes
  tbl←cols{(⊃⍺)(⌈/⍎¨⍵)}⌸counts
  blue←⍸(⊂'blue')≡¨(1⌷⍉tbl)
  red←⍸(⊂'red')≡¨(1⌷⍉tbl)
  green←⍸(⊂'green')≡¨(1⌷⍉tbl)
  notposs←0
  :If ≢red
      notposs←notposs ∨ tbl[;2][red]>12
  :EndIf
  :If ≢green
      notposs←notposs ∨ tbl[;2][green]>13
  :EndIf
  :If ≢blue
      notposs←notposs ∨ tbl[;2][blue]>14
  :EndIf
  res←(~notposs)×game
∇
⎕←+/solve¨p

