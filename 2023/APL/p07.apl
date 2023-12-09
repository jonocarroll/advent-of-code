#!/usr/bin/dyalogscript

⍝
⍝p←↑' '(≠⊆⊢)¨⊃⎕NGET'p07.txt'1
p←↑' '(≠⊆⊢)¨⊃⎕NGET'../R/inst/input07.txt'1
hands←⊣/p
bids←⍎¨⊢/p
f←{10⊥2↑{⍵[⍒⍵]}⊢∘≢⌸⍵}
r←'TJQKA',⍨2↓⎕D
⎕←+/r{bids×⍋⍋⍵,⍺⍳↑hands}f¨hands

p←↑' '(≠⊆⊢)¨⊃⎕NGET'p07.txt'1
⎕←+/('TJQKA',⍨2↓⎕D){(⍎¨⊢/p)×⍋⍋⍵,⍺⍳↑⊣/p}{10⊥2↑{⍵[⍒⍵]}⊢∘≢⌸⍵}¨⊣/p

