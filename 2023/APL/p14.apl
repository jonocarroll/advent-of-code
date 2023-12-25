#!/usr/bin/dyalogscript

⍝
⍝p←↑⊃⎕NGET'../R/inst/input14.txt'1
⍝p←↑⊃⎕NGET'p14.txt'1
⍝east←⌽⍉p
⍝roll←{'O\.'⎕R'.O'⊢⍵}⍤1⍣≡⊢east
⍝north←⍉⌽roll
⍝nrows←(1↑⍴north)
⍝⎕←+⌿+⌿('O'=north)×(⌽⍳nrows)∘.×(nrows/1)

⍝r←{'O\.'⎕R'.O'⊢⍵}⍤1⍣≡⊢⌽⍉p
⍝⎕←+/⊢/↑⍸'O'=r
⍝⎕←+/⊢/↑⍸'O'={'O\.'⎕R'.O'⊢⍵}⍤1⍣≡⊢⌽⍉p

⍝ credit: xpqz
p←↑⊃⎕NGET'p14.txt'1
⍝p←↑⊃⎕NGET'../R/inst/input14.txt'1
pp←'#',⍉⌽p
⎕←+/(2+≢p)-⊢/↑⍸'O'={{∊{⍵[⍋'#O.'⍳⍵]}¨⍵⊂⍨'#'=⍵}⍤1⊢⍵}pp