p←↑⊃⎕NGET'../R/inst/input04.txt' 1
count←{+/,(⊆'XMAS' 'SAMX')∘.≡⍵}
count1←{count,↓({∊,⊂⍵}⌺1 4)⍵}
hv←(count1 p) + (count1 ⍉p)
i1←⍸,∘.=⍨(⍳4) ⋄ i2←⍸,⌽∘.=⍨(⍳4)
d1←count{⍵[i1]}¨,(↓({,⍵}⌺4 4)p)
d2←count{⍵[i2]}¨,(↓({,⍵}⌺4 4)p)
hv+d1+d2 ⍝ Part 1

xi←⍸,(⌽⌈⊢)∘.=⍨(⍳3)
mas←{+/,(⊆'MSAMS' 'MMASS' 'SMASM' 'SSAMM')∘.≡⍵}
mas{⍵[xi]}¨,(↓({,⍵}⌺3 3)p) ⍝ Part 2
