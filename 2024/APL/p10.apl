⍝ https://youtu.be/gQ5uFQ1pfDE
c←,p←⍎¨↑⊃⎕NGET'../R/inst/input10.txt'1
adj←(1=∘.-⍨c)^1=+/¨|∘.-⍨,⍳⍴p
paths←(c=0)/(c=9)⌿(⊣++.×)⍣≡⍨adj
⎕←+/+/paths⍪⍨↑,⊂×paths ⍝ Part 1 and 2
