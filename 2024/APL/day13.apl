⎕pp←22 ⍝ print 22 digits
p←↑↑¨(⍎¨∊∘⎕D⊆⊢)¨¨(⊢⊆⍨0≠≢¨)⊃⎕NGET'inst/input13.txt'1
coins←((3 1+.×⊢×⌈≡⌊)⊢⌿⌹∘⍉¯1↓⊢)⍤2⊢ 
+/ coins p ⍝ part 1
+/ coins p+⍤2⍉0,0,⍪2⍴10000000000000 ⍝ part 2
