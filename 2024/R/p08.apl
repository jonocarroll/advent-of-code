⍝p←↑⊃⎕NGET'../tmp.txt'1
p←↑⊃⎕NGET'inst/input08.txt'1

∇ res← char solve p
  sym←⍸char=p
  ut←∘.<⍨⍳≢sym
  d←∘.-⍨sym
  del←d×ut
  offsets←↓del,¯2×del
  offsets←(0≠×/¨¨offsets)/¨offsets
  np←,({(⊂⊃⍵)+(⊃1↓⍵)}⍤1)(¯1↓⍉↑sym offsets)
  ib←((2=+/¨(⊂1 1)∘.≤np)^(2=+/¨(⊂⍴p)∘.≥np))/np
  res←'#'=('#'@ib)p
∇

∇ res← char solve2 p
  sym←⍸char=p
  ut←∘.<⍨⍳≢sym
  d←∘.-⍨sym
  del←d×ut
  offsets←↓⊃,/{⍵×del}¨((-⍳60),0,(⍳60))
  ⍝offsets←(0≠×/¨¨offsets)/¨offsets
  np←,({(⊂⊃⍵)+(⊃1↓⍵)}⍤1)(¯1↓⍉↑sym offsets)
  ib←((2=+/¨(⊂1 1)∘.≤np)^(2=+/¨(⊂⍴p)∘.≥np))/np
  res←'#'=('#'@ib)p
∇

syms←({'.'≠⍵}∪,p)/(∪,p)
+/+/⊃∨/{⍵ solve p}¨syms ⍝ part 1
+/+/⊃∨/{⍵ solve2 p}¨syms ⍝ part 2
