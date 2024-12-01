p←(⎕CSV⍠'Separator' ' ')'../R/inst/input01.txt'
x←⍎¨p[;1] ⋄ y←⍎¨p[;4]
+/|(x[⍋x]-y[⍋y]) ⍝ Part 1
+/x×+⌿y∘.=x ⍝ Part 2
