#!/usr/bin/dyalogscript

⍝
p←⊃⎕NGET'p01.txt'1
⍝ p←⊃⎕NGET'../R/inst/input01.txt'1
lastdigit←{¯1↑⍕⍵}
firstdigit←{1↑⍕⍵}
y←{⍎⍕¨(firstdigit,lastdigit)(~⍵∊⎕c⎕a)/⍵}¨p
⎕←p1←+/y

⍝
p←⊃⎕NGET'p01b.txt'1
⍝ p←⊃⎕NGET'../R/inst/input01.txt'1
⍝ TODO: figure out how to use power= (⍣=) to achieve this, otherwise just do it x3
p←'one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine' ⎕R 'o1ne' 't2wo' 'thr3ee' 'fo4ur' 'fi5ve' 'si6x' 'sev7en' 'eig8ht' 'ni9ne' ⊢ p
p←'one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine' ⎕R 'o1ne' 't2wo' 'thr3ee' 'fo4ur' 'fi5ve' 'si6x' 'sev7en' 'eig8ht' 'ni9ne' ⊢ p
p←'one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine' ⎕R 'o1ne' 't2wo' 'thr3ee' 'fo4ur' 'fi5ve' 'si6x' 'sev7en' 'eig8ht' 'ni9ne' ⊢ p
y←{⍎⍕¨(firstdigit,lastdigit)(~⍵∊⎕c⎕a)/⍵}¨p
⎕←p2←+/y
