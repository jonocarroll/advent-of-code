#!/usr/bin/dyalogscript

⍝
p←⊃⎕NGET'p01.txt'1
⍝ p←⊃⎕NGET'../R/inst/input01.txt'1
y←{⍎(⊃,⊢/)(⍵∊⎕D)/⍵}¨p
⎕←p1←+/y

⍝
p←⊃⎕NGET'p01b.txt'1
⍝ p←⊃⎕NGET'../R/inst/input01.txt'1
sub←{'one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine'⎕R'o1ne' 't2wo' 'th3ree' 'fo4ur' 'fi5ve' 'si6x' 'sev7en' 'ei8ght' 'ni9ne' ⊢ ⍵}
p←sub ⍣2 ⊢ p
y←{⍎(⊃,⊢/)(⍵∊⎕D)/⍵}¨p
⎕←p2←+/y
