#!/usr/bin/dyalogscript

p←⊃⎕NGET'p01.txt'1
lastdigit←{¯1↑⍕⍵}
firstdigit←{1↑⍕⍵}
y←{⍎⍕¨(firstdigit,lastdigit)(~⍵∊⎕c⎕a)/⍵}¨p
⎕←p1←+/y

