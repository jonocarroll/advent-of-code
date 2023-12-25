#!/usr/bin/dyalogscript

⍝ credit: rabbitgrowth
R A←0 1
workflows parts←(×⍤≢¨⊆⊢)⊃⎕NGET'p19.txt'1
(⍎'{' ',' '[a-z](?=[,}])'⎕R'←{' '⋄' '&⍵')¨workflows
⎕←+/(⍎'⋄+/x m a s×in⍬',⍨'[{}]' ',' '='⎕R'' '⋄' '←')¨parts