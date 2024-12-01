#!/bin/zsh

infile='../R/inst/input01.txt'
x=$(awk '{print $1}' $infile | sort)
y=$(awk '{print $2}' $infile | sort)
diffs=$(paste -d- <(echo $x) <(echo $y) | bc)
paste -d+ -s <(echo $diffs | sed 's/-//g') | bc

