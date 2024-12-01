#!/bin/zsh

infile='../R/inst/input01.txt'

## Part 1
x=$(awk '{print $1}' $infile | sort)
y=$(awk '{print $2}' $infile | sort)
diffs=$(paste -d- <(echo $x) <(echo $y) | bc)
paste -d+ -s <(echo $diffs | sed 's/-//g') | bc

## Part 2
xs=$(echo $x | uniq)
acc=0
for i in $(echo $xs); do
    ((acc += $i * $(grep -c $i <(echo $y))))
done
echo $acc



