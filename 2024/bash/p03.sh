#!/bin/bash

cat ../R/inst/input03.txt | \
  grep -Eo 'mul\([0-9]+,[0-9]+\)' | \
  sed -E 's/mul\(([0-9]+),([0-9]+)\)/\1 * \2/g' | \
  paste -s -d+ - | bc

cat ../R/inst/input03.txt | \
  paste -s - | \
  perl -pe 's/don'\''t\(\).*?do\(\)//g' | \
  grep -Eo 'mul\([0-9]+,[0-9]+\)' | \
  sed -E 's/mul\(([0-9]+),([0-9]+)\)/\1 * \2/g' | \
  paste -s -d+ - | bc
