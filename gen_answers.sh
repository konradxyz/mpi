#!/bin/bash

set -eax
proc=(1 2 4)
mach=(1 2 4 8 16)
for P in "${proc[@]}"
do
  for M in "${mach[@]}"
  do
    ./gen_show.sh "$(($P*$M))" $M
  done
done
