#!/bin/bash

set -eax
proc=(1 2 4)
mach=(1 2 4 8 16)
for P in "${proc[@]}"
do
  for M in "${mach[@]}"
  do
    PROCESSES="$(($P*$M))"
    TARGET=msp-par-test-"$PROCESSES"-$M.ll
    cp msp-par-template.ll "$TARGET"
    sed -i s/__PROCESSES__/"$PROCESSES"/ "$TARGET"
    sed -i s/__MACHINES__/"$M"/ "$TARGET"
    if [ $P -eq 1 ]
    then
      MODE=SMP
    fi
    if [ $P -eq 2 ]
    then
      MODE=DUAL
    fi
    if [ $P -eq 4 ]
    then
      MODE=VN
    fi

    sed -i s/__MODE__/"$MODE"/ "$TARGET"
    if [ "$(($P*$M))" -ge 32 ]
    then
      MINUTES=02
    else
      MINUTES=`printf "%0*d" 2 "$((32 / ($P * $M) + 1))"`
    fi
    sed -i s/__MINUTES__/"$MINUTES"/ "$TARGET"
  done
done
