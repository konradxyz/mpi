#!/bin/bash
#set -eax

FILE="MSP_kp306410_4000x4000_$1_$2.err"
DIRS=("bckv2" "bckv3" "bckv4")
VALS=()
for D in "${DIRS[@]}"
do
  A=`cat $D/*_$1_$2.err | grep "Time:" | sed "s/.*Time: //"`
  echo $A
  VALS+=($A)
done
echo "${VALS[@]}"

MIN="${VALS[0]}"

for V in "${VALS[@]}"
do
  echo $V >> show/$1_$2.show
done
echo "done"
