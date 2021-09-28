#!/bin/bash

for a in "pso" "rs" "df" "smbo"
do
  for b in 5
  do
    for c in "svm" "J48"
    do
      qsub run-job-drp.sh $a $b $c
    done
  done
done
