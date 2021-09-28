#!/bin/bash

for a in "in" "ex" 
do
  for b in $(seq -10 10) 100 
  do
    for c in "rv" "nv" 
    do
      for d in 1 2 3
      do
        for e in "pso" "rs" "df" "smbo"
        do
          for f in 5
          do
            for g in "svm" "J48"
            do
              qsub run-job-expKNN-dtw.sh $a $b $c $d $e $f $g
            done
          done
        done
      done
    done
  done
done

