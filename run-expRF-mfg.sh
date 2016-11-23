#!/bin/bash

for a in $(seq 1 255)
do
	for b in "rv" "nv"
	do
		for c in "pso" "rs" "df" "smbo"
		do
			for d in 5
			do
				for e in "svm" "J48"
				do
					qsub run-job-expRF-mfg.sh $a $b $c $d $e
					# echo "rrun-job-expRF-mfg.sh" $a $b $c $d $e
				done
			done
		done
	done
done
