#!/bin/bash

for a in "in" "ex" 
do
	for b in $(seq -10 10) 100 
	do
		for c in "qu" "hi"
		do
			for d in 5 10 20
			do
	 			for e in "pso" "rs" "df" "smbo"
				do
					for f in 5
					do
						for g in "rv" "nv"
						do
							for h in "svm" "J48"
							do
								qsub run-job-expRF-pqh.sh $a $b $c $d $e $f $g $h
								# echo "run-job-expRF-pqh.sh" $a $b $c $d $e $f $g $h
							done
						done
					done
				done
			done
		done
	done
done
