#!/bin/bash

for a in "in" "ex" 
do
	for b in $(seq -10 10) 100 
	do
		for c in "qu" "hi"
		do
			for d in 5 10 20
			do
				for e in "ed" "ip" "cs" "pc"
				do
					for f in 1 2 3
					do
						for g in "pso" "rs" "df" "smbo"
						do
							for h in 5
							do
								for i in "rv" "nv"
								do
									for j in "svm" "J48"
									do
										qsub run-job-expKNN-pqh.sh $a $b $c $d $e $f $g $h $i $j
									done
								done
							done
						done
					done
				done
			done
		done
	done
done

