#!/bin/bash

for a in "in" "ex" 
do
	for b in $(seq -10 10) 100 
	do
		for c in "rv" "nv" 
		do
			for d in 1 2 3
			do
				for e in "pso" "rs" "dfs"
				do
					for f in 5
					do
						qsub run-job-dtw.sh $a $b $c $d $e $f
					done
				done
			done
		done
	done
done

