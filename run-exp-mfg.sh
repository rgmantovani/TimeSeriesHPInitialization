#!/bin/bash

for a in $(seq 1 255) 
do
	for b in "rv" "nv"
	do
		for c in "ed" "ip" "cs" "pc"
		do
			for d in 1 2 3
			do
				for e in "pso" "rs" "dfs"
				do
					for f in 5
					do
						qsub run-job-mfg.sh $a $b $c $d $e $f
					done
				done
			done
		done
	done
done

