#!/bin/bash

for a in "pso" "rs" "dfs"
do
	for b in 5
	do
		qsub run-job-drp.sh $a $b
	done
done

