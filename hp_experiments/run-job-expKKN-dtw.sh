#!/bin/bash
#$ -S /bin/bash
#$ -cwd
#$ -N KNNdtw

Rscript expKNNdtw.R $1 $2 $3 $4 $5 $6 $7
