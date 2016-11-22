#!/bin/bash
#$ -S /bin/bash
#$ -cwd
#$ -N dtw

Rscript exp-dtw.R $1 $2 $3 $4 $5 $6

