#!/bin/bash
#$ -S /bin/bash
#$ -cwd
#$ -N mfg

Rscript exp-mfg.R $1 $2 $3 $4 $5 $6

