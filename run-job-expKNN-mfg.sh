#!/bin/bash
#$ -S /bin/bash
#$ -cwd
#$ -N KNNmfg

Rscript expKNNmfg.R $1 $2 $3 $4 $5 $6 $7

