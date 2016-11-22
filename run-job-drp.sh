#!/bin/bash
#$ -S /bin/bash
#$ -cwd
#$ -N drp

Rscript exp-drp.R $1 $2 $3

