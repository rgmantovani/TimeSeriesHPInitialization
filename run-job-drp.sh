#!/bin/bash
#$ -S /bin/bash
#$ -cwd
#$ -N drp

Rscript expDrp.R $1 $2 $3

