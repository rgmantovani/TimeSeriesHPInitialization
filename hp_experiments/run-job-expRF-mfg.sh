#!/bin/bash
#$ -S /bin/bash
#$ -cwd
#$ -N RFmfg

Rscript expRFmfg.R $1 $2 $3 $4 $5
