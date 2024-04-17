#!/bin/bash
#SBATCH --partition=day-long-cpu,preemptable
#SBATCH --output dsq-jobfile-%A_%a-%N.out
#SBATCH --array 0
#SBATCH --job-name make-crossfit-HD-truth
#SBATCH --mail-type "ALL"
#SBATCH --mail-user=anna.guo@emory.edu
#SBATCH --mem 1G

sim=4-crossfitting-HD
input=in 

make

input=not
make