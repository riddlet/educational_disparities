#!/bin/bash
#SBATCH -N 1   # node count
#SBATCH --ntasks-per-node=4  # core count
#SBATCH -t 3-00:00:00 
#SBATCH --mem-per-cpu=5000


Rscript --no-save --no-restore --verbose $1 > $1.Rout