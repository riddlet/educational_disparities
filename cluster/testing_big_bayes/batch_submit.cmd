#!/bin/bash
# serial job using 1 node and 1 processor,
# and runs for 1 minute (max).
#SBATCH -N 1   # node count
#SBATCH --ntasks-per-node=4  # core count
#SBATCH -t 3-00:00:00 
#SBATCH --mem-per-cpu=5000
# sends mail when process begins, and
# when it ends. Make sure you define your email
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mail-user=triddle@princeton.edu

Rscript $1