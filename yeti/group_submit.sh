#!/bin/bash
#group_submit.sh

# Directives
#PBS -N educ_disp
#PBS -W group_list=yetipsych
#PBS -l nodes=1:ppn=6
#PBS -l mem=2048mb
#PBS -l walltime=24:00:00
#PBS -M tar2119@columbia.edu
#PBS -m a
#PBS -V

# Set output and error directories
#PBS -o localhost:/vega/psych/users/tar2119/out
#PBS -e localhost:/vega/psych/users/tar2119/err
echo '1 node, 6 processors, 2048mb, walltime 24hrs'
date

module load gcc/4.9.1
R CMD BATCH --no-save --vanilla $1 $1.routput
date


#to run multiple files: for i in *.R; do qsub -F "$i$" group_submit.sh; done
# End of script