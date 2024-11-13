#!/bin/bash
#SBATCH --job-name="prevalence_10_13"
#SBATCH --time=02:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=1G
#SBATCH --partition=epyc2

# Your code below this line
R CMD BATCH --vanilla preprocessing/simulate_qctr.R 1 10 10 13 prevalence 250 1 0 pu spattemp clinic T || exit 91
echo "Script finished"