#!/bin/bash
#SBATCH --job-name="prevalence_10_13"
#SBATCH --time=02:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=1G
#SBATCH --partition=epyc2

# Your code below this line
R CMD BATCH --'args cont_n=1 term_n=10 mth="10" dy="13" aname="prevalence" cellSize=250 is_masking=1 fixed_aer=0 who_is_tb="pu" mod="spattemp" sel_rooms="clinic" save_quanta=TRUE' simulations/simulate_qctr.R 
echo "Script finished"