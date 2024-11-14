#!/bin/bash
#SBATCH --job-name="prevalence_10_13"
#SBATCH --time=7-23:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=10G
#SBATCH --partition=epyc2

# Your code below this line
module load Workspace
module load Workspace_Home
module load R
R CMD BATCH --'args cont_n=1 term_n=5000 mth="10" dy="10" aname="nomasksvent" cellSize=250 is_masking=0 fixed_aer=6 who_is_tb="du" mod="spattemp" sel_rooms="clinic" save_quanta=FALSE' simulations/simulate_qctr.R
