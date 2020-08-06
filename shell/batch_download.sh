#!/bin/bash
#SBATCH -p serial_requeue
#SBATCH -n 1
#SBATCH --mem=10000
#SBATCH -t 7200
#SBATCH -o /n/scratchlfs/koutrakis_lab/longxiang/Fracking_Radiation/log/download/log_rgn_%j._%a.out
#SBATCH -D /n/koutrakis_lab/lab/Fracking_Radiation/code/
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER

source /n/home02/loli/load_modules.sh

Sim=${SLURM_ARRAY_TASK_ID}

export Sim

Rscript --quiet --no-restore --no-save /n/koutrakis_lab/lab/Fracking_Radiation/code/55_Download_DI_Access.R

sleep 3 # pause to be kind to the scheduler