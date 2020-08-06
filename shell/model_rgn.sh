#!/bin/bash
#SBATCH -p serial_requeue
#SBATCH -n 1
#SBATCH --mem=18000
#SBATCH -t 1280
#SBATCH -o /n/scratchlfs/koutrakis_lab/longxiang/beta_model/log/log_rgn_%j._%a.out
#SBATCH -D /n/scratchlfs/koutrakis_lab/longxiang/Fracking_Radiation/code/
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER

source /n/home02/loli/load_modules.sh

Sim=${SLURM_ARRAY_TASK_ID}

export Sim

Rscript --quiet --no-restore --no-save /n/scratchlfs/koutrakis_lab/longxiang/Fracking_Radiation/code/63_Beta_Regional_Daily_Model.R
#Rscript --quiet --no-restore --no-save /n/scratchlfs/koutrakis_lab/longxiang/Fracking_Radiation/code/57_Daily_Beta_Model.R

sleep 3 # pause to be kind to the scheduler