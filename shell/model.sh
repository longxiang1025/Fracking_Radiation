#!/bin/bash
#SBATCH -p serial_requeue
#SBATCH -n 1
#SBATCH --mem=160000
#SBATCH -t 1240
#SBATCH -o /n/scratchlfs/koutrakis_lab/longxiang/beta_model/log/log_%j._%a.out
#SBATCH -D /n/koutrakis_lab/lab/Fracking_Radiation/code/
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER

 module load R_core/3.4.2-fasrc01
 module load R_packages/3.4.2-fasrc02
Sim=${SLURM_ARRAY_TASK_ID}

export Sim
cd /n/koutrakis_lab/lab/Fracking_Radiation/code/

Rscript --quiet --no-restore --no-save /n/koutrakis_lab/lab/Fracking_Radiation/code/57_Daily_Beta_Model.R

sleep 3 # pause to be kind to the scheduler