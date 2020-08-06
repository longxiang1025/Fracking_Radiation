#!/bin/bash
#SBATCH -p serial_requeue
#SBATCH -n 1
#SBATCH --mem=24000
#SBATCH -t 3600
#SBATCH -D /net/fs2k01/srv/export/koutrakis_lab/share_root/lab/Fracking_Radiation/log/prod/
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER

 module load R_core/3.4.2-fasrc01
 module load R_packages/3.4.2-fasrc02
Sim=${SLURM_ARRAY_TASK_ID}

export Sim

Rscript --quiet --no-restore --no-save /n/koutrakis_lab/lab/Fracking_Radiation/code/Re_37_Batch_City_Prod.R
Rscript --quiet --no-restore --no-save /n/koutrakis_lab/lab/Fracking_Radiation/code/Re_56_Merge_Daily_Prod_With_NARR_HYSPLIT.R

sleep 3 # pause to be kind to the scheduler