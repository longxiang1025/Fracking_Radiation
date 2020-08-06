#!/bin/bash
#SBATCH -p serial_requeue
#SBATCH -n 1
#SBATCH --mem=3000
#SBATCH -t 3000


#SBATCH -D /net/fs2k01/srv/export/koutrakis_lab/share_root/lab/Fracking_Radiation/log/log_hysplit
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER

 module load R_core/3.4.2-fasrc01
 module load R_packages/3.4.2-fasrc02
Sim=${SLURM_ARRAY_TASK_ID}

export Sim

#Rscript --quiet --no-restore --no-save /n/koutrakis_lab/lab/Fracking_Radiation/code/37_Batch_City_Prod.R
Rscript --quiet --no-restore --no-save /n/koutrakis_lab/lab/Fracking_Radiation/code/39_Batch_HYSPLIT.R

sleep 5 # pause to be kind to the scheduler