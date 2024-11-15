#!/bin/bash

#* This script defines the job to execute a cpcbayeslm package using 
#* a job/task array on a computer cluster

#SBATCH --array=1-5                           # Number of arrays of the job
#SBATCH --cpus-per-task=1                     # one CPU core per task
#SBATCH --mem-per-cpu=10G                     # memory per CPU: 10GB
#SBATCH --time=10:00:00                       # requested time per task: 10 hours
#SBATCH --mail-type=ALL                       # receive all job notifications via email
#SBATCH --mail-user=prinpep@gmail.com         # mail to where the job notifications will be sent
#SBATCH --output=slurm_output/slurm-%A-%a.out      # Location where the slurm outputs will be saved

#* Check full list of SLURM arguments in the next link:
#* - https://slurm.schedmd.com/sbatch.html

#* -----   To run this script ---------
#* - sbatch this_sbatch_file.sh


echo "The job ID is" $SLURM_ARRAY_JOB_ID          #* print the job ID to the screen
echo "The task ID is" $SLURM_ARRAY_TASK_ID
echo "The number of cpus per task are:" $SLURM_CPUS_PER_TASK


# # Load software
# module load R

#* Run the `analysis/01_check.R` script. 
#* This script imports three arguments:
#*  - The number of CPU's per task
Rscript analysis/01_check.R $SLURM_CPUS_PER_TASK