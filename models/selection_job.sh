#!/bin/bash
#SBATCH --job-name=Selection
##SBATCH --workdir
#SBATCH --cpus-per-task=1
#SBATCH --time=2:00:00
#SBATCH --mem=50000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu

chmod +x ./03-Selection-simple/01_run_model.R
srun ./03-Selection-simple/01_run_model.R
