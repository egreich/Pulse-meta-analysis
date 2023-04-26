#!/bin/bash
#SBATCH --job-name=Mixture
##SBATCH --workdir
#SBATCH --cpus-per-task=1
#SBATCH --time=2:00:00
#SBATCH --mem=50000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu

chmod +x models/04-Mixture-simple/01-run-model.R
srun Rscript models/04-Mixture-simple/01-run-model.R
