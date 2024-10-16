#!/bin/bash
#SBATCH --job-name=isthereapulse_%a
##SBATCH --workdir
#SBATCH --output=/scratch/egr65/Pulse-meta-analysis/log/isthereapulse_%A_%a.log
#SBATCH --cpus-per-task=1
#SBATCH --time=3:00:00
#SBATCH --mem=40000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu

### %A is monsoon job number %a is interior array index

module load R/4.1.2 # load a specific R version

chmod +x scripts/07_is_there_a_pulse.R # for permissions

# Run the analysis
srun Rscript scripts/07_is_there_a_pulse.R
