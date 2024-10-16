#!/bin/bash
#SBATCH --job-name=determine_%a
##SBATCH --workdir
#SBATCH --output=/scratch/egr65/Pulse-meta-analysis/log/determine_weights_%A_%a.log
#SBATCH --cpus-per-task=1
#SBATCH --time=3:00:00
#SBATCH --mem=40000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu

### %A is monsoon job number %a is interior array index

module load R/4.1.2 # load a specific R version

chmod +x scripts/06_determine_mixture_weights.R # for permissions

# Run the analysis
srun Rscript scripts/06_determine_mixture_weights.R
