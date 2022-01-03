#!/bin/bash
#SBATCH -c 40
#SBATCH --mem=256GB 
#SBATCH -t 0-2:00:00
#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH -o ./results/LDA/co_lda_output0.txt

module load anaconda/2020.11-py3.8

python coherence.py