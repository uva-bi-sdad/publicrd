#!/bin/bash
#SBATCH -c 40
#SBATCH --mem=256GB 
#SBATCH -t 0-20:00:00
#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH -o ./results/LDA/lda_output8-9.txt

module load anaconda/2020.11-py3.8

python topic_models-LDA.py