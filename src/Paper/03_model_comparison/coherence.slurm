#!/bin/bash
#SBATCH -c 40
#SBATCH --mem=256GB 
#SBATCH -t 0-4:00:00
#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH -o ./results/NMF/co_nmf_output4-6.txt

module load anaconda/2020.11-py3.8

python coherence.py