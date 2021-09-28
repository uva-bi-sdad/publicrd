#!/bin/bash
#SBATCH -c 40
#SBATCH --mem=256GB 
#SBATCH -t 0-5:00:00
#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH -o ./results/coherence/co_nmf_output7.txt

module load anaconda/2020.11-py3.8

python coherence.py