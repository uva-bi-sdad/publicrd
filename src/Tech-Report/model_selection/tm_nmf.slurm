#!/bin/bash
#SBATCH -c 40
#SBATCH --mem=256GB 
#SBATCH -t 2-00:00:00
#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH -o ./results/NMF/nmf_output4.txt

module load anaconda/2020.11-py3.8

python topic_models-NMF.py