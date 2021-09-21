#!/bin/bash
#SBATCH -c 20
#SBATCH --mem=128GB 
#SBATCH -t 0-00:20:00
#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH -o ./results/NMF/nmf_output0.txt

module load anaconda/2020.11-py3.8

python topic_models-NMF.py