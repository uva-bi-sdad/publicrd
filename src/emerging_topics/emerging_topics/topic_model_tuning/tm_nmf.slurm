#!/bin/bash
#SBATCH -c 40
#SBATCH --mem=256GB 
#SBATCH -t 0-01:00:00
#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH -o nmf_output-test.txt

module load anaconda/2020.11-py3.8

python topic_models-NMF.py