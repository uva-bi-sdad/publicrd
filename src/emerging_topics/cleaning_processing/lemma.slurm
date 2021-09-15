#!/bin/bash
#SBATCH -c 40
#SBATCH --mem=256GB 
#SBATCH -t 2-12:00:00
#SBATCH -p bii-gpu
#SBATCH -A biocomplexity
#SBATCH -o lemma_output.txt

module load anaconda/2019.10-py3.7

python lemmatize_docs.py