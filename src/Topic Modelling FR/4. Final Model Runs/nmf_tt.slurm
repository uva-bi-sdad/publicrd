#!/bin/bash
#SBATCH -c 40
#SBATCH --mem=256GB 
#SBATCH -t 2-12:00:00
#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH -o output123.txt

module load anaconda/2019.10-py3.7

python topic_tune_nmf.py