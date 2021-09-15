#!/bin/bash
#SBATCH -c 40
#SBATCH --mem=256GB 
#SBATCH -t 2-00:00:00
#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH -o output5-9.txt

module load anaconda/2019.10-py3.7

python topic_tune_LSA.py