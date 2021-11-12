#!/bin/bash
#SBATCH -c 10
#SBATCH --mem=64GB 
#SBATCH -t 0-5:00:00
#SBATCH -p bii
#SBATCH -A biocomplexity

module load anaconda/2020.11-py3.8

python model_saver.py