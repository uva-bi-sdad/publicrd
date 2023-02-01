#!/bin/bash
#SBATCH -c 40
#SBATCH --mem=256GB 
#SBATCH -t 3-00:00:00
#SBATCH -p bii
#SBATCH -A sds_sdad
#SBATCH -o ./Doc2Vec_model_output.txt

module load anaconda/2020.11-py3.8

python Doc2Vec_model.py