#!/bin/bash
#SBATCH -c 10
#SBATCH --mem=64GB 
#SBATCH -t 04:00:00
#SBATCH -p bii-gpu
#SBATCH --gres=gpu:1
#SBATCH -A biocomplexity
#SBATCH -o lemma_output.txt

module load anaconda/2020.11-py3.8

python 02-lemmatize.py