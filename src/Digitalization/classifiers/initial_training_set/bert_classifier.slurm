#!/bin/bash
#SBATCH -c 40
#SBATCH --mem=256GB
#SBATCH -t 3-00:00:00
#SBATCH -p bii-gpu
#SBATCH --gres=gpu:1
#SBATCH -A biocomplexity
#SBATCH -o initial_BERT_model_output.txt

module load anaconda
source activate bert_new

python initial_BERT_model.py