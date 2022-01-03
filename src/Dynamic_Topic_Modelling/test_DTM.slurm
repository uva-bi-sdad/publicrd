#!/bin/bash
i#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=30GB
#SBATCH --time=0-20:00:00
#SBATCH --output=test_DTM_LDA.out
#SBATCH --error=test_DTM_LDA.err
#SBATCH --partition=bii
#SBATCH --account=bii_sdad

module load anaconda/2020.11-py3.8

python DTM_LDA.py
