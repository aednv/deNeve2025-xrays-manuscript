#!/bin/bash

#SBATCH -c 1  # Number of Cores per Task
#SBATCH --mem=20000  # Requested Memory
#SBATCH -p cpu-long  # Partition
#SBATCH -t 48:00:00  # Job time limit
#SBATCH -o augmented_all_training.out
#SBATCH -e augmented_all_training.err



#for training on cpu

module purge
module load singularity/3.7.0

singularity exec ../../containers/detectron2-cpu-docker.simg python3 train_model.py


