#!/bin/bash

#SBATCH -c 1  # Number of Cores per Task
#SBATCH --mem=20000  # Requested Memory
#SBATCH -p cpu  # Partition
#SBATCH -t 48:00:00  # Job time limit
#SBATCH -o kfold4_all_training.out
#SBATCH -e kfold4_all_training.err



#for training on cpu

module purge

../containers/detectron2-cpu-docker.simg python3 training_stratifiedKfold4/train_model.py


