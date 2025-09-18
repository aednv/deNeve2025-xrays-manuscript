#tb1gt1ra3 vs gt1ra3 + tb1ra3 comparison

#libraries
library(lme4)
library(emmeans)
library(lmerTest)

#import dataframes post cleaning
setwd("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_24UM/results")
#this combined dataset csv was generated in Jupyter notebook "" , and has already been filtered for possible duplicates due to scan overlap and for segmentations that intersect with the image perimeter
allFilteredDf <- read_csv("joined_df_pre_5_pistil_count_filter.csv") #both 21 and 24UM data here

allFilteredDf$genotype <- as.factor(allFilteredDf$genotype)


#number of pistils counted per individual plant, per genotype
counts <- allFilteredNoTreatmentDf %>% group_by(genotype, plant_id) %>% summarise(pistil_count = n())

#number of pistils counted per individual plant, per genotype
allFilteredNoTreatmentDf %>% group_by(genotype, plant_id) %>% summarise(pistil_count = n()) %>% ggplot(aes(x=genotype,y=pistil_count)) + 
  geom_boxplot() + geom_jitter(width=.1,alpha=.5) + 
  ggtitle("number of pistils counted per individual plant, per genotype") %>% ggplot(aes(x=genotype,y=pistil_count)) + 
  geom_boxplot() + geom_jitter(width=.1,alpha=.5) + 
  ggtitle("number of pistils counted per individual plant, per genotype")


#filter for only plants with more than 5 pistil mask detections, only looking at gt1ra3B73 plants
allFilteredDf <- allFilteredDf %>%
  add_count(genotype, plant_id, name = "pistil_count") %>%
  filter(pistil_count >5)


setwd("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_21UM/results")
library(tidyverse)
library(ggbeeswarm)

#this combined dataset has already been filtered for possible duplicates due to scan overlap and for segmentations that intersect with the image perimeter
all21FilteredDf <- read_csv("all_filtered_combined.csv")

all21FilteredDf$genotype <- as.factor(all21FilteredDf$genotype)
all21FilteredDf$pistil_area <- as.numeric(all21FilteredDf$pistil_area)


#1. ~~ Pistil size comparison between genotypes ~~

#just considering non-treated plants
all21FilteredNoTreatmentDf <- all21FilteredDf %>% filter(treatment == "no_treatment")

#clean plant id label names
all21FilteredNoTreatmentDf$plant_id <- gsub("/", "", all21FilteredNoTreatmentDf$plant_id)
all21FilteredNoTreatmentDf$plant_id <- gsub("_", "", all21FilteredNoTreatmentDf$plant_id)

#data distribution
ggplot(all21FilteredNoTreatmentDf, aes(x=pistil_area)) + geom_histogram() + 
  facet_wrap(~genotype) + ggtitle("Pistil size distribution per genotype")

#number of individual plants per genotype, unequal sampling
all21FilteredNoTreatmentDf %>% group_by(genotype) %>% summarise(plant_count = n_distinct(plant_id))