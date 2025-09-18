#SHAPE ANALYSIS WITH 21UM and 24UM Datasets

#install libraries

library(Momocs) #older version 1.2.2
library(tidyverse)
library(ggbeeswarm)

#for installing older Momocs version
#install.packages("devtools")
#devtools::install_version("Momocs", version = "1.2.2")

# read in 21UM .nef file and 24UM .nef file from SHAPE program
cObj21 <- nef2Coe("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_21UM/masks/efd/masks_chc.nef")

cObj24 <- nef2Coe("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_24UM/results/24umshapefinal.nef")

#remove extra column

cObj21 <- subset(cObj21, select = -A1)
cObj24 <- subset(cObj24, select = -A1)

#rename the 24UM mask names
rownames(cObj24) <- paste0(rownames(cObj24), "2")

#join both datasets together
cObj <- rbind(cObj21,cObj24)

#run combined PCA
pca_result <- PCA(cObj)

#attach SHAPE program data to original dataframe - reformat mask names
allFilteredDfRow21 <- read_csv("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_21UM/results/all_filtered_combined_rowID.csv")
allFilteredDfRow21$mask_id_shape <- paste0(allFilteredDfRow21$mask_id,"_1")

allFilteredDfRow24 <- read_csv("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_24UM/all_filtered_combined_rowID_24.csv")
allFilteredDfRow24$mask_id_shape <- paste0(allFilteredDfRow24$mask_id,"_12")

#join datasets together
allFilteredDfRow21 <- allFilteredDfRow21[, -2]
allFilteredDfRow21$year <- 21
allFilteredDfRow24$year <- 24
allFilteredDfRow <- rbind(allFilteredDfRow21, allFilteredDfRow24)

pca_scores <- data.frame(pca_result$x[, 1:4])
# Naming the columns for easier reference
names(pca_scores) <- c("PC1", "PC2","PC3", "PC4")
pca_scores$row_labels <- rownames(pca_scores)

allFilteredDfRow$genotype <- as.factor(allFilteredDfRow$genotype)
joined_df <- merge(pca_scores, allFilteredDfRow, by.x = "row_labels", by.y = "mask_id_shape")

#combine same genotypes w/ different spelling
joined_df <- joined_df %>% mutate(genotype = recode(genotype, "b73" = "B73"))
joined_df <- joined_df %>% mutate(genotype = recode(genotype, "gt1-P_test" = "gt1p"))
joined_df <- joined_df %>% mutate(genotype = recode(genotype, "gt1ra3tb1" = "tb1gt1ra3_hybrid"))

#clean plant id label names
joined_df$plant_id <- gsub("/", "", joined_df$plant_id)
joined_df$plant_id <- gsub("_", "", joined_df$plant_id)
joined_df$plant <- paste0(joined_df$genotype, "_" , joined_df$plant_id)

#save csv before filter - contains linked PC data
write.csv(joined_df, file="joined_df_pre_5_pistil_count_filter.csv")

#PRE FILTER CHECK number of pistils counted per individual plant, per genotype
joined_df %>% group_by(genotype, plant_id) %>% summarise(pistil_count = n()) %>% ggplot(aes(x=genotype,y=pistil_count)) + 
  geom_boxplot() + geom_jitter(width=.1,alpha=.5) + 
  ggtitle("number of pistils counted per individual plant, per genotype")

#filter for plants with more than 5 detections, filter bad qual scans
joined_df <- joined_df %>%
  filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1p")) %>%  filter(plant!="gt1p_260-5") %>% filter(plant!="tb1_273-8") %>%  filter(plant!="gt1p_260-1") %>%  
  add_count(genotype, plant_id, name = "pistil_count") %>%
  filter(pistil_count > 5)

#POST FILTER CHECK number of pistils counted per individual plant, per genotype
joined_df %>% group_by(genotype, plant_id) %>% summarise(pistil_count = n()) %>% ggplot(aes(x=genotype,y=pistil_count)) + 
  geom_boxplot() + geom_jitter(width=.1,alpha=.5) + 
  ggtitle("number of pistils counted per individual plant, per genotype")


#PCA PLOTTING

#only keep the cObj with the final genotypes (gt1-, tb1gt1ra3, gt1ra3, post filtering)

joined_df <- joined_df %>% filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1p"))
cObj_filtered <- cObj[rownames(cObj) %in% joined_df$row_labels, ]

#run combined PCA
pca_result <- PCA(cObj_filtered)

pca_scores <- data.frame(pca_result$x[, 1:4])
# Naming the columns for easier reference
names(pca_scores) <- c("PC1", "PC2","PC3", "PC4")
pca_scores$row_labels <- rownames(pca_scores)

joined_df_new <- merge(pca_scores, joined_df, by.x = "row_labels", by.y = "row_labels")

joined_df_new %>% filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "tb1gt1ra3_hybrid", "gt1p")) %>% ggplot(aes(x = PC1.y, y = PC2.y, color=genotype)) +
  geom_point(alpha=.5) +  # adds the scatter plot points
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  ggtitle("Scatter Plot of the First Two Principal Components") + facet_wrap(~year)
#looks the same
explained_variance_subset <- pca_result$eig / sum(pca_result$eig)
explained_variance_subset#[1] pc1 0.0916996455 pc2 0.0717567194

