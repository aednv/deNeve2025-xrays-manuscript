#SHAPE+Momocs Anaysis
library(Momocs) #older version 1.2.2
library(tidyverse)
library(ggbeeswarm)
#install.packages("devtools")
#devtools::install_version("Momocs", version = "1.2.2")
cObj <- nef2Coe("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_21UM/masks/efd/masks_chc.nef")

cObj <- subset(cObj, select = -A1)

pca_result <- PCA(cObj)

allFilteredDfRow <- read_csv("all_filtered_combined_rowID.csv")

allFilteredDfRow$mask_id_shape <- paste0(allFilteredDfRow$mask_id,"_1")

pca_scores <- data.frame(pca_result$x[, 1:4])

# Naming the columns for easier reference
names(pca_scores) <- c("PC1", "PC2","PC3", "PC4")

ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point() +  # adds the scatter plot points
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  ggtitle("Scatter Plot of the First Two Principal Components")

pca_scores$row_labels <- rownames(pca_scores)
joined_df <- merge(pca_scores, allFilteredDfRow, by.x = "row_labels", by.y = "mask_id_shape")


joined_df %>% filter(genotype == "gt1-P_test" | genotype == "gt1ra3tb1" | genotype == "gt1ra3B73") %>% ggplot(aes(x = PC1, y = PC2, color=genotype)) +
  geom_point(alpha=.9) +  # adds the scatter plot points
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  ggtitle("Scatter Plot of the First Two Principal Components")

test <- sample(cObj,10)

coo_plot(test)


joined_df %>% filter(genotype == "gt1-P_test" | genotype == "gt1ra3tb1" | genotype == "gt1ra3B73") %>% ggplot(aes(x = PC1, y = PC2, color=pistil_circularity)) +
  geom_point(alpha=.9) +  # adds the scatter plot points
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  ggtitle("Scatter Plot of the First Two Principal Components") + scale_color_viridis_c()



joined_df%>%  filter(genotype == "gt1ra3B73") %>%  ggplot(aes(x = PC1, y = PC4, color=treatment)) +
  geom_point(alpha=.9) +  # adds the scatter plot points
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  ggtitle("Scatter Plot of the First Two Principal Components") 

anova_result <- aov(PC1 ~ pistil_area, data=joined_df)
summary(anova_result)

joined_df%>%  filter(genotype == "gt1ra3B73") %>% ggplot(aes(x=plant_id,y=pistil_area)) +geom_boxplot() + facet_wrap(~treatment, scales = "free_x")


#updated dataframe with additional metrics

allFilteredDf <- read_csv("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_21UM/finalDfextraShapeMetrics.csv")
allFilteredDf$block <- paste0(allFilteredDf$year, allFilteredDf$planting_date)

allFilteredDf <- allFilteredDf %>% filter(genotype == "gt1p" | genotype == "gt1ra3B73" | genotype == "tb1gt1ra3" )

allFilteredDf %>% filter(treatment=="no_treatment") %>%
  filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1p"))


#calc mean values
mean_pistil_solidity <- allFilteredDf %>% group_by(genotype, plant_id, block, year) %>% summarise(mean_solidity=median(solidity), .groups = 'drop')
mean_pistil_circularity <- allFilteredDf %>% group_by(genotype, plant_id, block, year) %>% summarise(mean_circularity=median(pistil_circularity), .groups = 'drop')
mean_pistil_ecc <- allFilteredDf %>% group_by(genotype, plant_id, block, year) %>% summarise(mean_ecc=median(eccentricity), .groups = 'drop')
mean_pistil_mja <- allFilteredDf %>% group_by(genotype, plant_id, block, year) %>% summarise(mean_mja=median(major_axis_length), .groups = 'drop')
mean_pistil_mna <- allFilteredDf %>% group_by(genotype, plant_id, block, year) %>% summarise(mean_mna=median(minor_axis_length), .groups = 'drop')
mean_pistil_a <- allFilteredDf %>% group_by(genotype, plant_id, block, year) %>% summarise(mean_a=median(pistil_area), .groups = 'drop')
mean_pistil_pc1 <- allFilteredDf %>% group_by(genotype, plant_id, block, year) %>% summarise(mean_pc1=median(PC1), .groups = 'drop')
mean_pistil_pc2 <- allFilteredDf %>% group_by(genotype, plant_id, block, year) %>% summarise(mean_pc2=median(PC2), .groups = 'drop')
dfs <- list(mean_pistil_solidity, mean_pistil_circularity, mean_pistil_ecc, mean_pistil_mja, mean_pistil_mna, mean_pistil_a, mean_pistil_pc1,mean_pistil_pc2)
# Merge all dataframes in the list by the same columns
result <- Reduce(function(x, y) merge(x, y, by = c("genotype", "plant_id", "block", "year"), all = FALSE), dfs)


#looking at block/year effect
allFilteredDf %>% filter(treatment=="no_treatment") %>% filter(solidity>.85) %>% ggplot(aes(x = genotype , y = solidity, color=block)) + 
  geom_boxplot(aes(shape=block),outlier.shape=NA) + geom_quasirandom(size=1, width=.4,alpha=.3) + geom_quasirandom(data = mean_pistil_solidity, aes(x = genotype, y = mean_solidity, size=3, shape=block), width=.4)





library(emmeans)
library(lmerTest)

#CIRCULARITY

#testing year and block effects for circularity

step0 <- lmer(pistil_circularity ~ genotype + (1|plant_id), data = allFilteredDf, REML=FALSE)
step1 <- lmer(pistil_circularity ~ genotype + year + (1|plant_id), data = allFilteredDf, REML=FALSE)  #add year fixed effect
step2 <- lmer(pistil_circularity ~ genotype + year + (1|year:block) + (1|plant_id), data = allFilteredDf, REML=FALSE) #add block random effect nested within years

anova(step0, step1) #year effect is very significant 3.812e-13 ***
anova(step1, step2) #block effect is not significant 0.09697 .

#testing genotype significance on circularity using step1 model - year effect only
nrow(model.frame(step1))
summary(step1) #effect of genotype, year is significant
emmeans_model <- emmeans(step1, ~ genotype, pbkrtest.limit = 5202)
pairs(emmeans_model)
# contrast              estimate      SE  df t.ratio p.value
# gt1p - gt1ra3B73       -0.0854 0.01045 283  -8.176  <.0001
# gt1p - tb1gt1ra3       -0.1330 0.01166 236 -11.405  <.0001
# gt1ra3B73 - tb1gt1ra3  -0.0476 0.00683 117  -6.969  <.0001
# 
# Results are averaged over the levels of: year 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 3 estimates

#ECCENTRICITY

#testing year and block effects for eccentricity

step0 <- lmer(eccentricity ~ genotype + (1|plant_id), data = allFilteredDf, REML=FALSE)
step1 <- lmer(eccentricity ~ genotype + year + (1|plant_id), data = allFilteredDf, REML=FALSE)  #add year fixed effect
step2 <- lmer(eccentricity ~ genotype + year + (1|year:block) + (1|plant_id), data = allFilteredDf, REML=FALSE) #add block random effect nested within years

anova(step0, step1) #year effect is very significant 3.233e-10 ***
anova(step1, step2) #block effect is not significant  0.2075

#testing genotype significance on circularity using step1 model - year effect only
nrow(model.frame(step1))
summary(step1) #effect of genotype, year is significant
emmeans_model <- emmeans(step1, ~ genotype, pbkrtest.limit = 5202)
pairs(emmeans_model)
# contrast              estimate      SE  df t.ratio p.value
# gt1p - gt1ra3B73        0.0557 0.00854 292   6.524  <.0001
# gt1p - tb1gt1ra3        0.0862 0.00951 243   9.065  <.0001
# gt1ra3B73 - tb1gt1ra3   0.0305 0.00549 118   5.550  <.0001
# 
# Results are averaged over the levels of: year 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 3 estimates 

#MAJOR AXIS LENGTH

#testing year and block effects for major axis length

step0 <- lmer(major_axis_length ~ genotype + (1|plant_id), data = allFilteredDf, REML=FALSE)
step1 <- lmer(major_axis_length ~ genotype + year + (1|plant_id), data = allFilteredDf, REML=FALSE)  #add year fixed effect
step2 <- lmer(major_axis_length ~ genotype + year + (1|year:block) + (1|plant_id), data = allFilteredDf, REML=FALSE) #add block random effect nested within years

anova(step0, step1) #year effect is not significant 0.5761
anova(step1, step2) #block effect is not significant 0.2899

#testing genotype significance on circularity using step1 model - year effect only
nrow(model.frame(step1))
summary(step1) #effect of genotype slightly significant
emmeans_model <- emmeans(step1, ~ genotype, pbkrtest.limit = 5202)
pairs(emmeans_model)
# contrast              estimate   SE  df t.ratio p.value
# gt1p - gt1ra3B73         0.676 2.28 242   0.296  0.9528
# gt1p - tb1gt1ra3         5.714 2.61 196   2.186  0.0760 *
# gt1ra3B73 - tb1gt1ra3    5.037 1.69 112   2.984  0.0097 ***
# 
# Results are averaged over the levels of: year 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 3 estimates 

#SOLIDITY

#testing year and block effects for major axis length

step0 <- lmer(solidity ~ genotype + (1|plant_id), data = allFilteredDf, REML=FALSE)
step1 <- lmer(solidity ~ genotype + year + (1|plant_id), data = allFilteredDf, REML=FALSE)  #add year fixed effect
step2 <- lmer(solidity ~ genotype + year + (1|year:block) + (1|plant_id), data = allFilteredDf, REML=FALSE) #add block random effect nested within years

anova(step0, step1) #year effect is significant 3.259e-13 ***
anova(step1, step2) #block effect is not significant

#testing genotype significance on circularity using step1 model - year effect only
nrow(model.frame(step1))
summary(step1) #effect of genotype, year very significant
emmeans_model <- emmeans(step1, ~ genotype, pbkrtest.limit = 5202)
pairs(emmeans_model)
# contrast              estimate      SE  df t.ratio p.value
# gt1p - gt1ra3B73       -0.0117 0.00200 367  -5.821  <.0001
# gt1p - tb1gt1ra3       -0.0196 0.00220 305  -8.898  <.0001
# gt1ra3B73 - tb1gt1ra3  -0.0079 0.00118 132  -6.709  <.0001
# 
# Results are averaged over the levels of: year 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 3 estimates 




step1 <- lmer(solidity ~ genotype + (1|block/plant_id) , data = allFilteredDf, REML=FALSE)
nrow(model.frame(step1))
summary(step1)

emmeans_model <- emmeans(step1, ~ genotype, pbkrtest.limit = 5202)
pairs(emmeans_model)


step1 <- lmer(solidity ~ genotype + block + (1|plant_id) , data = allFilteredDf, REML=FALSE)
nrow(model.frame(step1))
summary(step1)

emmeans_model <- emmeans(step1, ~ genotype, pbkrtest.limit = 5202)
pairs(emmeans_model)

result$year <- as.factor(result$year)

step1 <- lmer(solidity ~ genotype + (1|year/plant_id) , data = allFilteredDf, REML=FALSE)
nrow(model.frame(step1))
summary(step1)

emmeans_model <- emmeans(step1, ~ genotype, pbkrtest.limit = 5202)
pairs(emmeans_model)



#testing year and block effects

step0 <- lmer(solidity ~ genotype + (1|plant_id), data = allFilteredDf, REML=FALSE)

step1 <- lmer(solidity ~ genotype + year + (1|plant_id), data = allFilteredDf, REML=FALSE) 

step2 <- lmer(solidity ~ genotype + block + (1|plant_id), data = allFilteredDf, REML=FALSE) 

anova(step0, step1) #year effect is significant

anova(step1, step2) #block effect is not significant





#CIRCULARITY - looking at year effect (no block effect)
allFilteredDf %>% filter(treatment=="no_treatment") %>% ggplot(aes(x = genotype , y = pistil_circularity, color=genotype)) + 
  geom_boxplot(outlier.shape=NA) + geom_quasirandom(size=1, width=.4, color="black") + geom_quasirandom(data = result, aes(x = genotype, y = mean_circularity, size=3, shape=year), width=.4)

#ECCENTRICTY - looking at year effect (no block effect)
allFilteredDf %>% filter(treatment=="no_treatment") %>% ggplot(aes(x = genotype , y = eccentricity, color=genotype)) + 
  geom_boxplot(outlier.shape=NA) + geom_quasirandom(size=1, width=.4, color="black") + geom_quasirandom(data = result, aes(x = genotype, y = mean_ecc, size=3, shape=year), width=.4)

#MJ AXIS LEN - looking at year effect (no block effect)
allFilteredDf %>% filter(treatment=="no_treatment") %>% ggplot(aes(x = genotype , y = major_axis_length, color=genotype)) + 
  geom_boxplot(outlier.shape=NA) + geom_quasirandom(size=1, width=.4, color="black") + geom_quasirandom(data = result, aes(x = genotype, y = mean_mja, size=3, shape=year), width=.4)


#SOLIDITY - looking at year effect (no block effect)
allFilteredDf %>% filter(treatment=="no_treatment") %>% ggplot(aes(x = genotype , y = solidity, color=genotype)) + 
  geom_boxplot(outlier.shape=NA) + geom_quasirandom(size=1, width=.4, color="black") + geom_quasirandom(data = result, aes(x = genotype, y = mean_solidity, size=3, shape=year), width=.4)

#looking at block/year effect
allFilteredDf %>% filter(treatment=="no_treatment") %>% ggplot(aes(x = genotype , y = pistil_area, color=genotype)) + 
  geom_boxplot(outlier.shape=NA) + geom_quasirandom(size=1, width=.4, color="black") + geom_quasirandom(data = result, aes(x = genotype, y = mean_a, size=3, shape=year), width=.4)



#pc1 v pc2
allFilteredDf %>% # filter(treatment=="no_treatment") %>% 
  filter(year==24) %>%
  filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1p")) %>% 
  ggplot(aes(x = PC1, y = PC2, fill = genotype, color = genotype)) +
  #geom_density_2d(aes(size = ..level..,)) +   # Vary line thickness by contour level
  scale_size_continuous(range = c(0.5, 2)) +  # Set size range for line thickness
  theme_minimal() + 
  geom_point(data = result, aes(x = mean_pc1, y = mean_pc2), size=3, alpha=.5) +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA)
  )

#pc1 v pc2
allFilteredDf %>%  #filter(treatment=="no_treatment") %>%
  filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1p")) %>% 
  ggplot(aes(x = PC1, y = PC2, fill = genotype, color = genotype)) +
  #geom_density_2d(aes(size = ..level..,)) +   # Vary line thickness by contour level
  scale_size_continuous(range = c(0.5, 2)) +  # Set size range for line thickness
  theme_minimal() + 
  geom_point(data = result, aes(x = mean_pc1, y = mean_pc2, shape=block), size=4, alpha=.7) +
  geom_point(size=1, alpha=.2) +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA)
  )



allFilteredDf %>%
  filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1p")) %>% #filter(year == 24) %>%
  ggplot(aes(x = PC1, y = PC2, fill = genotype, color = genotype)) +
  geom_density_2d(aes(size = ..level..,)) +   # Vary line thickness by contour level
  scale_size_continuous(range = c(0.5, 2)) +  # Set size range for line thickness
  theme_minimal() + 
  geom_point(alpha=.2) +
  labs(
    x = "PC1",
    y = "PC2",
    fill = "Genotype",
    color = "Genotype"
  ) +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA)
  )



#can we cluster the genotypes at all by shape metrics? just 24UM comparison
result$block <- as.factor(result$block)
#pistil area vs circularity, just 24UM groups
allFilteredDf %>%  filter(treatment=="no_treatment") %>% filter(year==24) %>%
  filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1p")) %>% 
  ggplot(aes(x = pistil_area, y = pistil_circularity, fill = genotype, color = genotype)) +
  geom_density_2d(aes(size = ..level..,)) +   # Vary line thickness by contour level
  scale_size_continuous(range = c(0.5, 2)) +  # Set size range for line thickness
  theme_minimal() + 
  geom_point(data = result, aes(x = mean_a, y = mean_circularity), size=3, alpha=.5) +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA)
  )
#pistil area vs eccentricity
allFilteredDf %>% filter(treatment=="no_treatment") %>%
  filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1p")) %>% #filter(year == 24) %>%
  ggplot(aes(x = pistil_area, y = eccentricity, fill = genotype, color = genotype)) +
  geom_density_2d(aes(size = ..level..,)) +   # Vary line thickness by contour level
  scale_size_continuous(range = c(0.5, 2)) +  # Set size range for line thickness
  theme_minimal() + 
  geom_point(data = result, aes(x = mean_a, y = mean_ecc, shape=block), size=3, alpha=.5) +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA)
  )
#solidity vs eccentricity
allFilteredDf %>%  filter(treatment=="no_treatment") %>% filter(year==24) %>%
  filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1p")) %>% 
  ggplot(aes(x = solidity, y = eccentricity, fill = genotype, color = genotype)) +
  geom_density_2d(aes(size = ..level..,)) +   # Vary line thickness by contour level
  scale_size_continuous(range = c(0.5, 2)) +  # Set size range for line thickness
  theme_minimal() + 
  #geom_point(alpha=.1) +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA)
  )
#pistil area vs solidity
allFilteredDf %>%  filter(treatment=="no_treatment") %>% filter(year==24) %>%
  filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1p")) %>% 
  ggplot(aes(x = pistil_area, y = solidity, fill = genotype, color = genotype)) +
  geom_density_2d(aes(size = ..level..,)) +   # Vary line thickness by contour level
  scale_size_continuous(range = c(0.5, 2)) +  # Set size range for line thickness
  theme_minimal() + 
  #geom_point(alpha=.1) +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA)
  )
#major axis len vs solidity
allFilteredDf %>%  filter(treatment=="no_treatment") %>% filter(year==24) %>%
  filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1p")) %>% 
  ggplot(aes(x = major_axis_length, y = solidity, fill = genotype, color = genotype)) +
  geom_density_2d(aes(size = ..level..,)) +   # Vary line thickness by contour level
  scale_size_continuous(range = c(0.5, 2)) +  # Set size range for line thickness
  theme_minimal() + 
  #geom_point(alpha=.1) +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA)
  )
