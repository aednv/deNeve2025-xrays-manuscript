#library set up
library(tidyverse)
library(ggbeeswarm)

setwd("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments_and_analysis/maize_24UM/results")

#this combined dataset has already been filtered for possible duplicates due to scan overlap and for segmentations that intersect with the image perimeter
allFilteredDf <- read_csv("all_filtered_combined.csv")

allFilteredDf$genotype <- as.factor(allFilteredDf$genotype)
allFilteredDf$pistil_area <- as.numeric(allFilteredDf$pistil_area)

#################################################################################################################################

# PISTIL SIZE ANALYSIS PART 1 - Relates to figures 2, 3, and 4
# EXPLORING PISTIL SIZE DATA, 24UM

#just considering non-treated plants
allFilteredNoTreatmentDf <- allFilteredDf %>% filter(treatment == "no_treatment")

#clean plant id label names
allFilteredDf$plant_id <- gsub("/", "", allFilteredDf$plant_id)
allFilteredDf$plant_id <- gsub("_", "", allFilteredDf$plant_id)

#data distribution
ggplot(allFilteredNoTreatmentDf, aes(pistil_area)) + geom_histogram() + 
  facet_wrap(~genotype) + ggtitle("Pistil size distribution per genotype")

#number of individual plants per genotype
allFilteredDf %>% group_by(genotype, treatment) %>% summarise(plant_count = n_distinct(plant_id))

#number of pistils counted per individual plant, per genotype
allFilteredNoTreatmentDf %>% group_by(genotype, plant_id) %>% summarise(pistil_count = n()) %>% ggplot(aes(x=genotype,y=pistil_count)) + 
  geom_boxplot() + geom_jitter(width=.1,alpha=.5) + 
  ggtitle("number of pistils counted per individual plant, per genotype")

#boxplot of pistil sizes per genotype
allFilteredNoTreatmentDf %>% ggplot(aes(x=genotype,y=pistil_area)) + 
  geom_boxplot(aes(color=genotype), outlier.shape = NA, size=1.2) + 
  geom_quasirandom(size=.3,alpha=.6) + ggtitle("pitsil size by genotype")

#boxplot of pistil sizes per individual plant, per genotype
#1 line
allFilteredNoTreatmentDf$plant <- paste0(allFilteredNoTreatmentDf$genotype, "_" ,allFilteredNoTreatmentDf$plant_id)
allFilteredNoTreatmentDf %>% arrange(genotype) %>% ggplot(aes(x=plant,y=pistil_area)) + 
  geom_boxplot(aes(color=genotype), outlier.shape = NA, size=.8) + 
  geom_quasirandom(size=.3,alpha=.6) + ggtitle("pitsil size by genotype, by individual plant") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("pistil_size_by_genotpye_by_individual_high_res.png", width = 8, height = 6, dpi = 500)

#with facet wrap
allFilteredNoTreatmentDf %>% ggplot(aes(x=plant_id,y=pistil_area)) + 
  geom_boxplot(aes(color=genotype), outlier.shape = NA) + 
  ggtitle("pitsil size by genotype, by individual plant") + 
  facet_wrap(~genotype, scales="free_x") + geom_quasirandom(size=.1,alpha=.3)+ theme(axis.text.x = element_text(angle = 45, hjust = 1, size=6))

#filter plants with less than 5 detections:
FilteredDf <- allFilteredNoTreatmentDf %>% filter(genotype == "gt1ra3B73" | genotype == "tb1gt1ra3" | genotype == "gt1p" | genotype == "tb1gt1ra3_het" | genotype == "tb1" | genotype == "b73") %>%
#FilteredDf <- FilteredDf %>% group_by(genotype, plant_id) %>% summarise(pistil_count = n()) %>% filter(pistil_count > 5)

FilteredDf %>% arrange(genotype) %>% ggplot(aes(x=plant,y=pistil_area)) + 
  geom_boxplot(aes(color=genotype), outlier.shape = NA, size=.8) + 
  geom_quasirandom(size=.3,alpha=.6) + ggtitle("pitsil size by genotype, by individual plant, individuals with > than 5 detections") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("pistil_size_by_genotpye_by_individual_high_res.png", width = 8, height = 6, dpi = 500)

FilteredDf <- FilteredDf %>%
  filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1p", "tb1gt1ra3_het", "tb1", "b73")) %>%
  add_count(genotype, plant_id, name = "pistil_count") %>%
  filter(pistil_count >5)

#boxplot of pistil sizes per genotype
FilteredDf %>% ggplot(aes(x=genotype,y=pistil_area)) + 
  geom_boxplot(aes(color=genotype), outlier.shape = NA, size=1.2) + 
  geom_quasirandom(size=.3,alpha=.6, outlier.shape = NA) + ggtitle("pitsil size by genotype, > 5 detections")

allFilteredDf$pistil_area <- as.numeric(allFilteredDf$pistil_area)
allFilteredDf %>% ggplot(aes(x=treatment,y=pistil_area)) + geom_boxplot()

#superplot for pistil area by genotype
mean_pistil_area <- FilteredDf %>%
  group_by(genotype, plant_id) %>%
  summarise(mean_pistil_area = mean(pistil_area, na.rm = TRUE), .groups = 'drop')

ggplot(FilteredDf, aes(x = genotype, y = pistil_area, color=genotype)) +
  geom_boxplot(outlier.shape = NA) +         # Boxplot without outliers
  geom_quasirandom(width = 0.2, alpha = 0.4, size=.3) +  # Scatter plot for individual pistil area points
  geom_point(data = mean_pistil_area, aes(x = genotype, y = mean_pistil_area, color=genotype), 
              size = 3, shape = 16) +  # Overlay mean points per plant
  labs(y = "Pistil Area", x = "Genotype") +
  theme_minimal()

#########################################################################################################################################################################3

# PISTIL SIZE ANALYSIS PART 2 - Relates to figures 2, 3, and 4
# COMBINING 21UM and 24UM DATA + STATISTICS

setwd("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments_and_analysis/maize_21UM/results")
#this combined dataset has already been filtered for possible duplicates due to scan overlap and for segmentations that intersect with the image perimeter
allFilteredDf <- read_csv("all_filtered_combined.csv")

allFilteredDf$genotype <- as.factor(allFilteredDf$genotype)
allFilteredDf$pistil_area <- as.numeric(allFilteredDf$pistil_area)

#1. ~~ Pistil size comparison between genotypes ~~

#just considering non-treated plants
allFilteredNoTreatmentDf <- allFilteredDf %>% filter(treatment == "no_treatment")

#clean plant id label names
allFilteredDf$plant_id <- gsub("/", "", allFilteredDf$plant_id)
allFilteredDf$plant_id <- gsub("_", "", allFilteredDf$plant_id)

allFilteredNoTreatmentDf$plant <- paste0(allFilteredNoTreatmentDf$genotype, "_" ,allFilteredNoTreatmentDf$plant_id)

#filter plants with less than 5 detections:
FilteredDf21 <- allFilteredNoTreatmentDf %>% filter(genotype == "gt1ra3B73" | genotype == "tb1gt1ra3" | genotype == "gt1-P_test" | genotype == "tb1gt1ra3_het" | genotype == "tb1" | genotype == "b73") %>% filter(!plant_id=="gt1p_260-5")
#FilteredDf <- FilteredDf %>% group_by(genotype, plant_id) %>% summarise(pistil_count = n()) %>% filter(pistil_count > 5)

FilteredDf21 <- FilteredDf21 %>%
  filter(genotype %in% c("gt1ra3B73", "tb1gt1ra3", "gt1-P_test", "tb1gt1ra3_het", "tb1", "b73")) %>%  filter(plant!="gt1p_260-5") %>% filter(plant!="tb1_273-8") %>%  filter(plant!="gt1p_260-1") %>%  
  add_count(genotype, plant_id, name = "pistil_count") %>%
  filter(pistil_count >5)

FilteredDf21$genotype <- gsub("gt1-P_test", "gt1p", FilteredDf21$genotype)

FilteredDf$plant <- paste0(allFilteredNoTreatmentDf$genotype, "_" ,allFilteredNoTreatmentDf$plant_id)

#combining 21UM data and 24UM data
FilteredDf21$year <- 21
FilteredDf21 <- FilteredDf21 %>% select(-`Unnamed: 0`)
FilteredDf$year <- 24
FilteredDf <- FilteredDf %>% select(-`Unnamed: 0`)
finalCombined <- rbind(FilteredDf21, FilteredDf)

write.csv(finalCombined, "combined.csv", row.names=FALSE)
finalCombined_sorted <- finalCombined[order(finalCombined$plant_id, finalCombined$y_pos_all_overlapped_len_standardized), ]

write.csv(finalCombined_sorted, "combined_sorted.csv", row.names=FALSE)

finalCombined <- read_csv("combined.csv")

#number of pistils counted per individual plant, per genotype
finalCombined %>% group_by(genotype, plant_id) %>% summarise(pistil_count = n()) %>% ggplot(aes(x=genotype,y=pistil_count)) + 
  geom_boxplot(aes(color=genotype)) + geom_jitter(width=.1,alpha=.5) + 
  ggtitle("number of pistils counted per individual plant, per genotype")

#superplot
mean_pistil_area <- allDf %>%
  group_by(genotype, plant_id,year) %>%
  summarise(mean_pistil_area = mean(pistil_area, na.rm = TRUE), .groups = 'drop')

mean_pistil_area$year <- as.factor(mean_pistil_area$year)
ggplot(allDf, aes(x = genotype, y = pistil_area, color=genotype)) +
  geom_boxplot(outlier.shape = NA) +         # Boxplot without outliers
  geom_quasirandom(width = 0.2, alpha = 0.4, size=.3) +  # Scatter plot for individual pistil area points
  geom_point(data = mean_pistil_area, aes(x = genotype, y = mean_pistil_area, shape=year), 
             size = 3) +  # Overlay mean points per plant
  labs(y = "Pistil Area", x = "Genotype") +
  theme_minimal()

#statistics

# ~~ does pistil size differ between genotypes? ~~

finalCombined$block <- paste0(finalCombined$planting_date,finalCombined$year)

#model1 - all filtered no treatment, filter out less than 5 detections, factor in the year and different blocks
step1 <- lmer(pistil_area ~ genotype + block + (1|plant_id), data = finalCombined, REML=FALSE)
nrow(model.frame(step1))
summary(step1)
emmeans_model <- emmeans(step1, ~ genotype, lmerTest.limit = 4180, pbkrtest.limit = 4180)
pairs(emmeans_model)
#result - all genotypes have significantly different size

#model2 - both treated and untreated, using treatment as fixed effect, gt1 ra3 only
gt1ra3Df <- allFilteredDf %>% filter(genotype == "gt1ra3B73")
model2 <- lmer(pistil_area ~ treatment + (1|plant_id), data = gt1ra3Df, REML=FALSE)
nrow(model.frame(model2))
summary(model2)
emmeans_model <- emmeans(model2, ~ treatment)
pairs(emmeans_model)
#treatment has no effect on pistil size

#model3 - both treated and untreated, using treatment and planting date as fixed effects
gt1ra3Df <- allFilteredDf %>% filter(genotype == "gt1ra3B73")
model3 <- lmer(pistil_area ~ treatment + planting_date + (1|plant_id), data = gt1ra3Df, REML=FALSE)
nrow(model.frame(model3))
summary(model3)
emmeans_model <- emmeans(model3, ~ treatment)
pairs(emmeans_model)
#result:treatment has no effect on pistil size

#model4 - both treated and untreated, checking effect of planting_date w/o treatment
gt1ra3Df <- allFilteredDf %>% filter(genotype == "gt1ra3B73")
model4 <- lmer(pistil_area ~ planting_date + (1|plant_id), data = gt1ra3Df, REML=FALSE)
nrow(model.frame(model4))
summary(model4)
emmeans_model <- emmeans(model4, ~ planting_date, lmerTest.limit = 4180)
pairs(emmeans_model)
#result:early planting is significantly different than late planting



#######################################################################################################

# DEFOLIATION ANALYSIS - Relates to Fig. 3

#libraries
library(ggplot2)
library(dplyr)
library(ggbeeswarm)

def_df <- all21FilteredDf %>%
    filter(genotype == "gt1ra3B73")

mean_pistil_area <- def_df %>% 
group_by( treatment,plant_id) %>%
  summarise(mean_pistil_area = mean(pistil_area, na.rm = TRUE), .groups = 'drop')
  
ggplot(def_df, aes(x = treatment, y = pistil_area, color=genotype)) +
  geom_boxplot(outlier.shape = NA) +         # Boxplot without outliers
  geom_quasirandom(width = 0.2, alpha = 0.4, size=.3) +  # Scatter plot for individual pistil area points
  geom_point(data = mean_pistil_area, aes(x = genotype, y = mean_pistil_area), 
             size = 3, shape = 16) +  # Overlay mean points per plant
  labs(y = "Pistil Area", x = "Genotype") +
  theme_minimal()

# Filter for specific genotype
def_df <- all21FilteredDf %>%
  filter(genotype == "gt1ra3B73")

# Calculate mean pistil area per treatment and plant_id
mean_pistil_area <- def_df %>% 
  group_by(treatment, plant_id) %>%
  summarise(mean_pistil_area = mean(pistil_area, na.rm = TRUE), .groups = 'drop')

# Plot
ggplot(def_df, aes(x = treatment, y = pistil_area, color = genotype)) +
  geom_boxplot(outlier.shape = NA) +         # Boxplot without outliers
  geom_quasirandom(width = 0.2, alpha = 0.4, size = 0.3) +  # Scatter plot for individual pistil area points
  geom_point(data = mean_pistil_area, aes(x = treatment, y = mean_pistil_area), 
             size = 3, shape = 16) +  # Overlay mean points per treatment
  labs(y = "Pistil Area", x = "Treatment") +
  theme_minimal()

ggplot(def_df, aes(x = treatment, y = pistil_area, color=treatment)) +
  geom_boxplot(outlier.shape = NA, aes(color = treatment)) +  # Set color in this layer
  geom_quasirandom(aes(color = treatment), width = 0.2, alpha = 0.4, size = 0.3) +  # Set color in this layer
  geom_point(data = mean_pistil_area, aes(x = treatment, y = mean_pistil_area), 
             size = 3) +  # No color specified here
  labs(y = "Pistil Area", x = "Treatment") +
  theme_minimal()

# ~~ MLM

library(lme4)
library(emmeans)

# ~~ does pistil size differ between genotypes? ~~

#model2 - both treated and untreated, using treatment as fixed effect, gt1 ra3 only
gt1ra3Df <- allFilteredDf %>% filter(genotype == "gt1ra3B73")
model2 <- lmer(pistil_area ~ treatment + (1|plant_id), data = gt1ra3Df, REML=FALSE)
nrow(model.frame(model2))
summary(model2)
emmeans_model <- emmeans(model2, ~ treatment)
pairs(emmeans_model)
#treatment has no effect on pistil size

#model3 - both treated and untreated, using treatment and planting date as fixed effects
gt1ra3Df <- allFilteredDf %>% filter(genotype == "gt1ra3B73")
model3 <- lmer(pistil_area ~ treatment + planting_date + (1|plant_id), data = gt1ra3Df, REML=FALSE)
nrow(model.frame(model3))
summary(model3)
emmeans_model <- emmeans(model3, ~ treatment)
pairs(emmeans_model)
#result:treatment has no effect on pistil size

#model4 - both treated and untreated, checking effect of planting_date w/o treatment
gt1ra3Df <- allFilteredDf %>% filter(genotype == "gt1ra3B73")
model4 <- lmer(pistil_area ~ planting_date + (1|plant_id), data = gt1ra3Df, REML=FALSE)
nrow(model.frame(model4))
summary(model4)
emmeans_model <- emmeans(model4, ~ planting_date)
pairs(emmeans_model)
#result:early planting is significantly different than late planting

# DEFOLIATION PART 2
# DEFOLIATION 21UM and 24UM COMBINED


#superplot
mean_pistil_area <- allDf %>% filter(genotype=="gt1ra3B73") %>%
  group_by(year,genotype, plant_id, treatment) %>%
  summarise(mean_pistil_area = mean(pistil_area, na.rm = TRUE), .groups = 'drop')

mean_pistil_area$year <- as.factor(mean_pistil_area$year)
allDf %>% filter(genotype=="gt1ra3B73") %>% ggplot(aes(x = treatment, y = pistil_area, color=treatment)) +
  geom_boxplot(outlier.shape = NA) +         # Boxplot without outliers
  geom_quasirandom(width = 0.2, alpha = 0.4, size=.3) +  # Scatter plot for individual pistil area points
  geom_point(data = mean_pistil_area, aes(x = treatment, y = mean_pistil_area, shape=year), 
             size = 3) +  # Overlay mean points per plant
  labs(y = "Pistil Area", x = "Genotype") +
  theme_minimal()


#defoliation stats

data21 <- allDf %>% filter(genotype=="gt1ra3B73") %>% filter(year=="21") 
data24 <- allDf %>% filter(genotype=="gt1ra3B73") %>% filter(year=="24") 


step1 <- lmer(pistil_area ~ treatment + planting_date + (1|plant_id), data = data21, REML=FALSE)
nrow(model.frame(step1))
summary(step1)
emmeans_model <- emmeans(step1, ~ treatment)
pairs(emmeans_model)

step1 <- lmer(pistil_area ~ treatment + planting_date + (1|plant_id), data = data24, REML=FALSE)
nrow(model.frame(step1))
summary(step1)
emmeans_model <- emmeans(step1, ~ treatment)
pairs(emmeans_model)

#####################################################################################################

# POSITION ANALYSIS - relates to fig. 2

# ~~ how does gt1-P pistil size differ along length of tassel? ~~

gt1PDf <- finalCombined %>% filter(genotype == "gt1p")
gt1PDfx <- gt1PDf %>% filter(plant_id %in% sample(levels(as.factor(gt1PDf$plant_id)), 5))
gt1PDfx$y_pos_all_overlapped_len_standardized <- as.numeric(gt1PDfx$y_pos_all_overlapped_len_standardized)

ggplot(gt1PDfx, aes(x=y_pos_all_overlapped_len_standardized, color=plant_id,y=pistil_area,fill=plant_id,group=plant_id)) + geom_point() + geom_smooth(method="lm", se=FALSE)

ggplot(gt1PDfx, aes(x=y_pos_all_overlapped_len_standardized,color=plant_id, fill=plant_id)) + geom_histogram()   +  xlim(0, 1) + ylim(0,30)

gt1ra3Df <- finalCombined %>% filter(year == "24") %>% filter(genotype == "gt1ra3B73")
gt1ra3Dfx <- gt1ra3Df %>% filter(plant_id %in% sample(levels(as.factor(gt1ra3Df$plant_id)), 5))
gt1ra3Dfx$y_pos_all_overlapped_len_standardized <- as.numeric(gt1ra3Dfx$y_pos_all_overlapped_len_standardized)
ggplot(gt1ra3Dfx, aes(x=y_pos_all_overlapped_len_standardized, color=plant_id, fill=plant_id)) + geom_histogram()   +  xlim(0, 1) + ylim(0,30) + scale_color_manual(values = hcl.colors(5, "Purple-Orange"))

#gt1ra3
ggplot(gt1ra3Dfx, aes(x = y_pos_all_overlapped_len_standardized, fill = plant_id)) +
  geom_histogram() +  # or position="dodge" if needed
  xlim(0, 1) +
  ylim(0, 30) +
  scale_fill_manual(values = hcl.colors(5, "Teal")) + theme_minimal()

#gt1
ggplot(gt1PDfx, aes(x = y_pos_all_overlapped_len_standardized, fill = plant_id)) +
  geom_histogram() +  # or position="dodge" if needed
  xlim(0, 1) +
  ylim(0, 30) +
  scale_fill_manual(values = hcl.colors(5, "Purp")) + theme_minimal()

ggplot(gt1ra3Dfx, aes(x=y_pos_all_overlapped_len_standardized, color=plant_id,y=pistil_area,fill=plant_id,group=plant_id)) + geom_point() + geom_smooth(method="lm", se=FALSE) +ylim(0,3000)
gt1PDf$y_pos_all_overlapped_len_standardized <- as.numeric(gt1PDf$y_pos_all_overlapped_len_standardized)
ggplot(gt1PDf, aes(x=y_pos_all_overlapped_len_standardized,color=plant_id, fill=plant_id)) + geom_histogram()  +  xlim(0, 1)

gt1ra3Df$y_pos_all_overlapped_len_standardized <- as.numeric(gt1ra3Df$y_pos_all_overlapped_len_standardized)
ggplot(gt1ra3Dfx, aes(x=y_pos_all_overlapped_len_standardized,fill=plant_id,group=plant_id)) + geom_histogram()
gt1ra3Dfx <- gt1ra3Df %>% filter(plant_id == sample(levels(as.factor(gt1ra3Df$plant_id)), 5))
gt1ra3Df <- gt1ra3Df %>% filter(plant_id == sample(levels(as.factor(gt1ra3Df$plant_id)), 5))


# ~~ does pistil size differ along length of tassel? ~~

ggplot(FilteredDf, aes(x=y_pos_all_overlapped_len_standardized, y=pistil_area,color=plant_id)) + geom_point() + geom_smooth(method="lm", se=FALSE)

model5 <- lmer(pistil_area ~ y_pos_all_overlapped_len_standardized + (1|plant_id), data = FilteredDf, REML=FALSE)
nrow(model.frame(model5))
summary(model5)

model6 <- lmer(pistil_area ~ y_pos_all_overlapped_len_standardized + (y_pos_all_overlapped_len_standardized|plant_id), data = FilteredDf, REML=FALSE)
nrow(model.frame(model6))
summary(model6)

#compare the bottom 50 percent with the top 50 percent

FilteredDf$top_half = FilteredDf$y_pos_all_overlapped_len_standardized > .5
ggplot(FilteredDf, aes(x=top_half, y=pistil_area,fill=top_half)) + geom_boxplot()

# summarize mean and CI
summary_df <- FilteredDf %>%
  group_by(top_half) %>%
  summarise(
    mean = mean(pistil_area, na.rm = TRUE),
    se = sd(pistil_area, na.rm = TRUE)/sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    ci_lower = mean - qt(0.975, df = n-1) * se,
    ci_upper = mean + qt(0.975, df = n-1) * se
  )

results<-list()
for (plant in unique(FilteredDf$plant_id)) {
  plant_data <- FilteredDf[FilteredDf$plant_id == plant,]
  test_result <- t.test(pistil_area ~ top_half, data = plant_data, var.equal=FALSE)
  results[[plant]] <- test_result
}

FilteredDf %>% filter(y_pos_all_overlapped_len_standardized <.4) %>% ggplot(aes(x=y_pos_all_overlapped_len_standardized, y=pistil_area,color=plant_id)) + geom_point() + geom_smooth(method="lm", se=FALSE)
halfTest <- FilteredDf %>% filter(y_pos_all_overlapped_len_standardized <.4)

model7 <- lmer(pistil_area ~ y_pos_all_overlapped_len_standardized + (1|plant_id), data=halfTest,REML=FALSE)
nrow(model.frame(model7))
summary(model7)

#checking with other package that provides p values
library(nlme)
model_nlme <- lme(pistil_area ~ y_pos_all_overlapped_len_standardized, random = ~ 1 | plant_id, data = halfTest)
summary(model_nlme)

model <- lme(pistil_area ~ y_pos_all_overlapped_len_standardized, 
             random = ~ y_pos_all_overlapped_len_standardized | plant_id, 
             data = halfTest)

summary(model)


# 10/28/24

#boxplot of pistil circularity per genotype
FilteredDf$pistil_circularity <- as.numeric(FilteredDf$pistil_circularity)
FilteredDf %>% ggplot(aes(x=genotype,y=pistil_circularity)) + 
  geom_boxplot(aes(color=genotype), outlier.shape = NA, size=1.2) + 
  geom_quasirandom(size=.3,alpha=.6, outlier.shape = NA) + ggtitle("pitsil circ by genotype, > 5 detections")

library(lme4)
library(emmeans)
library(lmerTest)
library(pbkrtest)
# ~~ does pistil circularity differ between genotypes? ~~

#model1 - all filtered no treatment, filter out less than 5 detections
step1 <- lmer(pistil_circularity ~ genotype + (1|plant_id), data = FilteredDf, REML=FALSE)
nrow(model.frame(step1))
summary(step1)
emmeans_model <- emmeans(step1, ~ genotype)
pairs(emmeans_model)

##circularity is different between tb1gt1ra3 and gt1ra3 -- remember to check between defoliation treatments too
#check some examples of masks between the 2 genotypes to see if anything is visually different

#circularity superplot

mean_pistil_c <- FilteredDf %>% 
  group_by(genotype, plant_id) %>%
  summarise(mean_pistil_c = mean(pistil_circularity, na.rm = TRUE), .groups = 'drop')

ggplot(FilteredDf, aes(x = genotype, y = pistil_circularity, color=genotype)) +
  geom_boxplot(outlier.shape = NA) +         # Boxplot without outliers
  geom_quasirandom(width = 0.2, alpha = 0.4, size=.3) +  # Scatter plot for individual pistil area points
  geom_point(data = mean_pistil_c, aes(x = genotype, y = mean_pistil_c), 
             size = 3, shape = 16) +  # Overlay mean points per plant
  labs(y = "Pistil C", x = "Genotype") +
  theme_minimal()

ggplot(FilteredDf, aes(x=pistil_area, y=pistil_circularity, color=genotype)) + geom_point() + geom_smooth()


##################################################################################################################################

# PISTIL PAIR ANALYSIS - relates to fig. 2


setwd("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments_and_analysis/maize_24UM/results")
pistilDf <- read_csv("gt1ra3withDoublePistilFinal.csv")

#superplot

mean_pistil_c <- pistilDf %>% 
  group_by(genotype, plant_id) %>%
  summarise(mean_pistil_c = mean(pair_size_difference, na.rm = TRUE), .groups = 'drop')

pistilDf %>% filter(floret_pairs!='NA') %>% ggplot(aes(x = plant_id, y = pistil_area, color=floret_pairs)) +
  geom_boxplot(outlier.shape = NA) +         # Boxplot without outliers
  geom_quasirandom(width = 0.2, alpha = 0.4, size=.3)

pistilDf %>% filter(floret_pairs!='NA') %>% ggplot( aes(x = floret_pairs, y = pistil_area, group = pair_id, color=floret_pairs)) +
  geom_point() + geom_boxplot() +
  geom_line(color="black",alpha=.1) +
  theme_minimal() + facet_wrap(~year)

library(dplyr)

# Summarize mean and 95% CI by group
mean_ci <- pistilDf %>%
  filter(floret_pairs != "NA") %>%
  group_by(floret_pairs) %>%
  summarise(
    mean = mean(pistil_area, na.rm = TRUE),
    n = n(),
    se = sd(pistil_area, na.rm = TRUE) / sqrt(n),
    ci_lower = mean - qt(0.975, df = n - 1) * se,
    ci_upper = mean + qt(0.975, df = n - 1) * se
  )

# Summarize mean and 95% CI by year
mean_ci <- pistilDf %>%
  filter(floret_pairs != "NA") %>%
  group_by(year) %>%
  summarise(
    mean = mean(pistil_area, na.rm = TRUE),
    n = n(),
    se = sd(pistil_area, na.rm = TRUE) / sqrt(n),
    ci_lower = mean - qt(0.975, df = n - 1) * se,
    ci_upper = mean + qt(0.975, df = n - 1) * se
  )

pistilDf %>% filter(floret_pairs!='NA') %>% ggplot(aes(x = genotype, y = pistil_circularity, color=floret_pairs)) +
  geom_boxplot(outlier.shape = NA) +         # Boxplot without outliers
  geom_quasirandom(width = 0.2, alpha = 0.4, size=.3)

pistilDf %>% filter(floret_pairs!='NA') %>% ggplot(aes(x = genotype, y = PC1, color=floret_pairs)) +
  geom_boxplot(outlier.shape = NA) +         # Boxplot without outliers
  geom_quasirandom(width = 0.2, alpha = 0.4, size=.3, position='dodge')

pistilDf %>%
  filter(!is.na(floret_pairs)) %>%
  ggplot(aes(x = genotype, y = PC1, color = floret_pairs)) +
  geom_boxplot(outlier.shape = NA) +         # Boxplot without outliers
  geom_quasirandom(width = 0.2, alpha = 0.4, size = 0.3, dodge.width = 0.75) +
  
mean_pistil_pc1 <- pistilDf %>% 
  group_by(genotype, plant_id, floret_pairs) %>%
  summarise(mean_pistil_c = mean(PC1, na.rm = TRUE), .groups = 'drop')

pistilDf %>%
  filter(!is.na(floret_pairs)) %>%
  ggplot(aes(x = genotype, y = PC1, color = floret_pairs)) +
  geom_boxplot(outlier.shape = NA) +         # Boxplot without outliers
  geom_quasirandom(width = 0.2, alpha = 0.4, size = 0.3, dodge.width = 0.75) +
  geom_point(data = mean_pistil_pc1, position_dodge(width=.75)) 

# Calculate mean PC1 by genotype, plant_id, and floret_pairs
mean_pistil_pc1 <- pistilDf %>% filter(floret_pairs!='NA') %>%
  group_by(genotype, plant_id, floret_pairs) %>%
  summarise(mean_pistil_c = mean(PC1, na.rm = TRUE), .groups = 'drop')

# Plotting
pistilDf %>%
  filter(floret_pairs!='NA') %>%
  ggplot(aes(x = floret_pairs, y = PC1, color = floret_pairs)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) + # Boxplot with dodge
  geom_quasirandom(width = 0.2, alpha = 0.4, size = 0.3, position = position_dodge(width = 0.75)) +
  geom_point(data = mean_pistil_pc1, 
             aes(x = floret_pairs, y = mean_pistil_c, color = plant_id), 
             size = 3, shape = 16, position = position_dodge(width = 0.05))

# Calculate mean PC1 by genotype, plant_id, and floret_pairs
mean_pistil_pc1 <- pistilDf %>% filter(floret_pairs!='NA') %>%
  group_by(genotype, plant_id, floret_pairs) %>%
  summarise(mean_pistil_c = mean(pistil_circularity, na.rm = TRUE), .groups = 'drop')

pistilDf %>%
  filter(floret_pairs!='NA') %>%
  ggplot(aes(x = floret_pairs, y = pistil_circularity, color = floret_pairs)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) + # Boxplot with dodge
  geom_quasirandom(width = 0.2, alpha = 0.4, size = 0.3, position = position_dodge(width = 0.75)) +
  geom_point(data = mean_pistil_pc1, 
             aes(x = floret_pairs, y = mean_pistil_c, color = plant_id), 
             size = 3, shape = 16, position = position_dodge(width = 0.05))

# Ensure `pair_id` is included in `mean_pistil_pc1`
mean_pistil_pc1 <- pistilDf %>% filter(floret_pairs!='NA') %>% 
  group_by(genotype, plant_id, floret_pairs, pair_id) %>%  # Include `pair_id`
  summarise(mean_pistil_c = mean(PC1, na.rm = TRUE), .groups = 'drop')

# Plotting
pistilDf %>%
   filter(floret_pairs!='NA')  %>%
  ggplot(aes(x = floret_pairs, y = PC1, color = floret_pairs)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) + # Boxplot with dodge
  geom_quasirandom(width = 0.2, alpha = 0.4, size = 0.3, groupOnX = TRUE) + # Quasirandom without `position_dodge`
  geom_point(data = mean_pistil_pc1, 
             aes(x = floret_pairs, y = mean_pistil_c, color = plant_id), 
             size = 3, shape = 16, position = position_dodge(width = 0.75)) + 
  geom_line(data = mean_pistil_pc1, 
            aes(x = floret_pairs, y = mean_pistil_c, group = pair_id), color = "black")



#linear mixed model accounting for plant groups and pairs

library(lme4)
library(emmeans)
library(lmerTest)

# ~~ does pistil circularity differ between genotypes? ~~

filteredPistilDf <- pistilDf %>% filter(floret_pairs!='NA')

filteredPistilDf21 <- pistilDf %>% filter(floret_pairs!='NA') %>% filter(year==21)

filteredPistilDf24 <- pistilDf %>% filter(floret_pairs!='NA') %>% filter(year==24)

step1 <- lmer(pistil_area ~ floret_pairs + (1|plant_id) + (1 | pair_id), data = filteredPistilDf, REML=FALSE)
nrow(model.frame(step1))
summary(step1)
emmeans_model <- emmeans(step1, ~ floret_pairs)
pairs(emmeans_model)

step1 <- lmer(pistil_area ~ floret_pairs + (1|plant_id) + (1 | pair_id), data = filteredPistilDf21, REML=FALSE)
nrow(model.frame(step1))
summary(step1)
emmeans_model <- emmeans(step1, ~ floret_pairs)
pairs(emmeans_model)

step1 <- lmer(pistil_area ~ floret_pairs + (1|plant_id) + (1 | pair_id), data = filteredPistilDf24, REML=FALSE)
nrow(model.frame(step1))
summary(step1)
emmeans_model <- emmeans(step1, ~ floret_pairs)
pairs(emmeans_model)

step1 <- lmer(PC1 ~ floret_pairs + (1|plant_id) + (1 | pair_id), data = filteredPistilDf, REML=FALSE)
nrow(model.frame(step1))
summary(step1)
emmeans_model <- emmeans(step1, ~ floret_pairs)
pairs(emmeans_model)

pistilDf %>%
  filter(floret_pairs != 'NA') %>%
  ggplot(aes(x = floret_pairs, y = pistil_area, color=floret_pairs)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
    # Boxplot groups by floret_pairs
  geom_line(aes(group = pair_id), color = "black", alpha = 0.05) +  # Lines use pair_id for grouping
  theme_minimal() +
  facet_wrap(~year)

#####################################################################################################################################

# POSITION ANALYSIS - relates to Fig. 2
# pistil size by position on tassel branch

allDf <- read_csv('joined_df_pre_5_pistil_count_filter.csv')
allDf %>%
  filter(genotype == "gt1ra3B73") %>% filter(treatment=="no_treatment") %>% ggplot(aes(x=y_pos_all_overlapped_len_standardized, y=PC1,color=plant_id)) + geom_point(alpha=.00, outlier.shape=NA) + geom_smooth(method="lm", se=FALSE) +facet_wrap(~year)

allDf$top_half = allDf$y_pos_all_overlapped_len_standardized > .5

allDf %>%
  filter(genotype == "gt1ra3B73") %>% filter(treatment=="no_treatment") %>% ggplot( aes(x=genotype, y=pistil_area,fill=top_half)) + geom_boxplot()

allDf %>%
  filter(genotype == "gt1ra3B73") %>% filter(treatment=="no_treatment") %>% ggplot( aes(x=genotype, y=pistil_circularity,fill=top_half)) + geom_boxplot()
allDf %>%
  filter(genotype == "gt1ra3B73") %>% filter(treatment=="no_treatment") %>% ggplot( aes(x=genotype, y=PC1,fill=top_half)) + geom_boxplot()

mean_pistil_c <- allDf %>% filter(genotype == "gt1ra3B73") %>% filter(treatment=="no_treatment") %>% group_by(plant_id, top_half) %>% summarise(mean_area=mean(pistil_area), .groups = 'drop')

allDf %>% filter(genotype == "gt1ra3B73") %>% filter(treatment=="no_treatment") %>% ggplot( aes(x = top_half, y = pistil_area, color=top_half)) + 
  geom_boxplot(outlier.shape=NA, size=1.2) + geom_quasirandom(alpha=.1, color="black") + 
  geom_point(data = mean_pistil_c, aes(x = top_half, y = mean_area, size=3, alpha=.5, color=plant_id)) +
  geom_line(data=mean_pistil_c, aes(x=top_half, y=mean_area, group = plant_id), color = "black", alpha = 0.2) 

 # theme_minimal() + facet_wrap(~year)

filteredPistilDf <- allDf %>% filter(genotype == "gt1ra3B73") %>% filter(treatment=="no_treatment")
step1 <- lmer(pistil_area ~ top_half + (1|plant_id) , data = filteredPistilDf, REML=FALSE)
nrow(model.frame(step1))
summary(step1)
emmeans_model <- emmeans(step1, ~ top_half)
pairs(emmeans_model)

filteredPistilDf <- allDf %>% filter(genotype == "gt1ra3B73") %>% filter(treatment=="no_treatment")
step1 <- lmer(pistil_circularity ~ top_half + (1|plant_id) , data = filteredPistilDf, REML=FALSE)
nrow(model.frame(step1))
summary(step1)
emmeans_model <- emmeans(step1, ~ top_half)
pairs(emmeans_model)

filteredPistilDf <- allDf %>% filter(genotype == "gt1ra3B73") %>% filter(treatment=="no_treatment")
step1 <- lmer(PC1 ~ top_half + (1|plant_id) , data = filteredPistilDf, REML=FALSE)
nrow(model.frame(step1))
summary(step1)
emmeans_model <- emmeans(step1, ~ top_half)
pairs(emmeans_model)

allDf$plant_id <- paste0(allDf$`row number`, "-", allDf$`plant number`)

final <- right_join(allDf, allFilteredDf, by = "plant_id")  # all rows from df2, with matching rows from df1

################################################################################################################################################

# DEFOLIATION PHENOTYPING - tassel branch and tiller phenotyping - relates to Fig. 3

allDf <- read_csv('Defoliation_phenotyping_24UM.csv')
allDf$tbn <- as.numeric(allDf$tbn)
allDf$treatment <- ifelse(as.numeric(allDf$`plant number`) %% 2 == 1,"no_treatment", "defoliated")

allDf %>% filter(genotype == "gt1ra3B73") %>% ggplot( aes(x = treatment, y = tbn, color=treatment)) + 
  geom_boxplot(outlier.shape=NA, size=1.2) + geom_quasirandom(size=4.5, width=.1)

final %>% filter(genotype.x == "gt1ra3B73") %>% ggplot( aes(x = treatment.x, y = `Average tiller height/Plant Height`, color=treatment.x)) + 
  geom_boxplot(outlier.shape=NA, size=1.2) + geom_quasirandom(size=4.5, width=.1)