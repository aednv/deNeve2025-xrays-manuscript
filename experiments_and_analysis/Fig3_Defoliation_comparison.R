#libraries
library(lme4)
library(emmeans)
library(lmerTest)

#import dataframes post cleaning
setwd("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_24UM/results")
#this combined dataset csv was generated in Jupyter notebook "" , and has already been filtered for possible duplicates due to scan overlap and for segmentations that intersect with the image perimeter
allFilteredDf <- read_csv("joined_df_pre_5_pistil_count_filter.csv") #both 21 and 24UM data here

#filter for only plants with more than 5 pistil mask detections, only looking at gt1ra3B73 plants
allFilteredDf <- allFilteredDf %>%
  filter(genotype %in% c("gt1ra3B73")) %>%
  add_count(genotype, plant_id, name = "pistil_count") %>%
  filter(pistil_count >5)

#import defoliation phenotype data
setwd("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_24UM")
DefDf21 <- read_csv("Defoliation_phenotyping_21UM.csv")
DefDf24 <- read_csv("Defoliation_phenotyping_24UM.csv")

#merge phenotyping data and mask data
DefDf24$plant_id <- paste0(DefDf24$`row number`, "-", DefDf24$`plant number`)
DefDf24$treatment <- ifelse(as.numeric(DefDf24$`plant number`) %% 2 == 1,"no_treatment", "defoliated")
DefDf24$tbn <- as.numeric(DefDf24$tbn)
DefDf24$year <- 24
DefDf24 <- DefDf24 %>% select(treatment, tbn, plant_id, `Average tiller height/Plant Height`, year)
MergedDf <- right_join(DefDf24, allFilteredDf, by = c("plant_id", "treatment", "year"))  # all rows from df2, with matching rows from df1

DefDf21$`Average tiller height/Plant Height` <- DefDf21$tillers_all_added_cm/DefDf21$height_cm_if_tillers #its actually the opposite, fix col name later
DefDf21$tbn <- DefDf21$tbn_no_culm
DefDf21$year <- 21
DefDf21 <- DefDf21 %>% select(treatment, tbn, plant_id, `Average tiller height/Plant Height`, year)

MergedDf2 <- right_join(DefDf21, MergedDf, by = c("plant_id", "treatment", "year"))

MergedDf2 <- MergedDf2 %>%
  mutate(tbn = coalesce(tbn.x, tbn.y)) %>%
  select(-tbn.x, -tbn.y)

MergedDf2 <- MergedDf2 %>%
  mutate(standardized_tiller_len = coalesce(`Average tiller height/Plant Height.x`, `Average tiller height/Plant Height.y`)) %>%
  select(-`Average tiller height/Plant Height.x`, -`Average tiller height/Plant Height.y`)

MergedDf2$block <- paste0(MergedDf2$year, MergedDf2$planting_date)


#~~~~~~~~~~~~~tassel branch number graph~~~~~~~~~~~~~~~~~
SummarizedDf <- MergedDf2 %>%
  group_by(year, block, plant_id, treatment) %>%
  summarize(tbn = mean(tbn, na.rm = TRUE), .groups = 'drop') #all values are the same for each pistil entry per plant id

SummarizedDf$year <- as.factor(SummarizedDf$year)

SummarizedDf %>% ggplot(aes(x = factor(treatment, levels = c("no_treatment", "defoliated")), y = tbn, color=treatment)) + 
  geom_boxplot(outlier.shape=NA) + geom_quasirandom(aes(shape=block),size=3, width=.25) 

ggplot <- SummarizedDf %>% ggplot(aes(x = factor(treatment, levels = c("no_treatment", "defoliated")), y = tbn, color=treatment)) + 
  geom_boxplot(outlier.shape=NA) + geom_quasirandom(aes(shape=block),size=3, width=.25) 

setwd("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_24UM/results")
ggsave("final_tassel_branch_number_defoliation_21_24UM.pdf", ggplot, width = 8, height = 6) 
ggsave("final_tassel_branch_number_defoliation_21_24UM.png", ggplot, width = 8, height = 6, dpi = 300)  


#~~~~~~~~~~~~tassel branch number statistics~~~~~~~~~~~~
#ANOVA, fixed effects - block, treatment


anova_model <- aov(tbn ~ treatment * block, data = SummarizedDf)
summary(anova_model)
# 
# Df Sum Sq Mean Sq F value   Pr(>F)    
# treatment        1  72.39   72.39  33.181 2.94e-07 ***
#   block            3  81.25   27.08  12.413 1.94e-06 ***
#   treatment:block  3   3.10    1.03   0.474    0.702    
# Residuals       61 133.08    2.18                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 6 observations deleted due to missingness

#~~~~~~~~~~~~~standardized tiller length graph~~~~~~~~~~~~~~~~~
SummarizedDf <- MergedDf2 %>%
  group_by(year, block, plant_id, treatment) %>%
  summarize(standardized_tiller_len = mean(standardized_tiller_len, na.rm = TRUE), .groups = 'drop') #all values are the same for each pistil entry per plant id

SummarizedDf$year <- as.factor(SummarizedDf$year)

SummarizedDf %>% ggplot(aes(x = factor(treatment, levels = c("no_treatment", "defoliated")), y = standardized_tiller_len, color=treatment)) + 
  geom_boxplot(outlier.shape=NA) + geom_quasirandom(aes(shape=block),size=3, width=.25)

ggplot <- SummarizedDf %>% ggplot(aes(x = factor(treatment, levels = c("no_treatment", "defoliated")), y = standardized_tiller_len, color=treatment)) + 
  geom_boxplot(outlier.shape=NA) + geom_quasirandom(aes(shape=block),size=3, width=.25) 

setwd("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_24UM/results")
ggsave("final_standardized_tiller_len_defoliation_21_24UM.pdf", ggplot, width = 8, height = 6) 
ggsave("final_standardized_tiller_len_defoliation_21_24UM.png", ggplot, width = 8, height = 6, dpi = 300)  


#~~~~~~~~~~~~standardized tiller length statistics~~~~~~~~~~~~
#ANOVA, fixed effects - treatment, block

anova_model <- aov(standardized_tiller_len ~ treatment * block, data = SummarizedDf)
summary(anova_model)
# 
# Df Sum Sq Mean Sq F value   Pr(>F)    
# treatment        1  2.691  2.6908  15.567 0.000217 ***
#   block            3  0.340  0.1133   0.655 0.582952    
# treatment:block  3  1.431  0.4770   2.760 0.050220 .  
# Residuals       58 10.025  0.1728                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 38 observations deleted due to missingness


#~~~~~~~~~~~~~pistil area graph~~~~~~~~~~~~~~~~~

allFilteredDf$block <- paste0(allFilteredDf$year, allFilteredDf$planting_date)

mean_pistil_area <- allFilteredDf %>% group_by(plant_id, block, treatment) %>% summarise(mean_area=mean(pistil_area), .groups = 'drop')

allFilteredDf %>% ggplot(aes(x = factor(treatment, levels = c("no_treatment", "defoliated")), y = pistil_area, color=treatment)) + 
  geom_boxplot(outlier.shape=NA) + geom_quasirandom(size=1, width=.4,alpha=.3) + geom_quasirandom(data = mean_pistil_area, aes(x = treatment, y = mean_area, size=3, shape=block), width=.4)

ggplot <- allFilteredDf %>% ggplot(aes(x = factor(treatment, levels = c("no_treatment", "defoliated")), y = pistil_area, color=treatment)) + 
  geom_boxplot(outlier.shape=NA) + geom_quasirandom(size=1, width=.4,alpha=.3) + geom_quasirandom(data = mean_pistil_area, aes(x = treatment, y = mean_area, size=3, shape=block), width=.4)

setwd("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_24UM/results")
ggsave("final_pistil_size_defoliation_21_24UM.pdf", ggplot, width = 8, height = 6) 
ggsave("final_pistil_size_defoliation_21_24UM.png", ggplot, width = 8, height = 6, dpi = 300)  

#~~~~~~~~~~~~~pistil area statistics~~~~~~~~~~~~~~~~~

#mixed linear model

step1 <- lmer(pistil_area ~ treatment + (1|block/plant_id) , data = allFilteredDf, REML=FALSE)
nrow(model.frame(step1))
summary(step1)
# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: pistil_area ~ treatment + (1 | block/plant_id)
#    Data: allFilteredDf
# 
#      AIC      BIC   logLik deviance df.resid 
#  71143.8  71175.8 -35566.9  71133.8     4445 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.6274 -0.6351 -0.1193  0.4060  7.2702 
# 
# Random effects:
#  Groups         Name        Variance Std.Dev.
#  plant_id:block (Intercept)  44466   210.9   
#  block          (Intercept)  21953   148.2   
#  Residual                   497060   705.0   
# Number of obs: 4450, groups:  plant_id:block, 75; block, 4
# 
# Fixed effects:
#                       Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)           1912.683     84.300    5.165  22.689 2.26e-06 ***
# treatmentno_treatment   24.431     54.840   65.414   0.445    0.657    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# trtmntn_trt -0.349
# emmeans_model <- emmeans(step1, ~ treatment, pbkrtest.limit = 4450)
# pairs(emmeans_model)
# contrast                  estimate   SE df t.ratio p.value
# defoliated - no_treatment    -24.4 55.3 72  -0.442  0.6602
# 
# Degrees-of-freedom method: kenward-roger 

#"A linear mixed model was fitted to predict pistil area with treatment 
#as a fixed effect and block & plant id as random effects (pistil_area ~ treatment + (1 | block/plant_id)). 
#The estimated marginal means indicated that effect of treatment was not significant (p = 0.6602)."
