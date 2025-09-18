#PISTIL COUNT PER POSITION - relates to Fig. 2
#pistil quant / manual

setwd("/work/pi_mbartlett_umass_edu/AmberDeNeve/grass_carpel_ml_quantification_final/experiments/maize_24UM/results")
allFilteredDf <- read_csv("pistilsPerSpikeletPosition.csv")


allFilteredDf %>% ggplot(aes(x = pos_standard, y = pistilsPerSpikelet, colour = plant_id)) +
  geom_bar(position = "dodge")

ggplot(gt1PDfx, aes(x=y_pos_all_overlapped_len_standardized,color=plant_id, fill=plant_id)) + geom_histogram()   +  xlim(0, 1) + ylim(0,30)


allFilteredDf$pistilsPerSpikelet <- as.numeric(allFilteredDf$pistilsPerSpikelet)

allFilteredDf$pos_standard <- as.numeric(allFilteredDf$pos_standard)

allFilteredDf$plant_id <- as.factor(allFilteredDf$plant_id)
head(allFilteredDf)


library(dplyr)
library(ggplot2)

# Bin pos_standard into categories (e.g., 0.05 increments)
allFilteredDf <- allFilteredDf %>%
  mutate(pos_bin = round(pos_standard, 1))  # Adjust binning as needed

# Then make the stacked bar plot
ggplot(allFilteredDf, aes(x = factor(pos_bin), y = pistilsPerSpikelet, fill = plant_id)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Position (binned)", y = "Pistils per Spikelet") +
  theme_minimal()


library(dplyr)
library(ggplot2)

# Create bins in 0.10 increments
allFilteredDf <- allFilteredDf %>%
  mutate(pos_bin = cut(pos_standard, breaks = seq(0, 1, by = 0.10), include.lowest = TRUE))

# Plot with stacked bars
ggplot(allFilteredDf, aes(x = pos_bin, y = pistilsPerSpikelet, fill = plant_id)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Position (0.10 bins)", y = "Total Pistils") +
  theme_minimal()


library(dplyr)
library(ggplot2)

# Make sure pistilsPerSpikelet is a factor
allFilteredDf <- allFilteredDf %>%
  mutate(
    pistilsPerSpikelet = as.factor(pistilsPerSpikelet),
    pos_bin = cut(pos_standard, breaks = seq(0, 1, by = 0.10), include.lowest = TRUE)
  

# Count number of observations per pistil category per bin
binned_counts <- allFilteredDf %>%
  group_by(genotype, pos_bin, pistilsPerSpikelet) %>%
  summarise(count = n(), .groups = "drop")



# Plot grouped bar chart
ggplot(binned_counts, aes(x = pos_bin, y = count, fill = pistilsPerSpikelet)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Position (0.10 bins)", y = "Count", fill = "Pistils/Spikelet") +
  theme_minimal() + facet_wrap(~genotype)
