###########################################################
# DeLuca Bioblitz & Random Polygon Analysis
# Includes Florida iNaturalist comparisons
###########################################################

# Load packages
library(tidyverse)   # includes dplyr, ggplot2, readr, etc.
library(sf)          # for spatial data
library(viridis)     # color scales
library(ggspatial)   # north arrows etc.
library(biscale)     # bivariate mapping
library(patchwork)   # combining plots
library(spdep)       # spatial autocorrelation

###########################################################
# 1. Read in data
###########################################################
deluca_bioblitz <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")
random_polygon_effort <- read_csv("Data/Summarized_Data/random_polygon_effort.csv")
regional_species_counts <- read_csv("Data/Summarized_Data/regional_species_counts.csv")
distance_to_nearest_obs <- read_csv("Data/Summarized_Data/distance_to_nearest_obs.csv")
florida_inat <- readRDS("Data/Florida_Data/inat_combined_research.RDS")

###########################################################
# 2. Basic summaries
###########################################################
# Unique dates
length(unique(deluca_bioblitz$observed_on))

# Histogram of species counts (all & RG)
species_hist_all <- deluca_bioblitz %>%
  filter(!is.na(taxon_species_name)) %>%
  group_by(taxon_species_name) %>%
  summarise(N = n(), .groups = "drop")

ggplot(species_hist_all, aes(x = N)) +
  geom_histogram(color = "black", fill = "gray80") +
  theme_bw() +
  xlab("Number of observations") +
  ylab("Number of species (All quality grades)")

species_hist_RG <- deluca_bioblitz %>%
  filter(quality_grade == "research") %>%
  filter(!is.na(taxon_species_name)) %>%
  group_by(taxon_species_name) %>%
  summarise(N = n(), .groups = "drop")

ggplot(species_hist_RG, aes(x = N)) +
  geom_histogram(color = "black", fill = "gray80") +
  theme_bw() +
  xlab("Number of observations") +
  ylab("Number of species (Research grade only)")

###########################################################
# 3. Species accumulation plots
###########################################################
first_obs_species_all <- deluca_bioblitz %>%
  select(taxon_genus_name, taxon_species_name, observed_on, quality_grade) %>%
  filter(!is.na(taxon_species_name)) %>%
  group_by(taxon_species_name, observed_on) %>%
  distinct() %>%
  group_by(observed_on) %>%
  summarise(new_species = n_distinct(taxon_species_name), .groups = "drop") %>%
  mutate(cumulative_species = cumsum(new_species))

ggplot(first_obs_species_all, aes(x = observed_on)) +
  geom_col(aes(y = new_species), fill = "steelblue", alpha = 0.6, width = 4) +
  geom_line(aes(y = cumulative_species), color = "darkgreen", size = 1.2) +
  geom_point(aes(y = cumulative_species), color = "darkgreen", size = 2) +
  xlab("Sampling Date") +
  ylab("Species count (All grades)") +
  theme_bw()

# RG only
first_obs_species_RG <- deluca_bioblitz %>%
  filter(quality_grade == "research") %>%
  filter(!is.na(taxon_species_name)) %>%
  group_by(taxon_species_name, observed_on) %>%
  distinct() %>%
  group_by(observed_on) %>%
  summarise(new_species = n_distinct(taxon_species_name), .groups = "drop") %>%
  mutate(cumulative_species = cumsum(new_species))

ggplot(first_obs_species_RG, aes(x = observed_on)) +
  geom_col(aes(y = new_species), fill = "steelblue", alpha = 0.6, width = 4) +
  geom_line(aes(y = cumulative_species), color = "darkgreen", size = 1.2) +
  geom_point(aes(y = cumulative_species), color = "darkgreen", size = 2) +
  xlab("Sampling Date") +
  ylab("Species count (RG only)") +
  theme_bw()

###########################################################
# 4. Compare DeLuca to random polygons
###########################################################
# DeLuca values
deluca_values <- data.frame(
  number_of_observations = nrow(deluca_bioblitz),
  number_of_observers = length(unique(deluca_bioblitz$user_id)),
  number_of_species = deluca_bioblitz %>%
    filter(quality_grade == "research", !is.na(taxon_species_name)) %>%
    distinct(taxon_species_name) %>%
    nrow()
) %>%
  pivot_longer(cols = everything())

# Random polygons
random_polygon_long <- random_polygon_effort %>%
  select(polygon_id, number_of_observations, number_of_observers, number_of_species) %>%
  pivot_longer(cols = c(number_of_observations, number_of_observers, number_of_species))

# Violin + point plot
ggplot(random_polygon_long, aes(x = name, y = value)) +
  geom_violin(fill = "gray80", alpha = 0.5) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8) +
  geom_point(data = deluca_values, aes(x = name, y = value), color = "red", size = 3) +
  facet_wrap(~name, scales = "free") +
  scale_y_log10() +
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        axis.title.y = element_blank())

###########################################################
# 5. Compare DeLuca, Random Polygons, and Florida iNat
###########################################################
# Species counts
deluca_species_count <- deluca_bioblitz %>%
  filter(quality_grade == "research") %>%
  filter(!is.na(taxon_species_name)) %>%
  distinct(taxon_species_name) %>%
  nrow()

florida_species_count <- florida_inat %>%
  filter(!is.na(scientific_name)) %>%
  distinct(scientific_name) %>%
  nrow()

plot_df <- bind_rows(
  tibble(Source = "DeLuca Bioblitz", species_count = deluca_species_count),
  tibble(Source = "Florida iNat", species_count = florida_species_count),
  random_polygon_effort %>%
    select(polygon_id, number_of_species) %>%
    rename(species_count = number_of_species) %>%
    mutate(Source = "Random Polygon")
)

# Plot
ggplot(plot_df, aes(x = Source, y = species_count, fill = Source)) +
  geom_violin(data = subset(plot_df, Source == "Random Polygon"), alpha = 0.5) +
  geom_boxplot(data = subset(plot_df, Source == "Random Polygon"), width = 0.2, outlier.shape = NA) +
  geom_point(data = subset(plot_df, Source != "Random Polygon"), aes(color = Source), size = 4) +
  scale_y_log10() +
  labs(
    title = "Species Counts: DeLuca Bioblitz vs Random Polygons vs Florida iNat",
    y = "Number of species (log scale)",
    x = ""
  ) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold", size = 14))


