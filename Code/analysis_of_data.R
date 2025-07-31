## A script for some analysis of the DeLuca bioblitz data


# packages
library(tidyverse)
library(ggplot2)
library(readr)

# read in data
random_polygon_effort <- read_csv("Data/Summarized_Data/random_polygon_effort.csv")

regional_species_counts <- read_csv("Data/Summarized_Data/regional_species_counts.csv")

distance_to_nearest_obs <- read_csv("Data/Summarized_Data/distance_to_nearest_obs.csv")
