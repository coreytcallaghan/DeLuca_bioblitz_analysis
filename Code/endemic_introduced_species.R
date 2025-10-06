# supplementary figure for introduced and endemic species

library(tidyverse)
library(lubridate)

# Read in the bioblitz data and species native/non-native status
deluca_bioblitz <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")
species_status_florida <- read_csv("Data/DeLuca_iNaturalist_Data/species_status_florida.csv")

# Join the two datasets by scientific name
deluca_joined <- deluca_bioblitz %>%
  left_join(species_status_florida, by = c("scientific_name" = "name"))

# remove the odd ball date
deluca_joined <- deluca_joined %>%
  filter(observed_on != "2023-02-25")

## Filter to only include research grade data
deluca_joined_research <- deluca_joined %>%
  filter(quality_grade=="research")

# Make sure the date column is in Date format
deluca_joined_research <- deluca_joined_research %>%
  mutate(observed_on = as.Date(observed_on))

# Filter to only endemic and introduced species
endemic_introduced <- deluca_joined_research %>%
  filter(establishment_means %in% c("endemic", "introduced")) %>%
  distinct(scientific_name, establishment_means, observed_on)

# Get the first year each species was observed
species_first_seen <- endemic_introduced %>%
  mutate(year = year(observed_on)) %>%
  group_by(establishment_means, scientific_name) %>%
  summarise(first_year = min(year), .groups = "drop")

# Count cumulative species over time
cumulative_counts <- species_first_seen %>%
  group_by(establishment_means, first_year) %>%
  summarise(new_species = n(), .groups = "drop") %>%
  arrange(establishment_means, first_year) %>%
  group_by(establishment_means) %>%
  mutate(cumulative_species = cumsum(new_species)) %>%
  ungroup()

# Plot cumulative species over time
cumul_plot_endemic_invasive <- ggplot(
  cumulative_counts, aes(x = first_year, y = cumulative_species, color = establishment_means)) +
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  scale_color_manual(values = c("endemic" = "#4575b4", "introduced" = "#f46d43")) +
  labs(
    x = NULL,
    y = "Cumulative Number of Unique Species",
    color = "Establishment Type",
    title = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank() 
  )

# View the plot
cumul_plot_endemic_invasive

ggsave("Figures/Supp/cumul_plot_endemic_invasive.png", plot = cumul_plot_endemic_invasive, bg = "transparent")
