# Analysis of observations, observers, and quantification of bioblitz
# Levi Hoskins
# 7 Aug 2025

library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(readr)
library(scales)
library(purrr)

# Read in the bioblitz data
deluca_bioblitz <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")

## Get number of unique observers
unique(deluca_bioblitz$user_login) #203

# Summarize number of observations per observer
observer_summary <- deluca_bioblitz %>%
  filter(!is.na(user_login)) %>%
  group_by(user_login) %>%
  summarise(n_obs = n()) %>%
  ungroup()

# Plot (log x-axis for visualization)
## Add flag for top 5% observers
observer_summary <- observer_summary %>%
  mutate(top_5 = n_obs >= quantile(n_obs, 0.95))

# Plot with highlighted top 5%
ggplot(observer_summary, aes(x = n_obs, fill = top_5)) +
  geom_histogram(
    bins = 30,
    color = "white",
    alpha = 0.9,
    position = "identity"
  ) +
  scale_fill_manual(
    values = c("FALSE" = "grey50", "TRUE" = "tomato"),
    labels = c("Other Observers", "Top 5% Observers"),
    name = NULL
  ) +
  scale_x_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000),
    labels = comma
  ) +
  labs(
    title = "Observer Productivity in DeLuca Preserve",
    subtitle = "Top 5% observers highlighted",
    x = "Number of Observations (log scale)",
    y = "Number of Observers"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title = element_text(size = 14),
    axis.text = element_text(color = "black", size = 12),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

## Filter to only include research grade data
deluca_bioblitz_research <- deluca_bioblitz %>%
  filter(quality_grade=="research")

### get a list of species
species <- unique(deluca_bioblitz_research$taxon_species_name)

# Total unique species overall
total_species <- deluca_bioblitz %>%
  summarise(n_species = n_distinct(taxon_species_name)) %>%
  pull(n_species)

# Total unique species in research grade only
research_species <- deluca_bioblitz %>%
  filter(quality_grade == "research") %>%
  summarise(n_species = n_distinct(taxon_species_name)) %>%
  pull(n_species)

### Count the number of observations per species
most_observed_species <- deluca_bioblitz_research %>%
  group_by(taxon_species_name) %>%
  summarise(observation_count = n()) %>%
  arrange(desc(observation_count))

#### View top 10 most observed species
head(most_observed_species, 10)

### Count how many species have only 1 or 2 research-grade observations
rare_species_2 <- most_observed_species %>%
  filter(observation_count <= 2)
rare_species_2 #403

### Count how many species have only 1 or 2 research-grade observations
rare_species_1 <- most_observed_species %>%
  filter(observation_count <= 1)
rare_species_1 #276

# Top 5 most observed species
deluca_bioblitz_research %>%
  group_by(taxon_species_name, common_name) %>%
  summarise(observation_count = n(), .groups = "drop") %>%
  arrange(desc(observation_count)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = reorder(common_name, observation_count), y = observation_count)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_classic(base_size = 14) +
  coord_flip()

## Taxonmic focus of DeLuca compared to Florida
# Observed richness per group in DeLuca Bioblitz
deluca_group_counts <- deluca_bioblitz_research %>%
  group_by(iconic_taxon_name) %>%
  summarise(n_species = n_distinct(taxon_species_name)) %>%
  mutate(proportion = n_species / sum(n_species))

######## 65 csv files so takes a second to run
inat1 <- read_csv("Data/Florida_Data/inat1.csv")
inat2 <- read_csv("Data/Florida_Data/inat2.csv")
inat3 <- read_csv("Data/Florida_Data/inat3.csv")
inat4 <- read_csv("Data/Florida_Data/inat4.csv")
inat5 <- read_csv("Data/Florida_Data/inat5.csv")
inat6 <- read_csv("Data/Florida_Data/inat6.csv")
inat7 <- read_csv("Data/Florida_Data/inat7.csv")
inat8 <- read_csv("Data/Florida_Data/inat8.csv")
inat9 <- read_csv("Data/Florida_Data/inat9.csv")
inat10 <- read_csv("Data/Florida_Data/inat10.csv")
inat11 <- read_csv("Data/Florida_Data/inat11.csv")
inat12 <- read_csv("Data/Florida_Data/inat12.csv")
inat13 <- read_csv("Data/Florida_Data/inat13.csv")
inat14 <- read_csv("Data/Florida_Data/inat14.csv")
inat15 <- read_csv("Data/Florida_Data/inat15.csv")
inat16 <- read_csv("Data/Florida_Data/inat16.csv")
inat17 <- read_csv("Data/Florida_Data/inat17.csv")
inat18 <- read_csv("Data/Florida_Data/inat18.csv")
inat19 <- read_csv("Data/Florida_Data/inat19.csv")
inat20 <- read_csv("Data/Florida_Data/inat20.csv")
inat21 <- read_csv("Data/Florida_Data/inat21.csv")
inat22 <- read_csv("Data/Florida_Data/inat22.csv")
inat23 <- read_csv("Data/Florida_Data/inat23.csv")
inat24 <- read_csv("Data/Florida_Data/inat24.csv")
inat25 <- read_csv("Data/Florida_Data/inat25.csv")
inat26 <- read_csv("Data/Florida_Data/inat26.csv")
inat27 <- read_csv("Data/Florida_Data/inat27.csv")
inat28 <- read_csv("Data/Florida_Data/inat28.csv")
inat29 <- read_csv("Data/Florida_Data/inat29.csv")
inat30 <- read_csv("Data/Florida_Data/inat30.csv")
inat31 <- read_csv("Data/Florida_Data/inat31.csv")
inat32 <- read_csv("Data/Florida_Data/inat32.csv")
inat33 <- read_csv("Data/Florida_Data/inat33.csv")
inat34 <- read_csv("Data/Florida_Data/inat34.csv")
inat35 <- read_csv("Data/Florida_Data/inat35.csv")
inat36 <- read_csv("Data/Florida_Data/inat36.csv")
inat37 <- read_csv("Data/Florida_Data/inat37.csv")
inat38 <- read_csv("Data/Florida_Data/inat38.csv")
inat39 <- read_csv("Data/Florida_Data/inat39.csv")
inat40 <- read_csv("Data/Florida_Data/inat40.csv")
inat41 <- read_csv("Data/Florida_Data/inat41.csv")
inat42 <- read_csv("Data/Florida_Data/inat42.csv")
inat43 <- read_csv("Data/Florida_Data/inat43.csv")
inat44 <- read_csv("Data/Florida_Data/inat44.csv")
inat45 <- read_csv("Data/Florida_Data/inat45.csv")
inat46 <- read_csv("Data/Florida_Data/inat46.csv")
inat47 <- read_csv("Data/Florida_Data/inat47.csv")
inat48 <- read_csv("Data/Florida_Data/inat48.csv")
inat49 <- read_csv("Data/Florida_Data/inat49.csv")
inat50 <- read_csv("Data/Florida_Data/inat50.csv")
inat51 <- read_csv("Data/Florida_Data/inat51.csv")
inat52 <- read_csv("Data/Florida_Data/inat52.csv")
inat53 <- read_csv("Data/Florida_Data/inat53.csv")
inat54 <- read_csv("Data/Florida_Data/inat54.csv")
inat55 <- read_csv("Data/Florida_Data/inat55.csv")
inat56 <- read_csv("Data/Florida_Data/inat56.csv")
inat57 <- read_csv("Data/Florida_Data/inat57.csv")
inat58 <- read_csv("Data/Florida_Data/inat58.csv")
inat59 <- read_csv("Data/Florida_Data/inat59.csv")
inat60 <- read_csv("Data/Florida_Data/inat60.csv")
inat61 <- read_csv("Data/Florida_Data/inat61.csv")
inat62 <- read_csv("Data/Florida_Data/inat62.csv")
inat63 <- read_csv("Data/Florida_Data/inat63.csv")
inat64 <- read_csv("Data/Florida_Data/inat64.csv")
inat65 <- read_csv("Data/Florida_Data/inat65.csv")

# Generate file paths for inat1.csv to inat65.csv
inat_files <- paste0("Data/Florida_Data/inat", 1:65, ".csv")

inat_files <- list.files("Data/Florida_Data/")

# Read and combine them into one dataframe
inat_combined <- map_dfr(inat_files, read_csv)

# Brittany's addition: 
inat_files <- list.files("Data/Florida_Data/", full.names = TRUE)
inat_combined <- bind_rows(lapply(inat_files[1:3], read_csv))

#######################
### Bar plot
######################
#### similar to sam's to show taxnomy totals
## chosen birds, plants, and insects --- although any other works as well

### Total unique species by Kingdom
kingdom_summary <- deluca_bioblitz_research %>%
  group_by(taxon_kingdom_name) %>%
  summarise(n_species = n_distinct(taxon_species_name), .groups = "drop")

# Kingdom
ggplot(kingdom_summary, aes(x = reorder(taxon_kingdom_name, n_species), y = n_species)) +
  geom_col(fill = "tomato") +
  labs(
    title = "Total Unique Species by Kingdom",
    x = "Kingdom",
    y = "Number of Species"
  ) +
  theme_minimal() +
  coord_flip()

### Total unique species by Phylum
phylum_summary <- deluca_bioblitz_research %>%
  group_by(taxon_phylum_name) %>%
  summarise(n_species = n_distinct(taxon_species_name), .groups = "drop")

# Phylum
phylum_plot <- ggplot(phylum_summary, aes(x = reorder(taxon_phylum_name, n_species), y = n_species)) +
  geom_col(fill = "forestgreen") +
  labs(
    title = "Total Unique Species by Phylum",
    x = "Phylum",
    y = "Number of Species"
  ) +
  theme_classic() +
  coord_flip()

### Plot taxonomy of each group
create_taxonomic_plot <- function(df, group_filter, group_label, tax_rank_col) {
  df %>%
    filter(!!group_filter) %>%
    group_by(!!sym(tax_rank_col)) %>%
    summarise(n_species = n_distinct(taxon_species_name), .groups = "drop") %>%
    ggplot(aes(x = reorder(!!sym(tax_rank_col), n_species), y = n_species)) +
    geom_col(fill = "#467010") +
    labs(
      title = paste("Total", group_label, "Species by", tax_rank_col),
      x = tax_rank_col,
      y = "Number of Species"
    ) +
    theme_classic() +
    coord_flip()
}

# Plants: class level
plant_class_plot <- create_taxonomic_plot(
  df = deluca_bioblitz_research,
  group_filter = quote(taxon_kingdom_name == "Plantae"),
  group_label = "Plant",
  tax_rank_col = "taxon_class_name"
)

# Insects: order level
insect_order_plot <- create_taxonomic_plot(
  df = deluca_bioblitz_research,
  group_filter = quote(taxon_phylum_name == "Arthropoda" & taxon_class_name == "Insecta"),
  group_label = "Insect",
  tax_rank_col = "taxon_order_name"
)

# Birds: order level
bird_order_plot <- create_taxonomic_plot(
  df = deluca_bioblitz_research,
  group_filter = quote(taxon_class_name == "Aves"),
  group_label = "Bird",
  tax_rank_col = "taxon_order_name"
)

### Combine plots
side_column <- plant_class_plot / insect_order_plot / bird_order_plot + 
  plot_layout(heights = c(1, 1, 1))

final_plot_bar <- phylum_plot | side_column + 
  plot_layout(widths = c(2.5, 1))

# Display
final_plot_bar


####################
### Pie Chart
###################
# Kingdom
ggplot(kingdom_summary, aes(x = "", y = n_species, fill = taxon_kingdom_name)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  labs(title = "Total Unique Species by Kingdom", fill = "Kingdom") +
  theme_void() +  # removes background, axes
  theme(legend.position = "right")

# Phylum
phylum_pie <- ggplot(phylum_summary, aes(x = "", y = n_species, fill = taxon_phylum_name)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  labs(title = "Total Unique Species by Phylum", fill = "Phylum") +
  theme_void() +
  theme(legend.position = "right")

## Plants
plant_class_summary <- deluca_bioblitz_research %>%
  filter(taxon_kingdom_name == "Plantae") %>%
  group_by(taxon_class_name) %>%
  summarise(n_species = n_distinct(taxon_species_name), .groups = "drop")

plant_class_pie <- ggplot(plant_class_summary, aes(x = "", y = n_species, fill = taxon_class_name)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  labs(title = "Plant Species by Class", fill = "Class") +
  theme_void() +
  theme(legend.position = "right")

## Insects
insect_order_summary <- deluca_bioblitz_research %>%
  filter(taxon_phylum_name == "Arthropoda", taxon_class_name == "Insecta") %>%
  group_by(taxon_order_name) %>%
  summarise(n_species = n_distinct(taxon_species_name), .groups = "drop")

insect_order_pie <- ggplot(insect_order_summary, aes(x = "", y = n_species, fill = taxon_order_name)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  labs(title = "Insect Species by Order", fill = "Order") +
  theme_void() +
  theme(legend.position = "right")

## Birds
bird_order_summary <- deluca_bioblitz_research %>%
  filter(taxon_class_name == "Aves") %>%
  group_by(taxon_order_name) %>%
  summarise(n_species = n_distinct(taxon_species_name), .groups = "drop")

bird_order_pie <- ggplot(bird_order_summary, aes(x = "", y = n_species, fill = taxon_order_name)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  labs(title = "Bird Species by Order", fill = "Order") +
  theme_void() +
  theme(legend.position = "right")

#### Plot
side_column_pies <- plant_class_pie / insect_order_pie / bird_order_pie + 
  plot_layout(heights = c(1, 1, 1))

final_pie_plot <- phylum_pie | side_column_pies + 
  plot_layout(widths = c(2.5, 1))

final_pie_plot

#### Graphing pie chart
create_pretty_pie <- function(df, fill_col, title) {
  df <- df %>%
    mutate(
      fraction = n_species / sum(n_species),
      percent_label = scales::percent(fraction, accuracy = 0.1),
      ypos = cumsum(fraction) - 0.5 * fraction
    )
  
  ggplot(df, aes(x = "", y = fraction, fill = !!sym(fill_col))) +
    geom_col(color = "white", size = 0.5) +
    coord_polar(theta = "y") +
    labs(title = title, fill = "") +
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8),
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
    ) +
    scale_fill_viridis_d(option = "plasma")
}

# Phylum
phylum_summary <- deluca_bioblitz_research %>%
  group_by(taxon_phylum_name) %>%
  summarise(n_species = n_distinct(taxon_species_name), .groups = "drop") %>%
  filter(!is.na(taxon_phylum_name)) %>%
  arrange(desc(n_species))

phylum_pie <- create_pretty_pie(phylum_summary, "taxon_phylum_name", "Species by Phylum")

# Plants: class
plant_class_summary <- deluca_bioblitz_research %>%
  filter(taxon_kingdom_name == "Plantae") %>%
  group_by(taxon_class_name) %>%
  summarise(n_species = n_distinct(taxon_species_name), .groups = "drop") %>%
  filter(!is.na(taxon_class_name)) %>%
  arrange(desc(n_species))

plant_class_pie <- create_pretty_pie(plant_class_summary, "taxon_class_name", "Plant Species by Class")

# Insects: order
insect_order_summary <- deluca_bioblitz_research %>%
  filter(taxon_phylum_name == "Arthropoda", taxon_class_name == "Insecta") %>%
  group_by(taxon_order_name) %>%
  summarise(n_species = n_distinct(taxon_species_name), .groups = "drop") %>%
  filter(!is.na(taxon_order_name)) %>%
  arrange(desc(n_species))

insect_order_pie <- create_pretty_pie(insect_order_summary, "taxon_order_name", "Insect Species by Order")

# Birds: order
bird_order_summary <- deluca_bioblitz_research %>%
  filter(taxon_class_name == "Aves") %>%
  group_by(taxon_order_name) %>%
  summarise(n_species = n_distinct(taxon_species_name), .groups = "drop") %>%
  filter(!is.na(taxon_order_name)) %>%
  arrange(desc(n_species))

bird_order_pie <- create_pretty_pie(bird_order_summary, "taxon_order_name", "Bird Species by Order")

## Plot
final_pie_plot_pretty <- (phylum_pie | plant_class_pie) / (insect_order_pie | bird_order_pie) +
  plot_annotation(title = "Species Composition by Taxonomic Group") +
  plot_layout(widths = c(1, 1), heights = c(1, 1))

final_pie_plot_pretty

