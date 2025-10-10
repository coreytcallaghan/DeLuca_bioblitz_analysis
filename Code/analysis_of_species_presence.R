# Analysis of observations, observers, and quantification of bioblitz
## This script includes figure 4, the supplementary figures for bar and pie plots about taxonomy
## It also includes shannon's index and a way to visualize uniqueness at DeLuca

library(tidyverse)
library(patchwork)
library(scales)
library(vegan)
library(sf)
library(ggrepel)

# Read in the bioblitz data
deluca_bioblitz <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")

# Read in total iNat data with just counts and remove nas
regional_species_counts <- read_csv("Data/Summarized_Data/regional_species_counts.csv") %>%
  dplyr::filter(!is.na(Species))

## Get number of unique observers
unique(deluca_bioblitz$user_login) #203
unique(deluca_bioblitz$observed_on) #5, but 2023 is broken into the 24th and 25th (3 observations)
sum(deluca_bioblitz$observed_on == "2023-02-25")
deluca_bioblitz <- deluca_bioblitz %>%
  filter(observed_on != "2023-02-25")

########################
# Plot Supplementary Figure for observations and observers bell curve log scale
#########################
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
o_productivity <- ggplot(observer_summary, aes(x = n_obs, fill = top_5)) +
  geom_histogram(
    bins = 30,
    color = "white",
    alpha = 0.9,
    position = "identity"
  ) +
  scale_fill_manual(
    values = c("FALSE" = "grey50", "TRUE" = "#1f78b4"),
    labels = c("Other Observers", "Top 5% Observers"),
    name = NULL
  ) +
  scale_x_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000),
    labels = comma
  ) +
  labs(
    title = NULL,
    x = "Number of Observations (log scale)",
    y = "Number of Observers"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title = element_text(size = 14),
    axis.text = element_text(color = "black", size = 12),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

o_productivity

ggsave("Figures/Supp/observer_productivity.png", o_productivity, height = 6, width = 8, bg= "transparent")

#################
## Get quick results for research grade obs
#################

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

### Count how many species have only 1 research-grade observation
rare_species_1 <- most_observed_species %>%
  filter(observation_count <= 1)
rare_species_1 #276

#### Quick visualization of top five species + number of observations
deluca_bioblitz_research %>%
  group_by(taxon_species_name, common_name) %>%
  summarise(observation_count = n(), .groups = "drop") %>%
  arrange(desc(observation_count)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = reorder(common_name, observation_count), 
             y = observation_count, 
             fill = common_name)) + 
  geom_col() +
  labs(
    title = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_classic(base_size = 14) +
  coord_flip() +
  theme(legend.position = "none") 

#####################
## Read in entire florida inat
#####################
# Brittany's addition: 
#inat_files <- list.files("Data/Florida_Data/", full.names = TRUE)
#inat_combined <- bind_rows(lapply(inat_files[1:65], read_csv))

#saveRDS(inat_combined, "Data/Florida_Data/inat_combined.RDS")

## Read in entire Florida iNat
#inat_combined <- readRDS("Data/Florida_Data/inat_combined.RDS")

#head(inat_combined)
#colnames(inat_combined)

#inat_combined_research <- inat_combined %>%
#  dplyr::select(
#    id, uuid, observed_on, time_observed_at, created_at,
#    quality_grade, license, url, image_url,
#    user_id, user_login,
#    place_guess, latitude, longitude, positional_accuracy,
#    place_town_name, place_county_name, place_state_name,
#    place_country_name,
#    species_guess, scientific_name, common_name, iconic_taxon_name,
#    taxon_id, 
#    taxon_kingdom_name, taxon_phylum_name, taxon_class_name,
#    taxon_order_name, taxon_family_name, taxon_genus_name,
#    taxon_species_name, taxon_subspecies_name
#  )

## Do not need to filter to only research grade data because that is all the file contains
## Save as RDS for easy call back
#saveRDS(inat_combined_research, "Data/Florida_Data/inat_combined_research.RDS")

## Read in cleaned Florida iNat
### Come back to this code 
inat_combined_research <- readRDS("Data/Florida_Data/inat_combined_research.RDS")

#######################
## Prepare figure 5 for 
## rarity of species in DeLucao
#######################
### Prepare relative observation ratio per species
# Count occurrences in DeLuca
deluca_freq <- deluca_bioblitz_research %>%
  count(taxon_species_name, name = "obs_deluca")

# Count occurrences in Florida (restricted to species observed at DeLuca)
inat_freq <- inat_combined_research %>%
  filter(taxon_species_name %in% deluca_freq$taxon_species_name) %>%
  count(taxon_species_name, name = "obs_florida")

freq_df <- deluca_freq %>%
  left_join(inat_freq, by = "taxon_species_name") %>%
  mutate(
    deluca_only = (obs_deluca == obs_florida),
    category = case_when(
      deluca_only ~ "Unique to DeLuca",
      obs_deluca < 10 & obs_florida > 25 ~ "Locally Rare",  
      obs_deluca > 10 & obs_florida > 25 ~ "Common Everywhere",
      obs_deluca < 10 & obs_florida <= 25 ~ "Rare Everywhere",
      TRUE ~ "Underreported Everywhere"
    )
  )

# Recompute deluca_freq in freq_df
freq_df <- freq_df %>%
  mutate(deluca_freq = obs_deluca / obs_florida)

# Label top 5 statewide + all DeLuca-only + all Rare Everywhere
label_points <- freq_df %>%
  arrange(desc(obs_florida)) %>%
  slice_head(n = 5) %>%
  bind_rows(
    freq_df %>%
      arrange(desc(obs_deluca)) %>%
      slice_head(n = 5)
  ) %>%
  bind_rows(
    freq_df %>% filter(deluca_only)
  ) %>%
  bind_rows(
    freq_df %>% filter(category == "Rare Everywhere")
  ) %>%
  distinct(taxon_species_name, .keep_all = TRUE)

### Plot with obs_deluca vs. proportion
freq_plot_prop <- ggplot(freq_df, aes(x = obs_deluca, y = deluca_freq, color = category)) +
  geom_point(alpha = 1, size = 3) +
  geom_text_repel(
    data = label_points,
    aes(x = obs_deluca, y = deluca_freq, label = taxon_species_name), 
    size = 3,
    fontface = "bold",
    color = "black",
    max.overlaps = Inf,
    na.rm = TRUE
  ) +
  labs(
    x = "DeLuca Observations (total count)",
    y = "Proportion of DeLuca / Florida observations",
    color = "Category"
  ) +
  scale_x_log10() +
  scale_color_manual(
    values = c(
      "Unique to DeLuca" = "#d95f02",
      "Locally Rare" = "#7570b3", 
      "Rare Everywhere" = "#1b9e77",
      "Common Everywhere" = "#1f78b4",
      "Underreported Everywhere" = "#e6ab02" 
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

freq_plot_prop

## save as png
ggsave("Figures/figure_5_rarity_deluca_state.png", plot = freq_plot_prop, bg = "transparent")


#### Now same figure
### without labels
### Plot
freq_plot_prop_clean <- ggplot(freq_df, aes(x = obs_deluca, y = deluca_freq, color = category)) +
  geom_jitter(size = 2.5, alpha = 0.75, width = 0.05, height = 0) + 
  labs(
    x = "DeLuca Observations (total count)",
    y = "Proportion of DeLuca / Florida observations",
    color = "Category"
  ) +
  scale_x_log10() +
  scale_color_manual(
    values = c(
      "Unique to DeLuca" = "#d95f02",
      "Locally Rare" = "#7570b3", 
      "Rare Everywhere" = "#1b9e77",
      "Common Everywhere" = "#1f78b4",
      "Underreported Everywhere" = "#e6ab02" 
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

freq_plot_prop_clean

## save as png
ggsave("Figures/figure_5_rarity_deluca_state_clean.png", plot = freq_plot_prop_clean, width = 10, height = 7, bg = "transparent")

###############################
###### OR how it performs total
## This is just count data
## Supplementary Figure
###############################
regional_counts <- regional_species_counts %>%
  mutate(
    Prop_Osceola = deluca_bioblitz / `Osceola County`,
    Prop_Florida = deluca_bioblitz / Florida,
    Prop_All = deluca_bioblitz / All
  ) %>%
  select(Species, Prop_Osceola, Prop_Florida, Prop_All) %>%
  pivot_longer(
    cols = starts_with("Prop"),
    names_to = "Group",
    values_to = "Proportion"
  ) %>%
  mutate(
    Group = factor(Group, 
                   levels = c("Prop_Osceola", "Prop_Florida", "Prop_All"),
                   labels = c("Osceola County", "Florida", "All of iNaturalist"))
  )

# Plot
regional_counts_plot <- ggplot(regional_counts, aes(x = Group, y = Proportion)) +
  geom_boxplot(fill = "#1f78b4", alpha = 1, width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "black", size = 1.8) +
  scale_y_continuous(
    trans = pseudo_log_trans(base = 10, sigma = 1e-4),
    breaks = c(0, 0.001, 0.01, 0.1, 0.5, 1),
    labels = c("0", "0.001", "0.01", "0.1", "0.5", "1"),
    limits = c(0, 1)
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = NULL,
    x = "",
    y = "Proportion of Records from DeLuca"
  ) +
  theme(
    axis.text.x = element_text(color = "black", angle = 0, hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

regional_counts_plot

ggsave("Figures/Supp/deluca_rarity_total_obs_global.png", plot = regional_counts_plot, bg = "transparent")

############################
## Quantify iNat data with Shannon's
## from Bioblitz and Florida
############################

### Running Shannon's Diversity Index:
## DeLuca Shannon Index 5.809113
deluca_counts <- deluca_bioblitz_research %>%
  count(taxon_species_name) %>%
  pull(n)
shannon_deluca <- diversity(deluca_counts, index = "shannon")

## Florida Shannon Index 6.819194
fl_counts <- inat_combined_research %>%
  count(taxon_species_name) %>%
  pull(n)

shanon_florida <- diversity(fl_counts, index = "shannon")


# now think about valuable observations (in terms of rarity)
# Compute uniqueness indicators

# Precompute sets so takes less time to run for deluca, osceola, and florida
deluca_species <- deluca_bioblitz_research$taxon_species_name
osceola_species <- inat_combined_research %>%
  filter(place_county_name == "Osceola") %>%
  pull(taxon_species_name)
florida_species <- inat_combined_research$taxon_species_name

# Create summary table
records_value_summary <- tibble(
  taxon_species_name = unique(c(deluca_species, florida_species))
) %>%
  mutate(
    deluca_bioblitz = taxon_species_name %in% deluca_species,
    osceola_county = taxon_species_name %in% osceola_species,
    florida_state = taxon_species_name %in% florida_species
  )

# Summary metrics
summary_metrics <- records_value_summary %>%
  summarise(
    total_species = n(),
    deluca_unique_species = sum(deluca_bioblitz & !osceola_county & !florida_state),
    county_species = sum(osceola_county),
    pct_county = mean(osceola_county) * 100,
    state_species = sum(florida_state),
    pct_state = mean(florida_state) * 100
  )

summary_metrics

#######################################
## Possible supplementary figure to show
## more rarity anaylysis
### with summary metrics
#####################################
# Identify species unique to DeLuca
records_value_summary <- records_value_summary %>%
  mutate(unique_to_deluca = deluca_bioblitz & !osceola_county & !florida_state)

##
records_value_summary %>% 
  summarise(
    n_deluca = sum(deluca_bioblitz),
    n_osceola = sum(osceola_county),
    n_florida = sum(florida_state),
    n_unique = sum(unique_to_deluca)
  )

# Total number of Florida species
n_florida <- sum(records_value_summary$florida_state)

# Bar heights: all proportions relative to Florida
df_plot <- tibble(
  Region = factor(
    c("DeLuca", "Osceola", "Florida"),
    levels = c("DeLuca", "Osceola", "Florida")
  ),
  PropFlorida = c(
    sum(records_value_summary$deluca_bioblitz) / n_florida,
    sum(records_value_summary$osceola_county) / n_florida,
    n_florida / n_florida  # always 1
  )
)

# Plot
ggplot(df_plot, aes(x = Region, y = PropFlorida)) +
  geom_col(fill = "darkgreen", alpha = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    y = "% of Florida Species Present",
    x = "Region",
    title = NULL
  ) +
  theme_minimal()

#######################################
## Possible supplementary figure to show
## more rarity anaylysis
### with summary metrics
### Visualizing number of rare species in DeLuca
#####################################
# Rarity classification for species in DeLuca Bioblitz
deluca_counts <- deluca_bioblitz_research %>%
  count(taxon_species_name, name = "obs_count")

deluca_counts %>%
  mutate(rarity_class = case_when(
    obs_count <= 5 ~ "Very rare (≤5)",
    obs_count <= 20 ~ "Rare (6–20)",
    TRUE ~ "Common (>20)"
  )) %>%
  count(rarity_class) %>%
  ggplot(aes(x = rarity_class, y = n, fill = rarity_class)) +
  geom_col(show.legend = FALSE) +
  labs(
    x = "DeLuca Rarity Class",
    y = "Number of Species Documented",
    title = NULL
  ) +
  theme_minimal() +
  coord_flip()

######################
# Supplementary figures
# Visualizing the groups of species found
# For DeLuca only
######################
### Bar Plot
#### to show taxonomy totals
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

ggsave("Figures/Supp/bar_plot_species_group.png", plot = final_plot_bar, bg = "transparent")

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

ggsave("Figures/Supp/pie_plot_species_group.png", plot = final_pie_plot_pretty, bg = "transparent")

