# A script for some analysis of the DeLuca bioblitz data
## This includes figures 3 and 5, and the two parts of figure figure 2 (the DuLuca plot and bivariate plot)
### Species accumulation curve, histogram for sampling over years, and violin plot

# packages
library(tidyverse)
library(sf)
library(biscale)
library(patchwork)
library(ggspatial)
library(RStoolbox)
library(maptiles)
library(terra)
library(scales)

# read in data
random_polygon_effort <- read_csv("Data/Summarized_Data/random_polygon_effort.csv")

regional_species_counts <- read_csv("Data/Summarized_Data/regional_species_counts.csv")

distance_to_nearest_obs <- read_csv("Data/Summarized_Data/distance_to_nearest_obs.csv")

# Read in the bioblitz data
deluca_bioblitz <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")

# quick summarization of bioblitz data
length(unique(deluca_bioblitz$observed_on))
unique(deluca_bioblitz$observed_on)

##########################################
### Making figure 3, both left and right
## Species accumulation curve and frequency distribution
########################################

# Histogram of all species
species_hist_plot_all <- ggplot(species_hist_all, aes(x=N)) +
  geom_histogram(color="black", fill="gray80") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color="black", fill=NA),
    axis.text = element_text(color="black")
  ) +
  xlab("Number of observations") +
  ylab("Number of species (RG + Needs ID)")

# Histogram of RG species only
species_hist_plot_RG <- ggplot(species_hist_RG, aes(x=N)) +
  geom_histogram(color="black", fill="gray80") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color="black", fill=NA),
    axis.text = element_text(color="black")
  ) +
  xlab("Number of observations") +
  ylab("Number of species (RG)")

# Accumulation plot for all species
accum_all_plot <- ggplot(first_obs_species_all, aes(x = observed_on)) +
  geom_col(aes(y = new_species), fill = "steelblue", alpha = 0.6, width=4) +
  geom_line(aes(y = cumulative_species), color = "darkgreen", size = 1.2) +
  geom_point(aes(y = cumulative_species), size = 2, color = "darkgreen") +
  xlab("Sampling Date") +
  ylab("Species count (RG + Needs ID)") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color="black", fill=NA),
    axis.text = element_text(color="black")
  )

# Accumulation plot for RG only
accum_RG_plot <- ggplot(first_obs_species_RG, aes(x = observed_on)) +
  geom_col(aes(y = new_species), fill = "black", alpha = 1, width=4) +
  geom_line(aes(y = cumulative_species), color = "darkgreen", size = 1.2) +
  geom_point(aes(y = cumulative_species), size = 2, color = "darkgreen") +
  xlab("Sampling Date") +
  ylab("Species count (RG)") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color="black", fill=NA),
    axis.text = element_text(color="black")
  )

#### Final figure 3
final_figure_3 <- species_hist_plot_RG + accum_RG_plot
final_figure_3

ggsave("Figures/figure_3_accum_hist.png", plot = final_figure_3, bg = "transparent")

####################
#### LPH addition
## Map of observations for species in DeLuca Preserve
deluca <- st_read("Data/Shapefile/deluca.shp")

# Transform DeLuca to WGS84
deluca <- st_transform(deluca, crs = 4326)

# Convert bioblitz data to sf
bioblitz_sf <- deluca_bioblitz %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Keep only points within DeLuca
bioblitz_sf <- st_filter(bioblitz_sf, deluca, .predicate = st_within)

# Project to UTM zone
bioblitz_sf_proj <- st_transform(bioblitz_sf, 32617)

# Create 500m hex grid for data
hex_grid <- st_make_grid(bioblitz_sf_proj, cellsize = 500, square = FALSE) %>%
  st_sf() %>%
  mutate(hex_id = row_number())

# Join points to hex grid
bioblitz_joined <- st_join(bioblitz_sf_proj, hex_grid, join = st_within)

# Summarize observation and species counts per hex
hex_summary <- bioblitz_joined %>%
  st_drop_geometry() %>%
  group_by(hex_id) %>%
  summarise(
    n_obs = n(),
    n_species = n_distinct(taxon_species_name)
  ) %>%
  left_join(hex_grid, by = "hex_id") %>%
  st_as_sf()

# Transform back to lon/lat for plotting
hex_summary_ll <- st_transform(hex_summary, 4326)

##############################################
### Figure 2: Bivariate classification and mapping
### Map DeLuca, add bivariate plot
##############################################

# Create bivariate classification
# Adjust 'style' or 'dim' as needed
hex_bi <- bi_class(
  hex_summary_ll,
  x = n_obs,
  y = n_species,
  style = "quantile",
  dim = 3
)

# Create an expanded bbox around your hex layer
bbox_orig <- st_bbox(hex_bi) 
buffer <- 0.1
bbox_expanded <- bbox_orig
bbox_expanded["xmin"] <- bbox_expanded["xmin"] - buffer
bbox_expanded["ymin"] <- bbox_expanded["ymin"] - buffer
bbox_expanded["xmax"] <- bbox_expanded["xmax"] + buffer
bbox_expanded["ymax"] <- bbox_expanded["ymax"] + buffer

bbox_sfc <- st_as_sfc(bbox_expanded)

# Download ESRI satellite imagery
sat_map <- get_tiles(bbox_sfc, zoom = 15, provider = "Esri.WorldImagery", crop = TRUE)

# Get the bounding box of Deluca
deluca_bbox <- st_bbox(deluca)

# Crop to bounding box first, then mask to exact shape
sat_cropped <- crop(sat_map, deluca)
sat_masked  <- mask(sat_cropped, deluca)
plot(sat_masked)

# Convert the raster to a data frame
sat_masked_df <- as.data.frame(sat_masked, xy = TRUE)

# Rename the columns for clarity (x, y, R, G, B)
names(sat_masked_df) <- c("x", "y", "R", "G", "B")


# Then plot using ggRGB on sat_masked instead
bi_map_sat <- ggplot() +
  geom_raster(data = sat_masked_df, aes(x = x, y = y), fill = rgb(sat_masked_df$R, sat_masked_df$G, sat_masked_df$B, maxColorValue = 255)) +
  geom_sf(data = hex_bi, aes(fill = bi_class), color = "black", size = 0.3, alpha = 1) +
  geom_sf(data = deluca, fill = NA, color = "black", size = 1) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  annotation_scale(location = "bl", width_hint = 0.3,
                   pad_x = unit(0.2, "in"), pad_y = unit(0.1, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering()) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = expression("Longitude ("*degree*W*")"),
    y = expression("Latitude ("*degree*N*")")
  ) +
  coord_sf(
    xlim = c(deluca_bbox["xmin"], deluca_bbox["xmax"]),
    ylim = c(deluca_bbox["ymin"], deluca_bbox["ymax"])
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

bi_map_sat

ggsave("Figures/figure_2_bivariate_map_deluca.png", plot = bi_map_sat, bg = "transparent")

# Bivariate legend
bi_legend <- bi_legend(
  pal = "DkViolet",
  dim = 3,
  xlab = "Higher Observation Density →",
  ylab = "↑ Higher Species Richness",
  size = 8
)
bi_legend

ggsave("Figures/figure_2_bivariate_legend_deluca.png", plot = bi_legend, bg = "transparent")

##############################################
### Figure 4: Comparison of DeLuca vs Random Polygons
##############################################
########## let's do some context of how DeLuca 'performs' to 100 random polygons
# Project to UTM for area calc (zone 17N for Florida)
deluca_proj <- st_transform(deluca, 32617)

# Area in km²
deluca_area_km2 <- as.numeric(st_area(deluca_proj)) / 1e6

# Normalize DeLuca values
deluca_values_norm <- data.frame(
  Observers_per_km2     = length(unique(deluca_bioblitz$user_id)) / deluca_area_km2,
  Observations_per_km2  = nrow(deluca_bioblitz) / deluca_area_km2,
  Species_per_km2       = deluca_bioblitz %>%
    filter(quality_grade == "research", !is.na(taxon_species_name)) %>%
    distinct(taxon_species_name) %>%
    nrow() / deluca_area_km2
) %>%
  pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value") %>%
  mutate(Source = "DeLuca Bioblitz")

# Normalize random polygons
# Join random polygon table with shapefile if not already
random_values_norm <- random_polygon_effort %>%
  dplyr::select(polygon_id, number_of_observations, number_of_observers, number_of_species) %>%
  mutate(
    Observers_per_km2    = number_of_observers / deluca_area_km2,
    Observations_per_km2 = number_of_observations / deluca_area_km2,
    Species_per_km2      = number_of_species / deluca_area_km2
  ) %>%
  select(polygon_id, Observers_per_km2, Observations_per_km2, Species_per_km2) %>%
  pivot_longer(cols = c("Observers_per_km2","Observations_per_km2","Species_per_km2"),
               names_to = "Metric", values_to = "Value") %>%
  mutate(Source = "Random Polygons")

# Combine datasets
combined_values <- bind_rows(deluca_values_norm, random_values_norm)

# Plot (violin + boxplot + DeLuca point overlay)
fig_4 <- combined_values %>%
  ggplot(aes(x = Metric, y = Value)) +
  geom_violin(
    data = subset(combined_values, Source == "Random Polygons"),
    fill = "gray80", color = "black", alpha = 0.6
  ) +
  geom_boxplot(
    data = subset(combined_values, Source == "Random Polygons"),
    width = 0.15, outlier.shape = NA, alpha = 0.8
  ) +
  geom_point(
    data = subset(combined_values, Source == "DeLuca Bioblitz"),
    color = "firebrick1",
    size = 4,
    shape = 18
  ) +
  scale_y_log10() + 
  labs(
    title = NULL,
    x = "",
    y = "Value (log-scaled)"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_text(color = "black", size = 12),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  )

fig_4

## Save as png
ggsave("Figures/figure_4_biodiversity_metrics_random_poly.png", plot = fig_4, bg = "transparent")

# Perform a one-sample t-test: compare DeLuca value to random polygon distribution
combined_values %>%
  group_by(Metric) %>%
  group_modify(~ {
    deluca_val <- .x %>% filter(Source == "DeLuca Bioblitz") %>% pull(Value)
    random_vals <- .x %>% filter(Source == "Random Polygons") %>% pull(Value)
    t_res <- t.test(random_vals, mu = deluca_val)
    tibble(
      Metric = unique(.x$Metric),
      DeLuca_Value = deluca_val,
      Random_Mean = mean(random_vals),
      Random_SD = sd(random_vals),
      t_statistic = t_res$statistic,
      df = t_res$parameter,
      p_value = t_res$p.value
    )
  }) %>%
  ungroup()

