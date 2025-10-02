# load packages
library(tigris)
library(sf)
library(ggplot2)
library(dplyr)

#####################
### Figure of Florida + Osceola County
#####################

# Load all Florida counties
fl_counties <- counties(state = "FL", cb = TRUE, class = "sf")

# Identify Osceola County
highlight_counties <- fl_counties %>%
  filter(NAME %in% c("Osceola"))

# Create a new column for fill color
fl_counties <- fl_counties %>%
  mutate(fill_color = ifelse(NAME %in% c("Osceola"), "darkred", "white"))

# Plot
ggplot() +
  geom_sf(data = fl_counties, aes(fill = fill_color), color = "black", size = 0.3) +
  scale_fill_identity() +  
  theme_void() +
  theme(
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

# Save as PNG
ggsave(filename = "Figures/osceola_county_map.png", width = 8, height = 6, dpi = 300)
