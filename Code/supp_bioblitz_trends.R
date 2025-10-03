#############################################################################
# BioBlitz in the Literature
# May 12, 2023
# Brittany Mason
#############################################################################

library(tidyverse)

#read in google trends data and web of science data
bioblitz_trends <- read_csv("Data/Supplemental_Analysis/bioblitz_trends.csv")

# Find scaling factor to align ranges
scale_factor <- max(google$n, na.rm=TRUE) / max(inat$n, na.rm=TRUE)

ggplot() +
  geom_line(data = google, aes(x = Year, y = n, color = "Google Scholar"), size = 1) +
  geom_line(data = inat, aes(x = Year, y = n * scale_factor, color = "iNaturalist projects"), size = 1) +
  scale_y_continuous(
    name = "Number of Articles on Google Scholar",
    sec.axis = sec_axis(~./scale_factor, name = "Number of iNaturalist Projects")
  ) +
  scale_color_manual(values = c("Google Scholar" = "#4575b4",    # muted blue
                                "iNaturalist projects" = "#f46d43")) + # muted orange
  scale_x_continuous(breaks = seq(2010, 2022, 2), expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.title.y.left = element_text(color = "#4575b4", face="bold"),
    axis.title.y.right = element_text(color = "#f46d43", face="bold"),
    legend.position = "bottom"
  ) +
  labs(x = "Year", color = "Source")

ggsave("Figures/supp_googlescholar_bioblitz.jpg", width=5, height=4)




