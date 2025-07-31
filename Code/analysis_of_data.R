## A script for some analysis of the DeLuca bioblitz data


# packages
library(tidyverse)
library(ggplot2)
library(readr)

# read in data
random_polygon_effort <- read_csv("Data/Summarized_Data/random_polygon_effort.csv")

regional_species_counts <- read_csv("Data/Summarized_Data/regional_species_counts.csv")

distance_to_nearest_obs <- read_csv("Data/Summarized_Data/distance_to_nearest_obs.csv")

# Read in the bioblitz data
deluca_bioblitz <- read_csv("Data/DeLuca_iNaturalist_Data/deluca_bioblitz_obs.csv")

# analysis of bioblitz data
length(unique(deluca_bioblitz$observed_on))

unique(deluca_bioblitz$observed_on)

# make a histogram of all species observed
species_hist_all <- deluca_bioblitz %>%
  dplyr::select(taxon_species_name, quality_grade) %>%
  dplyr::filter(complete.cases(taxon_species_name)) %>%
  group_by(taxon_species_name) %>%
  summarize(N=n())

ggplot(species_hist_all, aes(x=N))+
  geom_histogram(color="black", fill="gray80")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Number of observations")+
  ylab("Number of species (RG + Needs ID)")

# make a histogram of RG species observed only
species_hist_RG <- deluca_bioblitz %>%
  dplyr::select(taxon_species_name, quality_grade) %>%
  dplyr::filter(quality_grade=="research") %>%
  dplyr::filter(complete.cases(taxon_species_name)) %>%
  group_by(taxon_species_name) %>%
  summarize(N=n())

ggplot(species_hist_RG, aes(x=N))+
  geom_histogram(color="black", fill="gray80")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Number of observations")+
  ylab("Number of species (RG)")

# plot an accumulation type plot of new species
first_obs_species_all <- deluca_bioblitz %>%
  dplyr::select(taxon_genus_name, taxon_species_name, observed_on, observed_on, quality_grade) %>%
  dplyr::filter(complete.cases(taxon_species_name)) %>%
  group_by(taxon_species_name, observed_on) %>%
  arrange(desc(observed_on)) %>%
  distinct() %>%
  group_by(observed_on) %>%
  summarise(new_species = n_distinct(taxon_species_name), .groups = "drop") %>%
  mutate(cumulative_species = cumsum(new_species))
  
ggplot(first_obs_species_all, aes(x = observed_on))+
  geom_col(aes(y = new_species), fill = "steelblue", alpha = 0.6, width=4)+
  geom_line(aes(y = cumulative_species), color = "darkgreen", size = 1.2)+
  geom_point(aes(y = cumulative_species), size = 2, color = "darkgreen")+
  xlab("Sampling Date")+
  ylab("Species count (RG + Needs ID)")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))


# plot an accumulation type plot of new species
# now for RG only!
first_obs_species_RG <- deluca_bioblitz %>%
  dplyr::select(taxon_genus_name, taxon_species_name, observed_on, observed_on, quality_grade) %>%
  dplyr::filter(quality_grade=="research") %>%
  dplyr::filter(complete.cases(taxon_species_name)) %>%
  group_by(taxon_species_name, observed_on) %>%
  arrange(desc(observed_on)) %>%
  distinct() %>%
  group_by(observed_on) %>%
  summarise(new_species = n_distinct(taxon_species_name), .groups = "drop") %>%
  mutate(cumulative_species = cumsum(new_species))

ggplot(first_obs_species_RG, aes(x = observed_on))+
  geom_col(aes(y = new_species), fill = "steelblue", alpha = 0.6, width=4)+
  geom_line(aes(y = cumulative_species), color = "darkgreen", size = 1.2)+
  geom_point(aes(y = cumulative_species), size = 2, color = "darkgreen")+
  xlab("Sampling Date")+
  ylab("Species count (RG)")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))


########## let's do some context of how DeLuca 'performs' to 100 random polygons

# first get the values for the DeLuca
deluca_values <- data.frame(number_of_observations=nrow(deluca_bioblitz),
                            number_of_observers=length(unique(deluca_bioblitz$user_id)),
                            number_of_species=deluca_bioblitz %>%
                              dplyr::filter(quality_grade=="research") %>%
                              dplyr::select(taxon_species_name) %>%
                              dplyr::filter(complete.cases(taxon_species_name)) %>%
                              distinct() %>%
                              nrow(.)) %>%
  pivot_longer(cols = everything()) 

random_polygon_effort %>%
  dplyr::select(polygon_id, number_of_observations, number_of_observers, number_of_species) %>%
  pivot_longer(cols=c("number_of_observations", "number_of_observers", "number_of_species")) %>%
  ggplot(., aes(x=value, y=name))+
  geom_violin()+
  geom_point(data = deluca_values, aes(x = value, y = name),
             color = "red", size = 3)+
  facet_wrap(~name, scales="free")+
  scale_x_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y=element_blank())+
  theme(axis.title.y=element_blank())


# now think about valuable observations (in terms of rarity)
# Compute uniqueness indicators
records_value_summary <- regional_species_counts %>%
  mutate(
    only_county = `Osceola County` == deluca_bioblitz,  # All county records from bioblitz
    only_state  = Florida == deluca_bioblitz,  # All state records from bioblitz
    only_world  = All == deluca_bioblitz       # Extreme: world unique (unlikely)
  )


summary_metrics <- records_value_summary %>%
  summarise(
    total_species = n(),
    county_unique_species = sum(only_county),
    pct_county_unique = mean(only_county) * 100,
    state_unique_species = sum(only_state),
    pct_state_unique = mean(only_state) * 100,
    world_unique_species = sum(only_world)
  )

summary_metrics

df_long <- records_value_summary %>%
  dplyr::select(Species, `Osceola County`, Florida, All, deluca_bioblitz) %>%
  pivot_longer(cols = c("Osceola County","Florida","All"),
               names_to = "Region", values_to = "TotalObs") %>%
  mutate(PropFromBioblitz = deluca_bioblitz / TotalObs)

ggplot(df_long, aes(x = Region, y = PropFromBioblitz)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "% of Observations from Bioblitz",
       x = "Scale",
       title = "Relative Contribution of DeLuca Bioblitz to Biodiversity Data") +
  theme_minimal()


records_value_summary %>%
  mutate(rarity_class = case_when(
    `Osceola County` <= 5 ~ "Very rare in county (≤5)",
    `Osceola County` <= 20 ~ "Rare (6–20)",
    TRUE ~ "Common (>20)"
  )) %>%
  count(rarity_class) %>%
  ggplot(aes(x = rarity_class, y = n, fill = rarity_class)) +
  geom_col(show.legend = FALSE) +
  labs(x = "County Rarity Class",
       y = "Number of Species Documented",
       title = "Bioblitz Captures Many Rare Species") +
  theme_minimal() +
  coord_flip()
