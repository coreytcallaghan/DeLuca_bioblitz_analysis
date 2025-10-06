library(httr)
library(jsonlite)
library(dplyr)

# Base URL
base_url <- "https://api.inaturalist.org/v1/observations/species_counts"

# Parameters
params <- list(
  place_id = 21,
  include_ancestors = "false",
  per_page = 200, 
  page = 1
)

# loop to get observations
all_data <- list()
page <- 1
repeat {
  # update page number
  params$page <- page
  
  # request data
  res <- GET(base_url, query = params)
  
  # check for error
  stop_for_status(res)
  
  # parse JSON
  dat <- content(res, as = "parsed", simplifyVector = TRUE)
  
  # store results
  all_data[[page]] <- dat$results
  
  # stop if no more results
  if(length(dat$results) == 0) break
  
  page <- page + 1
  Sys.sleep(0.5)  # polite pause to avoid rate-limiting
}

# combine into a data frame
species_counts <- bind_rows(all_data)

# look at first rows
head(species_counts)

# simplify data 
simple_data <- data.frame(rank=species_counts$taxon$rank, 
                          name=species_counts$taxon$name,
                          establishment_means=species_counts$taxon$establishment_means$establishment_means)

write_csv(simple_data, "species_status_florida.csv")
