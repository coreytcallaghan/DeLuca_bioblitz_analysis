# Data

**DeLuca_iNaturalist_Data:** This is all the iNaturalist data for the DeLuca Preserve (deluca_obs.csv) and deluca_bioblitz_obs.csv). This data was downloaded using the data export tool on iNaturalist on July 1, 2025. We chose to use the iNaturalist data export tool instead of GBIF because it allows us to filter by iNaturalist project, which is not possible in GBIF.

**Shapefile:** This folder contains a shapefile for the DeLuca Preserve. The boundary of the DeLuca Preserve changed slightly in 2025 compared to the years 2022-2024. This shapefile represents the most recent boundary, which was used for the 2025 bioblitz.

**Summarized_Data:** This folder contains data summaries to answer the research questions posted in the manuscript. The data is as follows:

-   **distance_to_nearest_obs:** For all species that had less than 100 research grade iNaturalist observations during the DeLuca bioblitzes, we calculated the distance to the nearest observation of that species excluding observations taken within the DeLuca Preserve shapefile. The distance is presented as meters.

-   **random_polygon_effort:** For this data set, we randomly generated 100 rectangles in the state of Florida which matched the size as the DeLuca Preserve shapefile. Next, we used the iNaturalist API to obtain the total number of observations, observers, and species within that rectangle. This data will be used to compare cumulative effort.
