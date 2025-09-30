#############################################################################
# BioBlitz in the Literature
# May 12, 2023
# Brittany Mason
#############################################################################

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(httr)
library(tidyverse)
library(jsonlite)
library(data.table)

#read in google trends data and web of science data
googletrends <- read_csv("Data/GoogleTrends_2100.csv")
wos <- read.table("Data/Web_of_Science.txt", sep="\t", header=TRUE)
inat <- read_csv("Data/inaturalist.csv")
googlescholar <- read_csv("Data/googlescholar_2010.csv")
googlescholarinat <- read_csv("Data/googlescholar_inat_bioblitz.csv")

#define the date column in the google trends data table and then create a new column with year
googletrends$Month <- ym(googletrends$Month)
googletrends$Year <- year(googletrends$Month)

#summarise data by year
googletrends <- googletrends %>% 
  group_by(Year) %>%
  dplyr::summarise(n=sum(n)) %>%
  dplyr::mutate("source"="'BioBlitz' Term Popularity - Google Trends")

#we want the sum to be a percentage
googletrends$percent <- (googletrends$n/sum(googletrends$n))*100

#remove years prior to 2004 from web of science data so the time frames match the data
wos <- wos %>%
  filter(Publication.Years >= 2010, Publication.Years < 2023) %>%
  dplyr::rename("Year"="Publication.Years", 
                "n" = "Record.Count",
                "percent"="X..of.127") %>%
  dplyr::mutate("source"="Web of Science 'BioBlitz' Search Results")

#get percent events from inaturalist
inat$source <- "iNaturalist 'bioblitz' Projects"
inat$percent <- (inat$n/sum(inat$n))*100

#get percent events form google scholar
googlescholar$source <- "Google Scholar 'BioBlitz' Search Results"
googlescholar$percent <- ((googlescholar$n)/sum(googlescholar$n))*100

#do the same for the google scholar inaturalist dataframe
googlescholarinat$source <- "Google Scholar 'bioblitz iNaturalist' Search Results"
googlescholarinat$percent <- ((googlescholar$n)/sum(googlescholar$n))*100

bioblitz <- rbind(googletrends, inat, googlescholar)

ggplot(bioblitz, aes(x=Year, y=percent, group=source)) +
  geom_line(aes(color=source), size=1) + theme_classic() +
  scale_x_continuous(breaks=seq(2010, 2022, 2), expand=c(0,0)) +
  theme(legend.title=element_blank(),
        legend.position=c(0.25,0.9),
        axis.title.x = element_blank()) +
  ylab("Percentage")

bioblitz_n <- rbind(inat, googlescholarinat)
names(bioblitz_n)[names(bioblitz_n) == 'source'] <- 'Number of'

plot <- ggplot(bioblitz_n, aes(x=Year, y=n, fill=`Number of`, color=`Number of`)) +
  geom_line(lwd=3) + theme_classic() + ylab("Count") +
  scale_x_continuous(breaks=seq(2010, 2022, 2)) +
  scale_y_continuous(expand=c(0,0), trans='log10', n.breaks=6) +
  scale_color_manual(values=c("darkseagreen3", "darksalmon")) +
  theme(legend.position=c(0.37,0.93),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        text = element_text(size = 60, color="black"))

ggsave("Figures/iNaturalist_BioBlitz2.jpg", plot=plot, width=24, height=16)




