# Play with abalone data
# Em Lim
# May 2024

# I cleaned up the abalone data from the RLS data in the "RLS_data_scrub.R" file and saved the clean csv "abalone_RLS.csv"

# this script is the beginning of an exploration of that data

# run this to install the packages!
renv::restore()

# Load packages and data -----
# Load packages
library(readr)
library(tidyr)
library(dplyr) # for general data wrangling
library(lubridate) # for looking at dates

library(glmmTMB) # The swiss army knife of modeling packages
library(DHARMa) # inspect model residuals/check assumptions
library(ggeffects) # for extracting predictions and running post hoc tests

library(ggplot2) # Plotting data
library(patchwork) # Arrange multiple plots together
library(visreg) # plot model predictions
library(viridisLite) # colour palettes

library(ggspatial)
library(sf)

# source file where my functions live
source("Code/Functions.R")

# Load abalone data 
abalone <- read_csv("Data/abalone_RLS.csv") %>%
  mutate(site_code = ifelse(site_code == "BMSC28", "BMSC24", site_code),
         latitude = ifelse(site_code == "BMSC24", "48.91607", latitude),
         longitude = ifelse(site_code == "BMSC24", "-125.1312", longitude))


# Where are abalone? -----

# this gives you a dataframe of the coordinates of each site and an average abalone density at each of them in case anyone wants to map this. careful bc some sites had multiple transects, some deep and some shallow and this will just average across those
aba_coords <- abalone %>%
  group_by(site_code) %>%
  mutate(mean_density = mean(density),
         mean_total = mean(transect_total),
         ) %>%
  ungroup() %>%
  select(site_code, site_name, survey_type, latitude, longitude, mean_total, mean_density) %>%
  unique() 
  

write_csv(aba_coords, "Data/abalone_coordinates.csv")


# Mapping -----
# load map

a_rank <- aba_coords %>%
  filter(site_name != "Baeria Rocks North Island Southside") %>%
  filter(site_name != "Eussen Rock") %>%
  filter(site_name != "Effingham Archipelago") %>%
  filter(site_name != "Baeria Rocks North Island Northside") %>%
  filter(site_name != "Faber Islets") %>%
  filter(site_name != "Baeria Rocks South Island") %>%
  filter(site_name != "Effingham West") %>%
  arrange(desc(mean_density)) %>%
  head(12)
  
# one row for each abalone
a <- abalone %>%
  uncount(transect_total)
  
most_aba <- a %>%
  filter(site_code %in% a_rank$site_code) 

idk <- abalone %>%
  group_by(site_name, depth) %>% 
  summarise(mean_den = mean(density))


hist(a$depth)

# do we want to look at the sites?
# maybe not bc this is more of a hist of where we did transects...
most_aba %>%
  ggplot(aes(x=depth, color=site_name, fill=site_name)) +
  geom_histogram(alpha=0.5, binwidth = 0.5, position = 'identity') +
#  scale_fill_viridis(discrete=TRUE) +
#  scale_color_viridis(discrete=TRUE) +
  theme_classic() +
  theme(
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)")
