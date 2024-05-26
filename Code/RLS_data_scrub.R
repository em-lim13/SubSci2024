# Clean up the abalone data from RLS data
# Em Lim
# May 2024

# run this to install the packages!
renv::restore()

# This script takes the macroinvert data from the online RLS database, and adds the 2022 kelp forest RLS data from Kieran and Claire, and the 2024 RLS data that's not on the website yet and combines it
# Then I've saved just the abalone csv for use in other scripts

# Load packages and data -----
# Load packages
library(readr)
library(tidyr)
library(dplyr) # for general data wrangling
library(lubridate) # for looking at dates

# source file where my functions live
source("Code/Functions.R")

# Load data
# Load data from rls online database
rls2021_2023 <- read_csv("Data/Raw_RLS/RLS_macroinverts_2021-2023.csv") %>%
  mutate(survey_date = ymd(survey_date)) %>%
  filter(survey_date > "2020-01-01") %>%
  # select columns
  select(site_code, site_name, survey_date, latitude, longitude, depth, method, block, species_name, size_class, total)

# load 2024 RLS data
rls2024 <- mr_scrubby("Data/Raw_RLS/RLS_CANADA_2024.csv") # use function stored in Functions.R to load and manipulate this file

# Load KCCA data
rls_kelp <- mr_scrubby("Data/Raw_RLS/RLS_KCCA_2022.csv") # use function!

# put them alllll together
rls <- rbind(rls2021_2023, rls2024, rls_kelp) %>%
  # add blocks 1 and 2 together so each survey just has one count per sp per size
  group_by(site_code, survey_date, depth, method, species_name, size_class) %>%
  mutate(transect_total = sum(total)) %>% 
  ungroup() %>%
  select(-c(block, total)) %>% # remove block and the old total
  unique() %>% # only keep one row per species per size per survey
  separate(site_code, 
           into = c("survey_type", NA), 
           sep = "(?<=[A-Za-z])(?=[0-9])",
           remove = FALSE) %>%
  as.data.frame() %>%
  # roughly assign site types, careful!
  # BMSC sites are part of the annual spring blitz
  # KCCA sites are Kieran's kelp sites, but 3 were no kelp controls
  # BMKC were Bridget's deep kelp sites
  mutate(survey_type = case_when(
      site_code == "Raymond Kelp Rock" ~ "shallow kelp",
      site_code == "Sand Town" ~ "sand",
      site_code == "KCCA6" ~ "rocky reef",
      site_code == "KCCA19" ~ "rocky reef",
      survey_type == "BMSC" ~ "rocky reef",
      survey_type == "KCCA" ~ "shallow kelp",
      survey_type == "BMKC" ~ "deep kelp"),
     # add column for abalone density, strips were 50 x 2 m
     density = transect_total/100 
    )


# just abalone!
abalone <- rls %>%
  filter(species_name == "Haliotis kamtschatkana")
  

# save the abalone file
write_csv(abalone, "Data/abalone_RLS.csv")




