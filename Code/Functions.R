# Functions 

# load packages
library(readr)
library(tidyr)
library(dplyr)
library(stringr)

# Clean rls data! -----
mr_scrubby <- function(filepath) {
  df <- read_csv(filepath) %>%
    filter(Method != 0) %>% # get rid of all method 0's
    slice(2:n()) %>% # cuts the first blank row
    # rename columns
    rename(
      site_code = `Site No.`,
      site_name = `Site Name`, 
      common_name = `Common name`,
      `0` = Inverts,
      species_name = Species,
      method = Method,
      block = Block,
      latitude = Latitude,
      longitude = Longitude,
      survey_date = Date,
      depth = Depth
    )  %>% 
    # Rename columns with spaces
    mutate(species_name = str_to_sentence(species_name),
           common_name = str_to_sentence(common_name),
           survey_date = dmy(survey_date),
           date_time_survey = ymd_hms(paste(survey_date, Time))) %>%
    # Pivot longer
    pivot_longer( cols = `0`:`400`, names_to = "size_class", values_to = "total") %>%
    drop_na(total) %>%
    filter(total > 0) %>%
    select(-Total) %>%
    # select columns
    select(site_code, site_name, survey_date, latitude, longitude, depth, method, block, species_name, size_class, total)
}
