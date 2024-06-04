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

# library(ggspatial)
# library(sf)

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


# write_csv(aba_coords, "Data/abalone_coordinates.csv")


# Load student data -------

data <- read_csv("Data/abalone_prelim2.csv") %>%
  rename(`2.5` = `<2.5cm`,
         `5` = `2.5-5.0cm`,
         `7.5` = `5.0-7.5cm`,
         `10` = `7.5-10.0cm`,
         `12.5` = `10.0-12.5cm`,
         `15` = `12.5-15.0cm`,
         `NA` = unsized) %>%
  mutate(buddy_pair = ifelse(buddy_pair == "GS_VK", "VK_GS", buddy_pair),
         method = case_when(
           survey_type == "quadrat" & survey_direction == "parallel" ~ "QPar",
           survey_type == "belt" & survey_direction == "parallel" ~ "BPar",
           survey_type == "quadrat" & survey_direction == "perpendicular" ~ "QPerp",
           survey_type == "belt" & survey_direction == "perpendicular" ~ "BPerp"))

long_data <- data %>%
  pivot_longer( cols = `2.5`:`NA`, names_to = "size_class", values_to = "total") %>%
  drop_na(total) %>%
  filter(total > 0) %>%
  mutate(size_class = as.numeric(size_class))

belt1 <- data %>%
  filter(survey_type == "belt") %>%
  filter(survey_complete == "yes") %>%
  rowwise() %>% 
  mutate(total = sum(c_across(`2.5`:`NA`), na.rm = T),
         density = total/transect_length_m) %>%
  ungroup() 

quad1 <- data %>%
  filter(survey_type == "quadrat") %>%
  filter(survey_complete == "yes") %>%
  rowwise() %>% 
  mutate(total = sum(c_across(`2.5`:`NA`), na.rm = T)) %>%
  ungroup() %>%
  group_by(site, surveyor, survey_direction, survey_type) %>%
  mutate(density = mean(total)) %>%
  ungroup()

quad <- quad1 %>%
  select(date, site, surveyor, buddy_pair, method, survey_direction, survey_type, transect_length_m, belt_start_depth_m, belt_end_depth_m, density) %>%
  unique()

belt <- belt1 %>%
  select(date, site, surveyor, buddy_pair, method, survey_direction, survey_type, transect_length_m, belt_start_depth_m, belt_end_depth_m, density) %>%
  unique()

trans <- rbind(quad, belt)

buddy <- trans %>%
  group_by(site, buddy_pair, survey_direction, survey_type) %>%
  mutate(trans_den = mean(density)) %>%
  select(-surveyor) %>%
  unique()

# 
ggplot(trans, aes(method, density)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, linewidth = 0.5) +
  facet_wrap(~site) +
  theme_bw() 

trans %>%
  filter(site != "ohiat") %>%
  filter(site != "goby_town")%>%
  filter(site != "pyramid_rock")%>%
  filter(site != "wizard_S") %>%
  ggplot(aes(surveyor, density)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = "mean", geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.4, linewidth = 1.5) +
  facet_wrap(~site)

# stats -----
mod <- glmmTMB(density ~ survey_direction*survey_type + (1|buddy_pair) + (1|site), 
               family = tweedie,
               data = buddy)

summary(mod)
plot(DHARMa::simulateResiduals(mod))

visreg(mod)


# size data ------
size <- read_csv("Data/aba_size_prelim.csv") %>%
  filter(surveyor != "SL")

ggplot(size, aes(length_cm, width_cm)) +
  geom_point(aes(colour = surveyor)) +
  geom_smooth(method = lm)

size_mod <- glmmTMB(length_cm ~ width_cm, 
                    data = size)
summary(size_mod)

plot(simulateResiduals(size_mod))
