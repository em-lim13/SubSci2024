# Sea star assemblages over time
# Em Lim
# June 4, 2022, updated May 2024

# Load packages -----
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2) # Graphing package
library(visreg)
library(glmmTMB) # Mixed effects linear models
#source("Code/themes_code.R")

# Load data
echino <- read_csv("Data/2024_05_27_star_data.csv") %>%
  mutate(density = total_transect/belt_area)

stars <- echino %>%
  filter(species != "red urchin") %>%
  filter(species != "purple urchin") %>%
  filter(species != "green urchin")

# Let's just look at sun stars
sun_stars <- echino %>%
  filter(species %in% c("sunflower star", "striped sunflower star", "morning star"))

# Which echinos are the most abundant?
stars_ranked <- stars %>%
  group_by(species) %>% 
  summarise(density = sum(density))  %>%
  arrange(desc(density)) %>%
  head(10)

# just keep those top 12 most abundant species
abundant_stars <- stars %>%
  filter(species %in% stars_ranked$species)

no_zeros <- stars %>%
  filter(density != 0)

# Plot the most abundant stars ???
ggplot(data = abundant_stars, aes(year, density, colour = species)) +
  geom_jitter(size = 2.8) +
  geom_smooth(linewidth = 1.6, method = lm) +
  theme_classic() +
  labs(x = "Year", y = "Density", colour = "Species") 

ggsave("Figures/Star_trend.png", device = "png", height = 9, width = 16, dpi = 400)

# Just sun stars
ggplot(data = sun_stars, aes(year, density, colour = species)) +
  geom_jitter(size = 2.8) +
  geom_smooth(linewidth = 1.6, method = lm) +
  theme_classic() +
  labs(x = "Year", y = "Density", colour = "Species")

ggsave("Output/Sunstar_trend.png", device = "png", height = 9, width = 16, dpi = 400)

# Just non zeros?
ggplot(data = no_zeros, aes(year, density, colour = species)) +
  geom_jitter() +
  geom_smooth(alpha = 0.2, method = lm) +
  theme_classic() +
  xlim(c(2014, 2024)) +
  labs(x = "Year", y = "Density", colour = "Species")

ggsave("Output/Star_trend.png", device = "png", height = 9, width = 16, dpi = 400)


# just urchins
m <- glmmTMB(belt_area ~ substrate,
             family = Gamma(link = 'log'),
                            data = echino)
summary(m)

