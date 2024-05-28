# Sea star assemblages over time
# Em Lim
# June 4, 2022, updated May 2024

# Load packages -----

# data wrangling
library(readr)
library(tidyr)
library(dplyr)

# visualising
library(ggplot2) # Graphing package
library(visreg)

# modelling
library(glmmTMB) # Mixed effects linear models
library(DHARMa)
library(emmeans)

# Load data
echino <- read_csv("Data/2024_05_27_star_data.csv") %>%
  mutate(density = total_transect/belt_area)

# just sea stars
stars <- echino %>%
  filter(species != "red urchin") %>%
  filter(species != "purple urchin") %>%
  filter(species != "green urchin")

# Which sp are the most abundant?
stars_ranked <- echino %>%
  group_by(species) %>% 
  summarise(density = sum(density))  %>%
  arrange(desc(density)) %>%
  head(8)

# just keep those top 8 most abundant species
abundant_stars <- echino %>%
  filter(species %in% stars_ranked$species)


# Let's just look at sun stars
sun_stars <- echino %>%
  filter(species %in% c("sunflower star", "striped sunflower star", "morning star"))

# Plot the most abundant stars ???
ggplot(data = abundant_stars, aes(year, density, colour = species)) +
  geom_jitter(size = 2.8) +
  geom_smooth(linewidth = 1.6, method = lm) +
  theme_classic() +
  labs(x = "Year", y = "Density", colour = "Species") 

ggsave("Figures/Star_trend.png", device = "png", height = 9, width = 16, dpi = 400)

# can we model stars?
star_mod <- glmmTMB(density ~ species*year, 
                    family = tweedie,
                    data = abundant_stars)
summary(star_mod)
plot(simulateResiduals(star_mod)) # waaay better with tweedie

star_mean <- emmeans::emtrends(star_mod, pairwise ~ species, var = "year")$emtrends %>%
  as.data.frame()

# look at those slopes
ggplot(star_mean, aes(x = year.trend, y = reorder(species, year.trend), xmin = asymp.LCL, xmax = asymp.UCL)) +
  geom_point(size = 2.7) +
  geom_errorbar(width = 0, linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_classic() +
  labs(x = "Slope", y = "Species")


# Just sun stars
ggplot(data = sun_stars, aes(year, density, colour = species)) +
  geom_jitter(size = 2.8) +
  geom_smooth(linewidth = 1.6, method = lm) +
  theme_classic() +
  labs(x = "Year", y = "Density", colour = "Species")

ggsave("Output/Sunstar_trend.png", device = "png", height = 9, width = 16, dpi = 400)

# can we model sun stars?
sun_star_mod <- glmmTMB(density ~ species*year, 
                        family = tweedie,
                    data = sun_stars)
summary(sun_star_mod)
plot(simulateResiduals(sun_star_mod)) # ooof