# Getting started
# Em Lim
# May 2024

# Load packages -----
# R packages contain useful functions to manipulate, analyze, and visualize data

# Package control
library(renv)
renv::init()
renv::snapshot()

# Manipulate data
library(tidyverse)
library(dplyr) # Data manipulation
library(lubridate) # dates

# Analysis
library(TMB) # needed for glmmTMB
library(glmmTMB) # The swiss army knife of modeling packages
library(DHARMa) # inspect model residuals/check assumptions
library(emmeans) # extract estimated marginal means
library(ggeffects) # effect sizes

# Visualize
library(ggplot2) # Plotting data
library(patchwork) # Arrange multiple plots together
library(visreg) # plot model predictions

# Renv is a package management package. It records snapshots of all the packages currently used in your repository and the versions you're using
# Renv should have prompted you to install all the packages I'm using, and make sure you have the same versions


# Load data ----
cup <- read_csv("Data/2022_06_06_anemones.csv") %>%
  rename(diam_cm = LargestCoralDiameter_cm,
         density = Density_num_m2) %>%
  mutate(depth_zone = ifelse(Depth_m < 6, "5 m", "9 m"),
         date = ymd("2022_06_06"))


# Plot data ----
ggplot(data = cup, aes(diam_cm, density, colour = depth_zone)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = lm) +
  labs(x = "Diameter (cm)", y = "Density", colour = "Depth")

# ggsave("Output/Cup_corals.png", device = "png", height = 9, width = 16, dpi = 400)

# Stats ----

# Build a model 
cup_model <- glmmTMB(density ~ diam_cm * Depth_m + (1|Group), 
                     data = cup)

# inspect model residuals
plot(DHARMa::simulateResiduals(cup_model))

# check model output!
summary(cup_model)

# visualize the model  
visreg(cup_model, "diam_cm", by = "Depth_m", overlay = "TRUE")

# use ggpredict to get estimates for the cup model
predict_cups <- ggpredict(cup_model, terms = c("diam_cm")) %>% 
  dplyr::rename(diam_cm = x,
                density = predicted) %>% 
  as_tibble()

# plot model predictions against data
ggplot() +
  geom_jitter(data = cup, 
              aes(x = diam_cm,
                  y = density)) +
  geom_line(data = predict_cups,
            aes(x = diam_cm, 
                y = density),
            linewidth = 2) +
  geom_ribbon(data = predict_cups,
              aes(x = diam_cm, 
                  y = density,
                  ymin = conf.low, ymax = conf.high), 
              alpha = 0.15) 
