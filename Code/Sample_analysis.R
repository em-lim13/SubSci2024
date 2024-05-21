# Example code for Sub Sci
# Em Lim
# Oct 17, 2023

# I wrote this script to analyze two experiments I ran to see if animals could increase the concentration of ammonium in cages. One experiment used sea cucumbers, and another used red rock crabs

# Load packages ----

# Package management
library(renv) # this package takes a snapshot of all the packages I'm using and will help you install those exact package versions

# Manipulate data
library(tidyverse) # Umbrella package contains lots of good stuff including the following
library(dplyr) # Data manipulation
library(lubridate) # dates

# Analysis
library(TMB) # needed for glmmTMB
library(glmmTMB) # The swiss army knife of modeling packages
library(DHARMa) # inspect model residuals/check assumptions
library(ggeffects) # effect sizes

# Visualize
library(ggplot2) # Plotting data
library(patchwork) # Arrange multiple plots together
library(visreg) # plot model predictions
library(png) # add images to plots
library(ggtext) # add text to plots


# Load data ----

# Sea cucumbers in cages
cuke_pee <- read_csv("Data/cuke_cages.csv") %>%
  rename(nh4_avg = nh4_conc) %>%
  mutate(cukes = factor(cukes, levels = c("Control", "Mid", "High"),
                        labels = c("Control", "Medium", "Large")),
         depth_center = c(scale(depth, scale = FALSE))) %>% # center depth
  as.data.frame()

# Red rock crabs in cages
crab_pee <- read_csv("Data/crab_cage_pee.csv") %>%
  mutate(week = c(rep("one", 12), rep("two", 11)),
         treatment = factor(as.factor(treatment), 
                            levels = c("control", "mid", "large"),
                            labels = c("Control", "Medium", "Large"))) %>%
  pivot_longer( cols = c(nh4_conc3, nh4_conc2, nh4_conc1), names_to = "measurement", values_to = "nh4_conc") %>%
  mutate(day = c(rep(3:1, times = 12), rep(6:4, times = 11)))%>%
  as.data.frame()


# Cuke stats -----
# create a "simple" model
# cukes = the treatment variable, did a cage have no cukes, 1 cuke, or 2 cukes?
# depth_center = continuous variable for cage depth, centred
mod_cu <- glmmTMB(nh4_avg ~ cukes + depth_center,
                  cuke_pee)

# check residuals
plot(simulateResiduals(mod_cu)) 

# look at model output
summary(mod_cu) 
# the intercept value is the control cage
# cukesMedium is the difference between control cage and medium cukes
# cukesLarge is the difference between control cage and large cukes
# depth_center is the effect of depth (as a continuous variable)
# we see no effect of sea cucumber treatment, but a positive effect of depth. Deeper cages had higher concentrations of ammonium

# visualize the model output
visreg(mod_cu)

# Crab stats ----
# this model is a bit more fancy, we're using a gamma distribution instead of a normal distribution and we've included a random for the week, because this experiment was run twice; once in week 1, and once in week 2

mod_cr_gamma <- glmmTMB(nh4_conc ~ treatment + (1|week), 
                        family = Gamma(link = "log"),
                        crab_pee)
# check resids
plot(simulateResiduals(mod_cr_gamma)) # this looks fine!

# model output
summary(mod_cr_gamma)
# cages containing medium and large crabs had higher concentrations of ammonium vs control cages

# visualize the model output
visreg(mod_cr_gamma)


# Graphing -------

# Create a nice palette
pal_cu <- viridisLite::viridis(6) # create a 6 colour palette
pal_c <- c(pal_cu[1], pal_cu[3], pal_cu[5]) # just pick three colours from that

# use ggpredict to get estimates for the cuke model
sum_cukes <- ggpredict(mod_cu, terms = "cukes") %>% 
  dplyr::rename(cukes = x,
                nh4_avg = predicted) %>% 
  as_tibble()

# use ggpredict to get estimates for crab model
sum_crabs <- ggpredict(mod_cr_gamma, terms = "treatment") %>% 
  dplyr::rename(treatment = x,
                nh4_avg = predicted) %>% 
  as_tibble()

# Make the plots
# plot cukes
cuke_plot <- ggplot() +
  geom_point(data = sum_cukes,
             aes(x = cukes, y = nh4_avg, colour = cukes),
             size = 8) +
  geom_errorbar(data = sum_cukes,
                aes(x = cukes, y = nh4_avg, colour = cukes,
                    ymin = conf.low,
                    ymax = conf.high),
                width = 0.4,
                linewidth = 1.5) +
  geom_jitter(data = cuke_pee, 
              aes(x = cukes, y = nh4_avg, colour = cukes), 
              size = 5, alpha = 0.5, height=0, width = 0.3) +
  theme_bw() + 
  theme(legend.position = "none",
        plot.title = element_text(size = 30)) +
  scale_colour_manual(values = rev(pal_c)) +
  labs(y = expression(paste("Ammonium"~(mu*M))), x = "Treatment") +
  ylim(c(0, 3.8)) 

# plot crabs
crab_plot <- ggplot() +
  geom_point(data = sum_crabs,
             aes(x = treatment, y = nh4_avg, colour = treatment),
             size = 8) +
  geom_errorbar(data = sum_crabs,
                aes(x = treatment, y = nh4_avg, colour = treatment,
                    ymin = conf.low,
                    ymax = conf.high),
                width = 0.4,
                linewidth = 1.5) +
  geom_jitter(data = crab_pee, 
              aes(x = treatment, y = nh4_avg, colour = treatment), 
              size = 5, alpha = 0.5, height=0, width = 0.3) +
  theme_bw() + 
  theme(legend.position = "none",
        plot.title = element_text(size = 30)) +
  scale_colour_manual(values = rev(pal_c)) +
  labs(y = expression(paste("Ammonium"~(mu*M))), x = "Treatment") +
  ylim(c(0, 3.8)) 

# plot together
cuke_plot + crab_plot & 
  plot_annotation(theme = theme(plot.background = 
                                  element_rect(color = "white", fill = "white")))

# Save this figure
ggsave("Figures/Example_cage_plot.png", device = "png", height = 6, width = 12, dpi = 400)
