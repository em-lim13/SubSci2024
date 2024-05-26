# Example code for Sub Sci
# Em Lim
# May 2024

# When you first open this, R should ask you to install a list of packages. 
# Please click yes!
# Renv is a package manager, and will make sure you have the same versions of packages that I'm using!
# Don't worry, this won't impact your other projects or R scripts. Renv will keep all the packages contained to this project
# The download will take a few minutes, just be patient and go look at the ocean for a while while you wait


# I wrote this script to analyze two experiments I ran to see if animals could increase the concentration of ammonium in cages. One experiment used sea cucumbers, and another used red rock crabs


# I've tried to add more annotations to this code, but if you're unfamiliar with linear modeling I strongly suggest checking out this fantastic tutorial written by my labmate:
# https://bayesbaes.github.io/2023/03/15/glmms.html

# I have "soft wrap" turned on, so R automatically starts a new line when my code hits the edge of my page
# If you don't have this on, you'll get some super long lines (sorry)
# To turn it on, go Tools > Global Options > Code > check "soft wrap R source files" > Apply !

# Load packages ----

# Package management
# Renv records snapshots of all the packages currently used in your repository and the versions you're using
library(renv) 
# make sure packages are correct
renv::restore() # this should make sure your packages are all consistent with this project, if it asks if you want to update the following packages, click yes

#renv::install("isoband@0.2.7")

renv::snapshot()

# Manipulate data
library(tidyverse) # for general data wrangling and plotting

# Analysis
# Loading TMB and glmmTMB will give you a warning. Just ignore this, the more updated version of Matrix and TMB don't run well on older computers so we're using these versions to avoid half the class being unable to run code
library(TMB) # needed for glmmTMB
library(glmmTMB) # The swiss army knife of modeling packages
library(DHARMa) # inspect model residuals/check assumptions
library(ggeffects) # for extracting predictions and running post hoc tests

# Visualize
library(ggplot2) # Plotting data
library(patchwork) # Arrange multiple plots together
library(visreg) # plot model predictions


# Load data ----

# You'll notice "%>%" in my code a lot! This is a pipe function: it takes each line of code and "pipes" it through the next set of functions. 
# This lets me load data into R, rename a column, and then change and add columns all in one chunk

# Sea cucumbers in cages
cuke_pee <- read_csv("Data/cuke_cages.csv") %>%
  # I don't like the name of this column, let's rename it!
  rename(nh4_avg = nh4_conc) %>%
  # Use mutate() to create new columns
  mutate(cukes = factor(cukes, levels = c("Control", "Mid", "High"),
                        labels = c("Control", "Medium", "Large")),
         depth_center = c(scale(depth, scale = FALSE))) %>% 
  # I want to make sure this is a dataframe
  as.data.frame()

# center depth: this subtracts the mean from each value (centering), but doesn't divide it by the standard deviation (scaling)
# Use c() on each scale() function to remove the extra attributes that the scale() function creates which cause issues with other packages

# Red rock crabs in cages
crab_pee <- read_csv("Data/crab_cage_pee.csv") %>%
  # I need to add a column for week, the first 12 rows are week one, and the next 11 rows are week two
  mutate(week = c(rep("one", 12), rep("two", 11)),
         treatment = factor(as.factor(treatment), 
                            levels = c("control", "mid", "large"),
                            labels = c("Control", "Medium", "Large"))) %>%
  # currently each pee measurement is an individual column, but I want those all in one column so I need to pivot!
  pivot_longer( cols = c(nh4_conc3, nh4_conc2, nh4_conc1), names_to = "measurement", values_to = "nh4_conc") %>%
  # new column for day of measurement
  mutate(day = c(rep(3:1, times = 12), rep(6:4, times = 11)))%>%
  as.data.frame()


# Cuke stats -----
# create a "simple" model
# nh4_avg is the response variable
# cukes = the categorical treatment variable, did a cage have no cukes, 1 cuke, or 2 cukes?
# depth_center = continuous variable for cage depth, centered
# centering this variable means we're making estimates about the sea cucumber treatment at the mean depth, instead of depth = 0.
mod_cu <- glmmTMB(nh4_avg ~ cukes + depth_center,
                  cuke_pee)

# check residuals
plot(simulateResiduals(mod_cu)) # we want the points on the left plot to roughly follow the red line, and the lines on the right plot should be black and fairly flat. This plot isn't the prettiest, but DHARMa isn't highlighting anything in red so it's ok

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
plot(simulateResiduals(mod_cr_gamma)) # this looks good!

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
  plot_annotation(theme = 
      theme(plot.background = element_rect(color = "white", fill = "white")))

# Save this figure
ggsave("Figures/Example_cage_plot.png", device = "png", height = 6, width = 12, dpi = 400)
