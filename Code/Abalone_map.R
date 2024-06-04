# Abalone map --------
# Em Lim
# May 2024

# Load packages -----
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(sf)
library(viridis)


# Load data ----
aba_coords <- read_csv("Data/abalone_coordinates.csv") %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) 

# Load GREAT shapefile ----
hakai_map <- sf::st_read("Data/Hakaii_coast/COAST_TEST2.shp") %>%
  st_sf() %>%
  st_set_crs(4326)

# honestly not 100% sure what this does
sf_use_s2(FALSE)

# Define colours
blue <- paste("#b9d1df", sep="")


# Making RLS maps -----
# make a map function
map_daddy <- function(lat_min, lat_max, long_min, long_max, 
                      coord_data, nh4_var, kelp_var, point_size, map_file, 
                      white_background = TRUE, invert = FALSE) {
  
  # invert land and sea colours if I use the potato map
  if(invert == FALSE){
    sea <- blue
    land <- "white"
  }
  if(invert == TRUE){
    sea <- "white"
    land <- blue
  }
  
  # specify black or white background
  if(white_background == TRUE){
    background <- "white"
    features <- "black"
  }
  if(white_background == FALSE){
    background <- "black"
    features <- "white"
  }
  
  ggplot() +
    geom_sf(data = map_file, fill = land, colour = sea) +
    # add points
    geom_sf(data = coord_data, 
            colour = "black",
            alpha = 0.9,
            size = point_size,
            aes(fill = {{nh4_var}},
                pch = {{kelp_var}})) +
    viridis::scale_fill_viridis(option="magma", direction = -1,
                                limits = c(0, 2),
                                guide = guide_colorbar(frame.colour = features, ticks.colour = features)) +
    coord_sf(xlim = c(lat_min, lat_max), ylim = c(long_min, long_max), expand = FALSE)  +
    labs(fill = expression(paste("NH"[4]^" +",(mu*M)))) +
    scale_shape_manual(values = c(21, 25), drop = F) +
    guides(pch = guide_legend(override.aes = 
                                list(colour = features))) +
    # Themes
    theme_bw() +
    theme(
      # panel stuff
      panel.background = element_rect(fill = sea),
      panel.grid.major = element_line(color = sea),
      panel.border = element_rect(fill = NA, colour = features),
      # remove axis
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      plot.title = NULL,
      plot.margin=grid::unit(c(0,0,0,0), "mm"),
      # Specify legend options
      legend.background = element_rect(color = NA, fill = background),  
      legend.key = element_rect(color = background,  fill = background),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = 24, color = features),  
      legend.title = element_text(size = 24, face = "bold", hjust = 0, color = features),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL,
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = 30, color = features),  
      strip.text.y = element_text(size = 30, color = features,angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = background, fill = background),  
    ) +
    annotation_scale(location = "br", width_hint = 0.4) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering)
}


# Make a map ----
map_daddy(lat_min = -125.4,
          lat_max = -125.0, 
          long_min = 48.75, 
          long_max = 49, 
          coord_data = aba_coords, 
          nh4_var = mean_density, 
          point_size = 4, 
          kelp_var = survey_type,
          map_file = hakai_map) 