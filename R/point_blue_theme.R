# Load in necessary libraries
library(tidyverse)
library(terra)
library(tidyterra)
library(here)
library(tmap)
library(janitor)
library(sf)
library(stars)
library(paletteer)
library(patchwork)
library(showtext)

font_add_google("Libre Franklin", family = "lib_frank")

point_blue <- "#4495D1"
point_light_blue1 <- "#8EBEE2"
point_light_blue2 <- "#D4E6F4"  
point_green <- "#74B743"
camel <- "#A98858"
gold <- "#FEDD63"
orange <- "#F7A54B"
charcoal <- "#595959"
rose <- "#B5727A"
bubblegum <- "#FF72DE"
maur_pauple <- "#762A83"

CONIFER <- "#74B162"
HARDWOOD <- "#A98858"
HERBACEOUS <- "#FEDD63"
SHRUB <- "#F7A54B"
URBAN <- "#595959"
WATER <- "#8EBEE2"
AGRICULTURE <- "#B5727A"
'BARREN/OTHER' <- "#FF72DE"

point_blue_theme <- function() {
  # Base theme light
  theme_light() +
    
    # Legend and text styling
    theme(
      # Put legend on the bottom of the plot
      
      legend.position = "bottom",
      
      #plot.title.position = "plot",
      
      # Bold title in libre franklin
      plot.title = element_text(
        face = "bold",
        family = "lib_frank",
        # Set standard size
        size = 27,
        # Add spacing between plot and title
        lineheight = 1.5,
      ),
      
      # Style subtitle
      plot.subtitle = element_text(
        family = "lib_frank",
        size = 20,
        margin = margin(b = 8),
      ),
      
      # Style axes text
      axis.title.y = element_text(
        size = 20,
        family = "lib_frank",
        margin = margin(r = 8, unit = "pt")
      ),
      axis.text.x = element_text(size = 15, family = "lib_frank"),
      
      # Add margin space below
      axis.text.y = element_text(
        size = 15,
        family = "lib_frank",
        margin = margin(r = 8, unit = "pt")
      ),
      
      # Format plot caption text
      plot.caption = element_text(
        family = "lib_frank",
        face = "italic",
        size = 15
      ),
      
      # Format legend text and title
      legend.text = element_text(size = 20, family = "lib_frank"),
      legend.title = element_text(family = "lib_frank", size = 15)
    )
}