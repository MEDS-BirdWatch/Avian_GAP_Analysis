library(tidyverse)
library(dplyr)
library(here)
library(readxl)

# Import PIF
pif <- read_excel(here("data", "ACAD Regional 2024.06.03.xlsx")) %>% 
  clean_names() %>% 
  filter(BCR == c(5, 9, 15, 32, 33))

# Filter to California codes (5, 9, 15, 32, 33)
# BCR = Bird conservation regions
# pif_ca: dataset of regional concern birds (241 distinct species) for California regions
# Columns: 
# 
pif_ca <- pif %>% 
  filter(BCR == c(5, 9, 15, 32, 33))

# Compare Names of indicator species and birds_joined df 
match_test <- semi_join(pif_ca, birds_joined, by = c("Scientific Name" = "scientific_name"))

# Join with indicspecies (might )
focal_species <- potential_indicators %>%
  left_join(pif_ca, by = "Common Name")

# Filter for high indicator values


# 